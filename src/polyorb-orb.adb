------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . O R B                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  The ORB core module.

--  $Id$

with Ada.Exceptions;
with Ada.Tags;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.Local;
with PolyORB.Constants;
with PolyORB.Exceptions;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Objects.Interface;
with PolyORB.ORB.Interface;
with PolyORB.References;
with PolyORB.References.Binding;
with PolyORB.Setup;
with PolyORB.Tasking.Threads;
with PolyORB.Task_Info;
with PolyORB.Transport;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body PolyORB.ORB is

   use PolyORB.Annotations;
   use PolyORB.Asynch_Ev;
   use PolyORB.Binding_Data;
   use PolyORB.Components;
   use PolyORB.Filters;
   use PolyORB.Jobs;
   use PolyORB.Log;
   use PolyORB.References;
   use PolyORB.Requests;
   use PolyORB.Scheduler;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads;
   use PolyORB.Transport;
   use PolyORB.Types;
   use Unsigned_Long_Flags;

   package L is new PolyORB.Log.Facility_Log ("polyorb.orb");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------------------------
   -- Tasking policy generic operations --
   ---------------------------------------

   ---------------------------
   -- Duplicate_Request_Job --
   ---------------------------

   function Duplicate_Request_Job
     (RJ : access Jobs.Job'Class)
     return Jobs.Job_Access
   is
      TRJ : Request_Job renames Request_Job (RJ.all);
      NJ  : constant Job_Access := new Request_Job;
      TNJ : Request_Job renames Request_Job (NJ.all);
   begin
      TNJ.ORB       := TRJ.ORB;
      TNJ.Requestor := TRJ.Requestor;
      TNJ.Request   := TRJ.Request;

      return NJ;
   end Duplicate_Request_Job;

   ----------------------------------------------
   -- Management of asynchronous event sources --
   ----------------------------------------------

   procedure Insert_Source
     (ORB : access ORB_Type;
      AES :        PolyORB.Asynch_Ev.Asynch_Ev_Source_Access);
   --  Insert AES in the set of asynchronous event sources
   --  monitored by ORB. The caller must not hold the ORB lock.

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access);
   --  Delete AES from the set of asynchronous event sources
   --  monitored by ORB. AES is destroyed.
   --  The caller must not hold the ORB lock.

   --------------------------------------------
   -- Annotations used by the ORB internally --
   --------------------------------------------

   type TAP_Note is new Note with record
      Profile_Factory : Binding_Data.Profile_Factory_Access;
      AES             : Asynch_Ev.Asynch_Ev_Source_Access;
   end record;

   type TE_Note is new Note with record
      AES : Asynch_Ev.Asynch_Ev_Source_Access;
   end record;

   ---------------------------
   -- ORB object operations --
   ---------------------------

   ------------
   -- Create --
   ------------

   procedure Create (ORB : in out ORB_Type) is
   begin
      Create (ORB.ORB_Lock);
      --  From now on access to ORB state is protected by this mutex.

      Enter (ORB.ORB_Lock);

      ORB.Job_Queue := PolyORB.Jobs.Create_Queue;
      ORB.Polling  := False;

      ORB.Scheduling_Policy := new Scheduling_Policy;
      Initialize (ORB.Scheduling_Policy, ORB.ORB_Lock);

      Leave (ORB.ORB_Lock);
   end Create;

   ----------------------
   -- Try_Perform_Work --
   ----------------------

   function Try_Perform_Work
     (ORB : access ORB_Type;
      Q   : access Job_Queue)
     return Boolean;
   pragma Inline (Try_Perform_Work);
   --  Perform one item of work from Q, if available.
   --  Precondition: This function must be called from within ORB
   --  critical section.
   --  Postcondition: On exit, we reenter ORB critical section
   --  Note: task running this function may exit ORB critical section
   --  to perform on item.

   function Try_Perform_Work
     (ORB : access ORB_Type;
      Q   : access Job_Queue)
      return Boolean
   is
      Job : constant Job_Access := Fetch_Job (Q);

   begin
      pragma Debug (O ("TPF " & Image (Current_Task) & ": "
                       & "enter Try_Perform_Work"));

      if Job /= null then
         Notify_Event (ORB.Scheduling_Policy, Executing_Job_E);

         Leave (ORB.ORB_Lock);

         pragma Debug (O ("TPF " & Image (Current_Task) & ": "
                          & "working on job "
                          & Ada.Tags.External_Tag (Job'Tag)));
         pragma Assert (Job /= null);

         Run (Job);

         Enter (ORB.ORB_Lock);
         pragma Debug (O ("TPF " & Image (Current_Task) & ": "
                          & "leaving Try_Perform_Work"));
         return True;

      else
         pragma Debug (O ("TPF " & Image (Current_Task) & ": "
                          & "nothing to do."));
         pragma Debug (O ("TPF " & Image (Current_Task) & ": "
                          & "leaving Try_Perform_Work"));

         return False;
      end if;
   end Try_Perform_Work;

   -----------------
   -- Queue_Event --
   -----------------

   procedure Queue_Event
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access);
   pragma Inline (Queue_Event);
   --  Queue an event that occurred on AES for processing.
   --  Precondition: This procedure must be called from within a
   --    critical section.

   procedure Queue_Event
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access)
   is
      Note : AES_Note;

   begin
      pragma Debug (O ("Queue_Event: enter"));

      Get_Note (Notepad_Of (AES).all, Note);
      Queue_Job (ORB.Job_Queue, Job_Access (Note.Handler));
      Notify_Event (ORB.Scheduling_Policy, Job_Queued_E);
   end Queue_Event;

   -----------------------
   -- Try_Check_Sources --
   -----------------------

   procedure Try_Check_Sources
     (ORB       : access ORB_Type;
      This_Task : in out Task_Info.Task_Info);
   --  Check ORB's AES for any incoming event.
   --  Precondition: This function must be called from within ORB
   --  critical section.
   --  Postcondition: On exit, we reenter ORB critical section
   --  Note: tasks running this function may exit ORB critical section.

   procedure Try_Check_Sources
     (ORB       : access ORB_Type;
      This_Task : in out Task_Info.Task_Info)
   is

      use Monitor_Lists;
      use PolyORB.Task_Info;

      It : Monitor_Lists.Iterator := First (ORB.Monitors);

      Poll_Interval : constant Duration := 0.1;
      --  XXX Poll_Interval should be configurable.

      Timeout : Duration;

   begin

      --  ORB.ORB_Lock is held.

      pragma Debug (O ("# of monitors:"
                       & Natural'Image (ORB.Number_Of_Monitors)));

      if ORB.Number_Of_Monitors = 1 then
         Timeout := PolyORB.Constants.Forever;
      else
         Timeout := Poll_Interval;
      end if;

      while not Last (It) loop
         declare
            Monitor : constant Asynch_Ev_Monitor_Access
              := Value (It).all;
         begin
            Set_State_Blocked (This_Task, Monitor);

            ORB.Polling := True;

            pragma Debug (O ("Try_Check_Sources: task "
                             & Image (This_Task)
                             & " about to Check_Sources."));

            Leave (ORB.ORB_Lock);

            declare
               Events : AES_Array := Check_Sources (Monitor, Timeout);
               --  This_Task will block on this action until an
               --  event occurs on one source monitors by Monitor,
               --  or Abort_Check_Sources is called on Monitor.

            begin

               Enter (ORB.ORB_Lock);
               --  Reenter critical sections to update ORB state

               pragma Debug (O ("Try_Check_Sources: task "
                                & Image (This_Task)
                                & " returned from Check_Sources."));

               --  Queue events, if any.

               for J in Events'Range loop
                  Queue_Event (ORB, Events (J));
               end loop;

               ORB.Polling := False;

               Set_State_Blocked (This_Task, null);
               --  Reset the monitor on which 'This_Task' is blocked.

               if Task_Info.Exit_Condition (This_Task)
                 or else Abort_Polling (This_Task) then
                  --  This task has met its exit condition or must
                  --  abort polling. We leave immediately.

                  return;
               end if;

            end;
         end;
         Next (It);

      end loop;

      --  ORB.ORB_Lock is held.

   end Try_Check_Sources;

   -----------------------
   -- The ORB main loop --
   -----------------------

   --  This is the main loop for all general-purpose ORB tasks. This
   --  function MUST NOT be called recursively.  Exceptions may not be
   --  propagated from within a critical section (i.e. with ORB_Lock
   --  held).

   ---------
   -- Run --
   ---------

   procedure Run
     (ORB            : access ORB_Type;
      Exit_Condition :        Exit_Condition_T := (null, null);
      May_Poll       :        Boolean := False)
   is
      use PolyORB.Task_Info;

      Task_Kind_For_Exit_Condition : constant array (Boolean)
        of Task_Kind := (True => Permanent, False => Transient);
      --  The task kind according to whether Exit_Condition
      --  is null (True) or not.

      This_Task : aliased Task_Info.Task_Info
        (Task_Kind_For_Exit_Condition (Exit_Condition.Condition = null));

      New_State : PTI.Task_State;
      Job_Done : Boolean;

   begin
      Enter (ORB.ORB_Lock);

      --  Set up task information for This_Task

      Set_Id (This_Task);
      Set_Exit_Condition (This_Task, Exit_Condition.Condition);
      Set_Polling (This_Task, May_Poll);

      if Exit_Condition.Task_Info /= null then
         Exit_Condition.Task_Info.all := This_Task'Unchecked_Access;
         --  This pointer must be reset to null before exiting Run
         --  so as to not leave a dangling reference.
      end if;

      Register_Task (ORB.Scheduling_Policy, This_Task);

      --  ORB Main loop

      Main_Loop :
      loop
         New_State := Schedule_Task
           (ORB.Scheduling_Policy,
            This_Task'Unchecked_Access);

         case New_State is
            when Running =>
               Set_State_Running (This_Task);
               Job_Done := Try_Perform_Work (ORB, ORB.Job_Queue);
               pragma Assert (Job_Done /= False);
               Notify_Event (ORB.Scheduling_Policy, Job_Completed_E);

            when Blocked =>
               --  This task will block on event sources, waiting for
               --  incoming events.

               Try_Check_Sources (ORB, This_Task);
               Notify_Event (ORB.Scheduling_Policy, End_Of_Check_Sources_E);

            when Idle =>
               --  This task is going idle. We are still holding
               --  ORB_Lock at this point. The tasking policy
               --  will release it while we are idle, and
               --  re-assert it before returning.

               Idle (ORB.Tasking_Policy, This_Task, ORB_Access (ORB));

            when Terminated =>
               exit Main_Loop;

            when Unscheduled =>
               raise Program_Error;
         end case;

         Set_State_Unscheduled (This_Task);

         --  Condition at end of loop: ORB_Lock is held.

      end loop Main_Loop;

      --  Remove reference to This_Task

      if Exit_Condition.Task_Info /= null then
         Exit_Condition.Task_Info.all := null;
      end if;

      Unregister_Task (ORB.Scheduling_Policy, This_Task);

      pragma Debug (O ("Run: leave."));

      Leave (ORB.ORB_Lock);

   exception
      when E : others =>
         --  XXX at this point it is assumed that ORB_Lock is
         --  not being held by this task.

         O ("ORB.Run got exception:", Error);
         O (Ada.Exceptions.Exception_Information (E), Error);

         --  Remove reference to This_Task

         if Exit_Condition.Task_Info /= null then
            Exit_Condition.Task_Info.all := null;
         end if;

         raise;
   end Run;

   ------------------
   -- Work_Pending --
   ------------------

   function Work_Pending (ORB : access ORB_Type) return Boolean
   is
      Result : Boolean;

   begin
      Enter (ORB.ORB_Lock);
      Result := not Is_Empty (ORB.Job_Queue);
      Leave (ORB.ORB_Lock);

      return Result;
   end Work_Pending;

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work (ORB : access ORB_Type)
   is
      One_Job_Done : Boolean;

   begin
      Enter (ORB.ORB_Lock);

      One_Job_Done := Try_Perform_Work (ORB, ORB.Job_Queue);
      pragma Debug (O ("Peform_Work: One Job Done ? : "
                       & Boolean'Image (One_Job_Done)));

      Leave (ORB.ORB_Lock);
   end Perform_Work;

   --------------
   -- Suhtdown --
   --------------

   procedure Shutdown
     (ORB                 : access ORB_Type;
      Wait_For_Completion :        Boolean := True) is
   begin

      pragma Debug (O ("Shutdown: enter"));

      --  Stop accepting incoming connections.
      --  XXX TBD

      --  Wait for completion of all current jobs

      if Wait_For_Completion then
         --  XXX TBD
         raise Not_Implemented;
      end if;

      --  Shutdown the ORB

      Enter (ORB.ORB_Lock);

      Notify_Event (ORB.Scheduling_Policy, ORB_Shutdown_E);

      Leave (ORB.ORB_Lock);

      pragma Debug (O ("Shutdown: leave"));
   end Shutdown;

   ------------------------
   -- Profile_Factory_Of --
   ------------------------

   function Profile_Factory_Of
     (TAP : Transport.Transport_Access_Point_Access)
     return Binding_Data.Profile_Factory_Access;
   pragma Inline (Profile_Factory_Of);

   function Profile_Factory_Of
     (TAP : Transport.Transport_Access_Point_Access)
     return Binding_Data.Profile_Factory_Access
   is
      N : TAP_Note;

   begin
      Get_Note (Notepad_Of (TAP).all, N);
      return N.Profile_Factory;
   end Profile_Factory_Of;

   ---------------------------
   -- Register_Access_Point --
   ---------------------------

   procedure Register_Access_Point
     (ORB   : access ORB_Type;
      TAP   :        PT.Transport_Access_Point_Access;
      Chain :        PF.Factory_Access;
      PF    :        PBD.Profile_Factory_Access)
   is
      New_AES : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TAP.all);
      A_Note  : AES_Note;
      ORB_Acc : constant ORB_Access := ORB_Access (ORB);
   begin
      pragma Debug (O ("Register_Acces_Point: enter"));
      Get_Note (Notepad_Of (New_AES).all, A_Note);
      declare
         Handler : constant AES_Event_Handler_Access
           := A_Note.Handler;
         TAP_Handler : TAP_AES_Event_Handler
           renames TAP_AES_Event_Handler (Handler.all);
      begin
         --  Set link from AES to TAP, Chain and PF.
         Handler.ORB := Component_Access (ORB);
         Handler.AES := New_AES;
         TAP_Handler.TAP := TAP;
         TAP_Handler.Filter_Factory_Chain := Chain;
         TAP_Handler.Profile_Factory := PF;
      end;

      Set_Note (Notepad_Of (TAP).all,
                TAP_Note'(Note with Profile_Factory => PF, AES => New_AES));
      --  Set link from TAP to PF, and from TAP to AES.

      Enter (ORB_Acc.ORB_Lock);
      pragma Debug (O ("Inserting new source: Access Point"));
      TAP_Lists.Append (ORB_Acc.Transport_Access_Points, TAP);
      Leave (ORB_Acc.ORB_Lock);

      Insert_Source (ORB_Acc, New_AES);

      pragma Debug (O ("Register_Acces_Point: leave"));
   end Register_Access_Point;

   ----------------------
   -- Is_Profile_Local --
   ----------------------

   function Is_Profile_Local
     (ORB : access ORB_Type;
      P   : access Binding_Data.Profile_Type'Class)
     return Boolean is
   begin
      if P.all in Binding_Data.Local.Local_Profile_Type then
         return True;
      end if;

      Enter (ORB.ORB_Lock);
      declare
         use TAP_Lists;
         It : Iterator := First (ORB.Transport_Access_Points);
      begin
         All_Access_Points :
         while not Last (It) loop
            exit All_Access_Points
            when Binding_Data.Is_Local_Profile
              (Profile_Factory_Of (Value (It).all), P);
            Next (It);
         end loop All_Access_Points;
         Leave (ORB.ORB_Lock);

         return not Last (It);
      end;
   end Is_Profile_Local;

   -----------------------
   -- Register_Endpoint --
   -----------------------

   procedure Register_Endpoint
     (ORB          : access ORB_Type;
      TE           :        PT.Transport_Endpoint_Access;
      Filter_Stack :        PF.Filter_Access;
      Role         :        Endpoint_Role)
   is
      New_AES    : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TE.all);

      A_Note     : AES_Note;
      ORB_Acc    : constant ORB_Access := ORB_Access (ORB);
   begin
      pragma Debug (O ("Register_Endpoint: enter"));

      Connect_Upper (TE, Component_Access (Filter_Stack));
      Connect_Lower (Filter_Stack, Component_Access (TE));
      --  Connect filter to transport.

      Emit_No_Reply
        (Component_Access (TE),
         Filters.Interface.Set_Server'
         (Server => Component_Access (ORB)));

      --  Notes.AES is null for write only Endpoint
      if New_AES /= null then
         Get_Note (Notepad_Of (New_AES).all, A_Note);
         declare
            Handler : constant AES_Event_Handler_Access
              := A_Note.Handler;
            TE_Handler : TE_AES_Event_Handler
              renames TE_AES_Event_Handler (Handler.all);
         begin
            Handler.ORB := Component_Access (ORB);
            Handler.AES := New_AES;

            TE_Handler.TE := TE;
         end;
      end if;

      --  Register link from AES to TE.

      Set_Note
        (Notepad_Of (TE).all,
         TE_Note'(Annotations.Note with AES => New_AES));
      --  Register link from TE to AES.

      --  Assign execution resources to the newly-created connection.

      case Role is
         when Server =>
            Handle_New_Server_Connection
              (ORB_Acc.Tasking_Policy,
               ORB_Acc,
               Active_Connection'(AES => New_AES, TE => TE));

         when Client =>
            Handle_New_Client_Connection
              (ORB_Acc.Tasking_Policy,
               ORB_Acc,
               Active_Connection'(AES => New_AES, TE => TE));
      end case;

      pragma Debug (O ("Register_Endpoint: leave"));
   end Register_Endpoint;

   ------------------------
   -- Set_Object_Adapter --
   ------------------------

   procedure Set_Object_Adapter
     (ORB : access ORB_Type;
      OA  :        Obj_Adapters.Obj_Adapter_Access)
   is
      use type Obj_Adapters.Obj_Adapter_Access;

   begin
      pragma Assert (ORB.Obj_Adapter = null);
      ORB.Obj_Adapter := OA;
   end Set_Object_Adapter;

   --------------------
   -- Object_Adapter --
   --------------------

   function Object_Adapter (ORB : access ORB_Type)
     return Obj_Adapters.Obj_Adapter_Access is
   begin
      return ORB.Obj_Adapter;
   end Object_Adapter;

   -------------------
   -- Insert_Source --
   -------------------

   procedure Insert_Source
     (ORB : access ORB_Type;
      AES :        Asynch_Ev_Source_Access) is
   begin
      Enter (ORB.ORB_Lock);

      pragma Debug (O ("Insert_Source: enter"));
      pragma Assert (AES /= null);

      --  Disable polling to enable safe modification of AES list
      Disable_Polling (ORB.Scheduling_Policy);

      --  At this stage, no task shall concurrently run Check_Sources.
      pragma Assert (not ORB.Polling);

      --  Register source to a monitor.
      declare
         use Monitor_Lists;
         It : Iterator := First (ORB.Monitors);
         Success : Boolean := False;
      begin

         while not Last (It) loop
            Register_Source (Value (It).all, AES, Success);
            exit when Success;
            Next (It);
         end loop;

         if not Success then
            declare
               New_AEM : constant Asynch_Ev_Monitor_Access
                 := AEM_Factory_Of (AES.all).all;
            begin
               Create (New_AEM.all);
               Append (ORB.Monitors, New_AEM);
               ORB.Number_Of_Monitors := Length (ORB.Monitors);
               Register_Source (New_AEM, AES, Success);
               pragma Assert (Success);
            end;
         end if;
      end;

      Notify_Event (ORB.Scheduling_Policy, Event_Sources_Added_E);

      --  Modification completed, enable polling
      Enable_Polling (ORB.Scheduling_Policy);

      pragma Debug (O ("Insert source: leave"));

      Leave (ORB.ORB_Lock);

   end Insert_Source;

   -------------------
   -- Delete_Source --
   -------------------

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access) is
   begin
      --  XXX should contemplate suppressing monitor from ORB.Monitors
      --  if neccessary .. where is it done?

      Enter (ORB.ORB_Lock);

      pragma Debug (O ("Delete_Source: enter"));

      --  Disable polling to enable safe modification of AES list
      Disable_Polling (ORB.Scheduling_Policy);

      --  At this stage, no task shall concurrently run Check_Sources.
      pragma Assert (not ORB.Polling);

      --  Remove source
      Unregister_Source (AES);
      Notify_Event (ORB.Scheduling_Policy, Event_Sources_Deleted_E);

      --  Modification completed, enable polling
      Enable_Polling (ORB.Scheduling_Policy);

      Leave (ORB.ORB_Lock);

      --  Actually destroy AES
      Destroy (AES);

      pragma Debug (O ("Delete_Source: leave"));
   end Delete_Source;

   ----------------------------------
   -- Job type for object requests --
   ----------------------------------

   ---------
   -- Run --
   ---------

   procedure Run (J : access Request_Job)
   is
      AJ : Job_Access := Job_Access (J);

   begin
      Handle_Request_Execution
        (P => J.ORB.Tasking_Policy, ORB => J.ORB, RJ => J);
      Free (AJ);

   exception
      when E : others =>
         pragma Debug (O ("Run: Got exception "
                          & Ada.Exceptions.Exception_Information (E)));

         Free (AJ);
         raise;
   end Run;

   -----------------
   -- Run_Request --
   -----------------

   procedure Run_Request (J : access Request_Job) is
   begin
      pragma Debug (O ("Run Request_Job: enter"));
      pragma Assert (J.Request /= null);

      declare
         use type Task_Info.Task_Info_Access;
         Surrogate : Components.Component_Access;
         Pro : PolyORB.Binding_Data.Profile_Access;
      begin
         pragma Debug (O ("Task " & Image (Current_Task)
                            & " executing: "
                            & Requests.Image (J.Request.all)));
         if J.Request.Requesting_Task /= null then
            pragma Debug
              (O ("... requested by "
                  & Task_Info.Image (J.Request.Requesting_Task.all)));
            null;
         end if;

         declare
            use PolyORB.Exceptions;

            Error : Error_Container;

         begin
            References.Binding.Bind
              (J.Request.Target, J.ORB, Surrogate, Pro, False, Error);

            if Found (Error) then
               pragma Debug (O ("Run_Request: Got an error when binding: "
                                & Error_Id'Image (Error.Kind)));

               --  Any error caught at this level implies a
               --  problem within the object adapter. We bounce the
               --  exception to the user for further processing.

               J.Request.Exception_Info
                 := PolyORB.Exceptions.Error_To_Any (Error);

               Catch (Error);

               Emit_No_Reply (J.Requestor,
                              Objects.Interface.Executed_Request'
                              (Req => J.Request));
               return;
            end if;
         end;

         --  XXX May be a point to synchronize on With_Server ...
         --  At this point, the server has been contacted, a binding
         --  has been created, a servant manager has been reached,
         --  we are _before_ invoking the request to the target
         --  if we are on the _server_ side, we can send an
         --  'Executed_Request' Message

         --  ==> does this comply with CORBA 22.2.2.1 definition
         --      of SYNC_WITH_SERVER ?

         declare
            Profiles : constant Profile_Array :=
              Profiles_Of (J.Request.Target);
         begin
            if Is_Set (Sync_With_Server, J.Request.Req_Flags) and then
              Get_Profile_Tag (Profiles (Profiles'First).all) = Tag_Local
            then
               pragma Debug (O ("With_Server completed, sending incomplete"
                             & " Executed_Request message"));

               Emit_No_Reply (J.Requestor,
                              Objects.Interface.Executed_Request'
                              (Req => J.Request));
            end if;
         end;

         --  Setup_Environment (Oid);
         --  XXX for 'Current'

         declare
            Result : constant Components.Message'Class
              := Emit (Surrogate,
                       Objects.Interface.Execute_Request'
                       (Req => J.Request,
                        Pro => Pro));
         begin
            --  Unsetup_Environment ();
            --  Unbind (J.Req.Target, J.ORB, Servant);
            --  XXX Unbind must Release_Servant.

            --  XXX Actually cannot unbind here: if the binding
            --    object is destroyed that early, we won't
            --    have the opportunity to receive a reply...
            pragma Debug
              (O ("Run_Request: got " & Ada.Tags.External_Tag (Result'Tag)));

            if Result not in Null_Message then
               --  An answer was synchronously provided by the
               --  servant: send it back to the requesting party
               --  iff it is required.

               if Is_Set (Sync_With_Target, J.Request.Req_Flags)
                 or else Is_Set (Sync_Call_Back, J.Request.Req_Flags)
               then
                  Emit_No_Reply (J.Requestor, Result);
               end if;
            end if;

            --  XXX Should that be Emit? Should there be a reply
            --      from Requestor?
            --  XXX Who frees the Request object?

         end;
         pragma Debug (O ("Run_Request: executed request"));
      end;

   end Run_Request;

   ----------------------
   -- Create_Reference --
   ----------------------

   procedure Create_Reference
     (ORB : access ORB_Type;
      Oid : access Objects.Object_Id;
      Typ : in     String;
      Ref :    out References.Ref) is
   begin
      Enter (ORB.ORB_Lock);
      declare
         use PolyORB.Binding_Data;
         use PolyORB.Binding_Data.Local;
         use TAP_Lists;

         It : Iterator := First (ORB.Transport_Access_Points);

         Profiles : References.Profile_Array
           (1 .. Length (ORB.Transport_Access_Points) + 1);
         Last_Profile : Integer := Profiles'First - 1;
      begin
         while not Last (It) loop
            declare
               PF : constant Profile_Factory_Access
                 := Profile_Factory_Of (Value (It).all);
            begin
               if PF /= null then
                  --  Null profile factories may occur for access points
                  --  that have an ad hoc protocol stack, but no binding
                  --  data information.
                  declare
                     P : constant Profile_Access
                       := Create_Profile (PF, Oid.all);
                  begin
                     if P /= null then
                        Last_Profile := Last_Profile + 1;
                        Profiles (Last_Profile) := P;
                     end if;
                  end;
               end if;
            end;
            Next (It);
         end loop;

         --  Add a local profile

         Last_Profile := Last_Profile + 1;
         Profiles (Last_Profile) := new Local_Profile_Type;
         Create_Local_Profile
           (Oid.all,
            Local_Profile_Type (Profiles (Last_Profile).all));

         Leave (ORB.ORB_Lock);

         References.Create_Reference
           (Profiles (Profiles'First .. Last_Profile), Typ, Ref);
      end;
   end Create_Reference;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (ORB : access ORB_Type;
      Msg :        PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Objects.Interface;

      Result : Components.Null_Message;
   begin
      pragma Debug (O ("Handling message of type "
                       & Ada.Tags.External_Tag (Msg'Tag)));

      if Msg in Interface.Queue_Job then
         Enter (ORB.ORB_Lock);

         Queue_Job (ORB.Job_Queue,
                    Interface.Queue_Job (Msg).Job);

         Notify_Event (ORB.Scheduling_Policy, Job_Queued_E);

         Leave (ORB.ORB_Lock);

      elsif Msg in Interface.Queue_Request then
         declare
            QR : Interface.Queue_Request
              renames Interface.Queue_Request (Msg);
            Req : Requests.Request_Access renames QR.Request;

            QJ : constant Interface.Queue_Job :=
              (Job => new Request_Job);
            J  : Job_Access renames QJ.Job;
         begin
            pragma Debug (O ("Queue_Request: enter"));

            Request_Job (J.all).ORB       := ORB_Access (ORB);
            Request_Job (J.all).Request   := Req;

            if QR.Requestor = null then

               --  If the request was queued directly by a client,
               --  then the ORB is responsible for setting its
               --  state to completed on reply from the
               --  object.

               Request_Job (J.all).Requestor := Component_Access (ORB);

            else

               Request_Job (J.all).Requestor := QR.Requestor;

            end if;

            Req.Requesting_Component := Request_Job (J.all).Requestor;

            pragma Debug (O ("Queue_Request: leave"));
            return Handle_Message (ORB, QJ);
         end;

      elsif Msg in Executed_Request then

         declare
            use PolyORB.Task_Info;

            Req : Requests.Request
              renames Executed_Request (Msg).Req.all;
         begin

            --  The processing of Executed_Request must be done
            --  in the ORB critical section, because it must not
            --  take place between the time an ORB task checks its
            --  exit condition and the moment the task goes idle.

            Enter (ORB.ORB_Lock);

            Req.Completed := True;

            --  XXX The correctness of the following is not
            --  completely determined.
            --  Is this mutitask-safe????

            --  As of 20021122, the answer is NO.
            --  Run evoluted DSA tests with -n 2 -c 100 -s 1
            --  and Thead_Pool server.

            pragma Debug (O ("Request completed."));
            if Req.Requesting_Task /= null then

               --  Notify the task
               pragma Debug
                 (O ("... requesting task is "
                     & Task_State'Image
                     (State (Req.Requesting_Task.all))));

               Notify_Event (ORB.Scheduling_Policy,
                             Event'(Kind => Request_Result_Ready,
                                    TI   => Req.Requesting_Task));

            else
               --  The requesting task has already taken note of
               --  the completion of the request: nothing to do.
               null;
            end if;
            Leave (ORB.ORB_Lock);
         end;

      elsif Msg in Interface.Oid_Translate then
         declare
            Result : constant Interface.URI_Translate
              := (Path => Obj_Adapters.Oid_To_Rel_URI
                  (ORB.Obj_Adapter, Interface.Oid_Translate (Msg).Oid));
         begin
            return Result;
         end;

      elsif Msg in Interface.URI_Translate then
         declare
            Result : constant Interface.Oid_Translate
              := (Oid => Obj_Adapters.Rel_URI_To_Oid
                  (ORB.Obj_Adapter, Interface.URI_Translate (Msg).Path));
         begin
            return Result;
         end;

      elsif Msg in Interface.Monitor_Endpoint then
         declare
            TE : constant Transport_Endpoint_Access
              := Interface.Monitor_Endpoint (Msg).TE;
            Note : TE_Note;
         begin
            Get_Note (Notepad_Of (TE).all, Note);

            --  Notes.AES is null for write only Endpoint

            if Note.AES /= null then
               pragma Debug (O ("Inserting source: Monitored Endpoint"));
               Insert_Source (ORB, Note.AES);
            end if;
         end;

      elsif Msg in Interface.Monitor_Access_Point then
         declare
            TAP : constant Transport_Access_Point_Access
              := Interface.Monitor_Access_Point (Msg).TAP;
            Note : TAP_Note;
         begin
            Get_Note (Notepad_Of (TAP).all, Note);

            pragma Debug (O ("Inserting source: Monitored Endpoint"));
            Insert_Source (ORB, Note.AES);

         end;

      elsif Msg in Interface.Unregister_Endpoint then
         declare
            TE : Transport_Endpoint_Access
              := Interface.Unregister_Endpoint (Msg).TE;
            AES : Asynch_Ev_Source_Access;
            Note : TE_Note;
         begin
            Get_Note (Notepad_Of (TE).all, Note);
            AES := Note.AES;

            if AES /= null then
               Delete_Source (ORB, AES);
            end if;
            Destroy (TE);
         end;

      else
         pragma Debug (O ("ORB received unhandled message of type "
                          & Ada.Tags.External_Tag (Msg'Tag)));
         raise Components.Unhandled_Message;
      end if;

      return Result;
   end Handle_Message;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Setup.The_ORB := new ORB_Type (Setup.The_Tasking_Policy);
      Create (Setup.The_ORB.all);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb",
       Conflicts => Empty,
       Depends   => +"orb.tasking_policy"
       & "binding_data.soap?"
       & "binding_data.srp?"
       & "binding_data.iiop?"
       & "protocols.srp?"
       & "protocols.giop?"
       & "protocols.soap?"
       & "scheduler"
       & "smart_pointers"
       & "exceptions.stack"
       & "tasking.threads"
       & "tasking.mutexes"
       & "tasking.condition_variables",
       Provides => Empty,
       Init     => Initialize'Access));

end PolyORB.ORB;
