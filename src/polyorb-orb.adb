------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . O R B                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
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
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads;
   use PolyORB.Transport;
   use PolyORB.Types;

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
      NJ : constant Job_Access := new Request_Job;
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
      AES : Asynch_Ev_Source_Access);
   --  Insert AES in the set of asynchronous event sources
   --  monitored by ORB. The caller must hold the ORB lock.

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access);
   --  Delete AES from the set of asynchronous event sources
   --  monitored by ORB. AES is destroyed.

   procedure Run
     (AEH : access AES_Event_Handler) is
   begin
      Handle_Event
        (AES_Event_Handler'Class (AEH.all)'Access,
         AEH.ORB, AEH.AES);
      --  Redispatch.

      if AEH.AES = null then
         declare
            V_AEH : Job_Access := Job_Access (AEH);
         begin
            Free (V_AEH);
         end;
      end if;
   end Run;

   --------------------------------------
   -- Spec of 'Event_Handlers' package --
   --------------------------------------

   package Event_Handlers is

      --  Handler for AES associated with a Transport Access Point

      type TAP_AES_Event_Handler is new AES_Event_Handler with record
         TAP : Transport_Access_Point_Access;
         --  Factory of Transport_Endpoint components.

         Filter_Factory_Chain : Filters.Factory_Access;
         --  Factory of Filter (protocol stack) components.

         Profile_Factory : Binding_Data.Profile_Factory_Access;
         --  Factory of profiles capable of associating the
         --  address of TAP and the specification of the
         --  protocol implemented by Filter_Factory_Chain
         --  with an object id.
      end record;

      procedure Handle_Event
        (H   : access TAP_AES_Event_Handler;
         ORB :        ORB_Access;
         AES : in out Asynch_Ev_Source_Access);

      --  Handler for AES associated with a Transport Endpoint

      type TE_AES_Event_Handler is new AES_Event_Handler with record
         TE : Transport_Endpoint_Access;
         --  Transport_Endpoint component (connected to a
         --  protocol stack).
      end record;

      procedure Handle_Event
        (H   : access TE_AES_Event_Handler;
         ORB :        ORB_Access;
         AES : in out Asynch_Ev_Source_Access);
   end Event_Handlers;

   --------------------------------------------
   -- Annotations used by the ORB internally --
   --------------------------------------------

   type TAP_Note is new Note with record
      Profile_Factory : Binding_Data.Profile_Factory_Access;
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

      Create (ORB.Idle_Tasks);
      ORB.Job_Queue := PolyORB.Jobs.Create_Queue;
      ORB.Shutdown := False;
      ORB.Polling  := False;
      Create (ORB.Polling_Completed);
      ORB.Idle_Counter := 0;

      Leave (ORB.ORB_Lock);
   end Create;

   ----------------------
   -- Try_Perform_Work --
   ----------------------

   function Try_Perform_Work
     (ORB : access ORB_Type;
      Q   : access Job_Queue)
     return Boolean;
   --  Perform one item of work from Q, if available.
   --  Precondition: This function must be called from within a
   --    critical section.
   --  Postcondition: if a job has been executed, then the critical
   --    section has been left, and True is returned.
   --    If no job was available, the critical section is not left,
   --    and False is returned.

   function Try_Perform_Work
     (ORB : access ORB_Type;
      Q   : access Job_Queue)
      return Boolean
   is
      Prefix : constant String
        := "TPF " & Image (Current_Task) & ": ";
      Job : constant Job_Access := Fetch_Job (Q);
   begin
      pragma Debug (O (Prefix & "enter"));

      if Job /= null then
         Leave (ORB.ORB_Lock);

         pragma Debug (O (Prefix & "working"));
         pragma Assert (Job /= null);

         Run (Job);

         return True;
      else
         pragma Debug (O (Prefix & "nothing to do."));

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

   procedure Queue_Event
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access)
   is
      Note : AES_Note;
   begin

      pragma Debug (O ("Queue_Event: enter"));

      Get_Note (Notepad_Of (AES).all, Note);
      Queue_Job (ORB.Job_Queue, Job_Access (Note.Handler));
   end Queue_Event;

   --------------------------------------
   -- Body of 'Event_Handlers' package --
   --------------------------------------

   package body Event_Handlers is

      ------------------
      -- Handle_Event --
      ------------------

      procedure Handle_Event
        (H   : access TAP_AES_Event_Handler;
         ORB :        ORB_Access;
         AES : in out Asynch_Ev_Source_Access) is
      begin
         pragma Debug (O ("Handle_Event: TAP AES"));

         declare
            New_TE     : Transport_Endpoint_Access;
            New_Filter : Filter_Access;
         begin
            Accept_Connection (H.TAP.all, New_TE);
            --  Create transport endpoint.

            New_Filter := Create_Filter_Chain
              (H.Filter_Factory_Chain);

            Register_Endpoint (ORB, New_TE, New_Filter, Server);

            Enter (ORB.ORB_Lock);
            Insert_Source (ORB, AES);
            --  Continue monitoring the TAP's AES.

            Leave (ORB.ORB_Lock);
         end;
      end Handle_Event;

      ------------------
      -- Handle_Event --
      ------------------

      procedure Handle_Event
        (H   : access TE_AES_Event_Handler;
         ORB :        ORB_Access;
         AES : in out Asynch_Ev_Source_Access) is
      begin
         pragma Debug (O ("Handle_Event: TE AES"));

         begin
            Emit_No_Reply
              (Component_Access (H.TE),
               Filters.Interface.Data_Indication'
                 (Data_Amount => 0));
            --  The size of the data received is not known yet.

         exception
            when Connection_Closed =>
               O ("Connection closed.");

               --  Close has been called on the transport endpoint.
               --  Both the Endpoint and the associated AES must
               --  now be destroyed.
               Handle_Close_Server_Connection
                 (ORB.Tasking_Policy, H.TE);

               Destroy (H.TE);
               --  Destroy the transport endpoint and the associated
               --  protocol stack.

               Destroy (AES);
               --  No need to Unregister_Source, because the AES
               --  is already unregistered while an event is being
               --  processed.

            when E : others =>
               O ("Got exception while sending Data_Indication:", Error);
               O (Ada.Exceptions.Exception_Information (E), Error);
               Close (H.TE.all);

               Destroy (H.TE);
               Destroy (AES);
         end;

      end Handle_Event;

   end Event_Handlers;

   -----------------------
   -- The ORB main loop --
   -----------------------

   --  This is the main loop for all general-purpose
   --  ORB tasks. This function MUST NOT be called recursively.
   --  Exceptions may not be propagated from within a critical
   --  section (i.e. with ORB_Lock held).

   procedure Run
     (ORB            : access ORB_Type;
      Exit_Condition : Exit_Condition_T := (null, null);
      May_Poll       : Boolean := False)
   is
      use Task_Info;

      Task_Kind_For_Exit_Condition : constant array (Boolean)
        of Task_Kind := (True => Permanent, False => Transient);
      --  The task kind according to whether Exit_Condition
      --  is null (True) or not.

      This_Task : aliased Task_Info.Task_Info
        (Task_Kind_For_Exit_Condition
         (Exit_Condition.Condition = null));

      --------------
      -- Exit_Now --
      --------------

      function Exit_Now return Boolean;
      pragma Inline (Exit_Now);
      --  True if this instance of the ORB main loop must be terminated.

      function Exit_Now return Boolean is
      begin
         return (Exit_Condition.Condition /= null
                 and then Exit_Condition.Condition.all)
           or else ORB.Shutdown;
      end Exit_Now;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup;
      pragma Inline (Cleanup);
      --  Remove all references to This_Task. Must be called
      --  before returning from Run (even when not debugging).

      procedure Cleanup is
      begin
         if Exit_Condition.Task_Info /= null then
            Exit_Condition.Task_Info.all := null;
         end if;
      end Cleanup;

   begin
      Enter (ORB.ORB_Lock);
      Set_Id (This_Task);
      if Exit_Condition.Task_Info /= null then
         Exit_Condition.Task_Info.all
           := This_Task'Unchecked_Access;
      end if;
      --  This pointer must be reset to null before exiting Run
      --  so as to not leave a dangling reference.

      Main_Loop :
      loop
         pragma Debug (O ("Run: task " & Image (Current_Task)
                          & " entering main loop."));

         Check_Condition :
         loop
            exit Main_Loop when Exit_Now;

            exit Check_Condition
            when not Try_Perform_Work (ORB, ORB.Job_Queue);

            Enter (ORB.ORB_Lock);
            --  Try_Perform_Work has released ORB_Lock and
            --  executed a job from the queue. Reassert ORB_Lock.
         end loop Check_Condition;

         --  ORB_Lock is held.

         if May_Poll
           and then not ORB.Polling
           and then Monitor_Lists.Length (ORB.Monitors) > 0
         then
            declare
               use Monitor_Lists;

               It : Monitor_Lists.Iterator := First (ORB.Monitors);
               Len : constant Integer := Length (ORB.Monitors);

               Poll_Interval : constant Duration := 0.1;
               --  XXX Poll_Interval should be configurable.

               Timeout : Duration;

            begin
               pragma Debug (O ("# of monitors:" & Integer'Image (Len)));

               if Len = 1 then
                  Timeout := PolyORB.Constants.Forever;
               else
                  Timeout := Poll_Interval;
               end if;

               --  ORB.ORB_Lock is held.

               while not Last (It) loop
                  declare
                     Monitor : constant Asynch_Ev_Monitor_Access
                       := Value (It).all;
                  begin
                     Set_Status_Blocked (This_Task, Monitor);

                     <<Retry>>
                     ORB.Selector := Monitor;
                     ORB.Polling := True;
                     ORB.Source_Deleted := False;
                     Leave (ORB.ORB_Lock);

                     pragma Debug (O ("Run: task " & Image (Current_Task)
                                      & " about to Check_Sources."));
                     declare
                        Events : AES_Array
                          := Check_Sources (Monitor, Timeout);
                     begin
                        pragma Debug
                          (O ("Run: task " & Image (Current_Task)
                              & " returned from Check_Sources."));
                        Enter (ORB.ORB_Lock);
                        Broadcast (ORB.Polling_Completed);

                        ORB.Polling := False;
                        ORB.Selector := null;

                        exit Main_Loop when Exit_Now;

                        if ORB.Source_Deleted then
                           --  An asynchronous event source was
                           --  unregistered while we were blocking,
                           --  and may now have been destroyed.
                           goto Retry;
                        end if;

                        for J in Events'Range loop
                           Queue_Event (ORB, Events (J));
                        end loop;

                        Set_Status_Running (This_Task);

                     end;
                  end;
                  Next (It);
               end loop;

               --  ORB.ORB_Lock is held.
            end;

         else

            --  This task is going idle. We are still holding
            --  ORB_Lock at this point. The tasking policy
            --  will release it while we are idle, and
            --  re-assert it before returning.

            Set_Status_Idle (This_Task, ORB.Idle_Tasks);
            Idle (ORB.Tasking_Policy, ORB_Access (ORB));
            Set_Status_Running (This_Task);

         end if;

         --  Condition at end of loop: ORB_Lock is held.

      end loop Main_Loop;

      pragma Debug (O ("Run: leave."));
      Cleanup;

      if not ORB.Polling then
         Signal (ORB.Idle_Tasks);
         --  Wake up idle tasks (if any) so that one of them
         --  checks asynchronous event sources.
      end if;

      Leave (ORB.ORB_Lock);

   exception
      when E : others =>
         --  XXX at this point it is assumed that ORB_Lock is
         --  not being held by this task.

         O ("ORB.Run got exception:", Error);
         O (Ada.Exceptions.Exception_Information (E), Error);

         Cleanup;
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

   procedure Perform_Work (ORB : access ORB_Type) is
   begin
      Enter (ORB.ORB_Lock);
      if not Try_Perform_Work (ORB, ORB.Job_Queue) then
         Leave (ORB.ORB_Lock);
      end if;
   end Perform_Work;

   --------------
   -- Suhtdown --
   --------------

   procedure Shutdown
     (ORB                 : access ORB_Type;
      Wait_For_Completion : Boolean := True) is
   begin

      --  Stop accepting incoming connections.
      --  XX TBD

      if Wait_For_Completion then
         --  XX TBD
         raise Not_Implemented;
      end if;

      Enter (ORB.ORB_Lock);
      ORB.Shutdown := True;
      Leave (ORB.ORB_Lock);

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
      TAP   : Transport_Access_Point_Access;
      Chain : Filters.Factory_Access;
      PF    : Binding_Data.Profile_Factory_Access)
   is
      Handler : constant AES_Event_Handler_Access
        := new Event_Handlers.TAP_AES_Event_Handler;
      TAP_Handler : Event_Handlers.TAP_AES_Event_Handler
        renames Event_Handlers.TAP_AES_Event_Handler (Handler.all);

      New_AES : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TAP.all);
   begin
      Handler.ORB := ORB_Access (ORB);
      Handler.AES := New_AES;
      TAP_Handler.TAP := TAP;
      TAP_Handler.Filter_Factory_Chain := Chain;
      TAP_Handler.Profile_Factory := PF;

      Set_Note (Notepad_Of (New_AES).all,
                AES_Note'(Annotations.Note with Handler => Handler));
      --  Set link from AES to TAP, Chain and PF.

      Set_Note (Notepad_Of (TAP).all,
                TAP_Note'(Note with Profile_Factory => PF));
      --  Set link from TAP to PF.

      Enter (ORB.ORB_Lock);
      TAP_Lists.Append (ORB.Transport_Access_Points, TAP);
      Insert_Source (ORB, New_AES);
      Leave (ORB.ORB_Lock);
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
      TE           :        Transport_Endpoint_Access;
      Filter_Stack :        Filters.Filter_Access;
      Role         :        Endpoint_Role)
   is
      Handler : constant AES_Event_Handler_Access
        := new Event_Handlers.TE_AES_Event_Handler;
      TE_Handler : Event_Handlers.TE_AES_Event_Handler
        renames Event_Handlers.TE_AES_Event_Handler (Handler.all);
      New_AES    : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TE.all);
   begin
      Connect_Upper (TE, Component_Access (Filter_Stack));
      Connect_Lower (Filter_Stack, Component_Access (TE));
      --  Connect filter to transport.

      Emit_No_Reply
        (Component_Access (TE),
         Filters.Interface.Set_Server'
         (Server => Component_Access (ORB)));

      Handler.ORB := ORB_Access (ORB);
      Handler.AES := New_AES;

      TE_Handler.TE := TE;
      Set_Note
        (Notepad_Of (New_AES).all,
         AES_Note'(Annotations.Note with Handler => Handler));
      --  Register link from AES to TE.

      Set_Note
        (Notepad_Of (TE).all,
         TE_Note'(Annotations.Note with AES => New_AES));
      --  Register link from TE to AES.

      --  Assign execution resources to the newly-created connection.

      case Role is
         when Server =>
            Handle_New_Server_Connection
              (ORB.Tasking_Policy,
               ORB_Access (ORB),
               Active_Connection'(AES => New_AES, TE => TE));
         when Client =>
            Handle_New_Client_Connection
              (ORB.Tasking_Policy,
               ORB_Access (ORB),
               Active_Connection'(AES => New_AES, TE => TE));
      end case;

   end Register_Endpoint;

   ------------------------
   -- Set_Object_Adapter --
   ------------------------

   procedure Set_Object_Adapter
     (ORB : access ORB_Type;
      OA  : Obj_Adapters.Obj_Adapter_Access)
   is
      use Obj_Adapters;
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
      AES : Asynch_Ev_Source_Access)
   is
   begin
      pragma Assert (AES /= null);

      declare
         use Monitor_Lists;
         It : Iterator := First (ORB.Monitors);
         Success : Boolean;
      begin
         Success := False;
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
               Register_Source (New_AEM, AES, Success);
               pragma Assert (Success);
            end;
         end if;
      end;

      if ORB.Polling then
         pragma Assert (ORB.Selector /= null);
         pragma Debug (O ("Waking up polling task."));
         Abort_Check_Sources (ORB.Selector.all);
      else
         Signal (ORB.Idle_Tasks);
      end if;
   end Insert_Source;

   -------------------
   -- Delete_Source --
   -------------------

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access)
   is
   begin
      Enter (ORB.ORB_Lock);

      Unregister_Source (AES);

      if ORB.Polling then
         ORB.Source_Deleted := True;
         pragma Assert (ORB.Selector /= null);
         Abort_Check_Sources (ORB.Selector.all);
         Wait (ORB.Polling_Completed, ORB.ORB_Lock);
      end if;

      Leave (ORB.ORB_Lock);
      Destroy (AES);
   end Delete_Source;

   ----------------------------------
   -- Job type for object requests --
   ----------------------------------

   ---------
   -- Run --
   ---------

   procedure Run (J : access Request_Job) is
      AJ : Job_Access := Job_Access (J);
   begin
      Handle_Request_Execution
        (P => J.ORB.Tasking_Policy, ORB => J.ORB, RJ => J);
      Free (AJ);
   exception
      when others =>
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

         begin
            References.Binding.Bind
              (J.Request.Target, J.ORB, Surrogate, Pro);

         exception
            when E : others =>
               pragma Debug (O ("Run_Request: Got an exception when binding"
                                & Ada.Exceptions.Exception_Information (E)));

               --  Any exception caught at this level implies a
               --  problem within the object adapter. We bounce the
               --  exception to the user for further processing.

               J.Request.Exception_Info
                 := PolyORB.Exceptions.System_Exception_To_Any (E);

               Emit_No_Reply (J.Requestor,
                              Objects.Interface.Executed_Request'
                              (Req => J.Request));
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
            Result : constant Components.Message'class
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
              (O ("Run: got " & Ada.Tags.External_Tag (Result'Tag)));

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

   exception
      when E : others =>
         pragma Debug (O ("Run_Request: Got exception "
                          & Ada.Exceptions.Exception_Information (E)));
         raise;

   end Run_Request;

   ----------------------
   -- Create_Reference --
   ----------------------

   procedure Create_Reference
     (ORB : access ORB_Type;
      Oid : access Objects.Object_Id;
      Typ : in String;
      Ref : out References.Ref) is
   begin
      Enter (ORB.ORB_Lock);
      declare
         use PolyORB.Binding_Data;
         use TAP_Lists;

         It : Iterator := First (ORB.Transport_Access_Points);

         Profiles : References.Profile_Array
           (1 .. Length (ORB.Transport_Access_Points));
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
                  Last_Profile := Last_Profile + 1;
                  Profiles (Last_Profile) := Create_Profile (PF, Oid.all);
                  pragma Assert (Profiles (Last_Profile) /= null);
               end if;
            end;
            Next (It);
         end loop;
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
      Msg : PolyORB.Components.Message'Class)
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

         --  Ensure that one ORB task will process this job.

         if ORB.Idle_Counter /= 0 then
            Signal (ORB.Idle_Tasks);
         elsif ORB.Polling then
            pragma Assert (ORB.Selector /= null);
            Abort_Check_Sources (ORB.Selector.all);
         else
            null;
            --  No task is blocked: assume that one will
            --  eventually loop in ORB.Run and process this job.
         end if;

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
               Request_Job (J.all).Requestor
                 := Component_Access (ORB);
            else
               Request_Job (J.all).Requestor := QR.Requestor;
            end if;
            Req.Requesting_Component := Request_Job (J.all).Requestor;
            return Handle_Message (ORB, QJ);
            pragma Debug (O ("Queue_Request: leave"));
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
               pragma Debug
                 (O ("... requesting task is "
                     & Task_Status'Image
                     (Status (Req.Requesting_Task.all))));

               case Status (Req.Requesting_Task.all) is
                  when Running =>
                     null;
                  when Blocked =>

                     declare
                        use Asynch_Ev;

                        Sel : constant Asynch_Ev_Monitor_Access
                          := Selector (Req.Requesting_Task.all);
                     begin
                        pragma Debug (O ("About to abort block"));
                        pragma Assert (Sel /= null);
                        Abort_Check_Sources (Sel.all);
                        pragma Debug (O ("Aborted."));
                     end;

                  when Idle =>
                     Broadcast
                       (Condition (Req.Requesting_Task.all));
                     --  Cannot use Signal here, because it wakes
                     --  up only one task, which may or may not be
                     --  the Requesting_Task.
                     --  XXX This is inefficient. Each task should
                     --  have its own CV used as suspension object.
               end case;
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

            --  If we are currently processing an event that
            --  happened on TE, the ORB_Lock is already held.
            --  This can also be called from within the processing
            --  of a request job, i.e. outside of ORB_Lock, so
            --  for now we re-enter ORB_Lock.

            Enter (ORB.ORB_Lock);
            Insert_Source (ORB, Note.AES);
            Leave (ORB.ORB_Lock);

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
      (Name => +"orb",
       Conflicts => Empty,
       Depends => +"soft_links" & "orb.tasking_policy"
         & "binding_data.soap?"
         & "binding_data.srp?"
         & "binding_data.iiop?"
         & "protocols.srp?"
         & "protocols.giop?"
         & "protocols.soap?"
         & "smart_pointers"
         & "exceptions.stack",
       Provides => Empty,
       Init => Initialize'Access));

end PolyORB.ORB;
