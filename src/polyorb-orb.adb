------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . O R B                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  The ORB core module

with Ada.Exceptions;
with Ada.Tags;

with PolyORB.Any.Initialization;
with PolyORB.Binding_Data.Local;
with PolyORB.Binding_Object_QoS;
with PolyORB.Errors;
with PolyORB.Filters.Iface;
with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.ORB.Iface;
with PolyORB.References.Binding;
with PolyORB.Request_QoS;
with PolyORB.Servants.Iface;
with PolyORB.Setup;
with PolyORB.Smart_Pointers.Initialization;
with PolyORB.Tasking.Threads;
with PolyORB.Transport.Handlers;
with PolyORB.Utils.Strings;

--  The following units are with'd only so that they register initialization
--  modules.

pragma Warnings (Off, PolyORB.Any.Initialization);
pragma Warnings (Off, PolyORB.Smart_Pointers.Initialization);

package body PolyORB.ORB is

   use PolyORB.Annotations;
   use PolyORB.Asynch_Ev;
   use PolyORB.Binding_Data;
   use PolyORB.Binding_Objects;
   use PolyORB.Components;
   use PolyORB.Filters;
   use PolyORB.Jobs;
   use PolyORB.Log;
   use PolyORB.ORB_Controller;
   use PolyORB.References;
   use PolyORB.Requests;
   use PolyORB.Tasking.Threads;
   use PolyORB.Transport;
   use PolyORB.Transport.Handlers;
   use PolyORB.Types;
   use Unsigned_Long_Flags;

   package L is new PolyORB.Log.Facility_Log ("polyorb.orb");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

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
   --  Insert AES in the set of asynchronous event sources monitored by ORB.
   --  The caller must not hold the ORB lock.

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access);
   --  Delete AES from the set of asynchronous event sources monitored by ORB.
   --  AES is destroyed. The caller must not hold the ORB lock.

   --------------------------------------------
   -- Annotations used by the ORB internally --
   --------------------------------------------

   --  Transport access point note

   type TAP_Note is new Note with record
      Profile_Factory : Binding_Data.Profile_Factory_Access;
      AES             : Asynch_Ev.Asynch_Ev_Source_Access;
   end record;

   --  Transport endpoint note

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
      pragma Unreferenced (ORB);

   begin
      --  Note: this function will be completed when implementing support for
      --  multiple ORB instances, as mandated by the CORBA personality.

      null;
   end Create;

   ----------------------------------
   -- Find_Reusable_Binding_Object --
   ----------------------------------

   function Find_Reusable_Binding_Object
     (ORB : access ORB_Type;
      Pro : Binding_Data.Profile_Access;
      QoS : PolyORB.QoS.QoS_Parameters) return Smart_Pointers.Ref
   is
      use BO_Lists;

      It     : Iterator;
      Result : Smart_Pointers.Ref;

   begin
      pragma Debug (O ("Find_Reusable_Binding_Object: enter"));
      pragma Debug (O ("#BO registered = "
        & Natural'Image (Length (ORB.Binding_Objects))));

      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      It := First (ORB.Binding_Objects);

      --  Loop through all the BOs registered in ORB, and try to find one
      --  that can be reused.

      All_Binding_Objects :
      while not Last (It) loop

         --  XXX Check that the profile has been set.
         --  Until bidirectionnal BO are implemented we cannot reuse the server
         --  BOs as client BOs and inversely. So for the moment, server BOs
         --  have a null profile and are not handled here. This check shall be
         --  removed once bidirectionnal BO are implemented.

         declare
            BO_Acc : Binding_Object_Access renames Value (It).all;
         begin
            if Get_Profile (BO_Acc) /= null then

               if Same_Node (Pro.all, Get_Profile (BO_Acc).all)
                    and then
                  PolyORB.Binding_Object_QoS.Is_Compatible (BO_Acc, QoS)
               then

                  --  We know that BO_Acc is still valid, because the
                  --  finalization of the binding object involves unregistering
                  --  it in ORB critical section. However, BO_Acc.all might be
                  --  in the process of being finalized already, i.e. its
                  --  usage counter might have dropped to 0 already. In that
                  --  case, Smart_Pointers.Reuse_Entity will leave Result
                  --  unchanged (nil).

                  Smart_Pointers.Reuse_Entity
                    (Result, Smart_Pointers.Entity_Ptr (BO_Acc));
                  exit All_Binding_Objects
                    when not Smart_Pointers.Is_Nil (Result);
               end if;

            end if;
         end;

         Next (It);
      end loop All_Binding_Objects;

      Leave_ORB_Critical_Section (ORB.ORB_Controller);
      pragma Debug (O ("Find_Reusable_Binding_Object: leave"));

      --  If no reusable Binding Object has been found, Result is a nil
      --  Reference.

      return Result;
   end Find_Reusable_Binding_Object;

   -----------------------
   -- The ORB main loop --
   -----------------------

   --  This is the main loop for all general-purpose ORB tasks. This subprogram
   --  must not be called recursively. Exceptions must not be propagated from
   --  within ORB critical section.

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work
     (ORB       : access ORB_Type;
      This_Task : in out Task_Info.Task_Info);
   pragma Inline (Perform_Work);
   --  Perform one item of work from Q.
   --  Precondition:  Must be called from within ORB critical section.
   --  Postcondition: On exit, ORB critical section has been reasserted.
   --  Note: tasks running this function may exit ORB critical section
   --  temporarily.

   procedure Perform_Work
     (ORB       : access ORB_Type;
      This_Task : in out Task_Info.Task_Info)
   is
      use PolyORB.Task_Info;

   begin
      pragma Debug (O ("TPF " & Image (This_Task) & ": "
                       & "enter Perform_Work"));

      pragma Assert (Task_Info.Job (This_Task) /= null);

      Leave_ORB_Critical_Section (ORB.ORB_Controller);

      pragma Debug (O ("TPF " & Image (This_Task) & ": "
                       & "working on job "
                       & Ada.Tags.External_Tag
                       (Task_Info.Job (This_Task)'Tag)));

      Run (Task_Info.Job (This_Task));

      Enter_ORB_Critical_Section (ORB.ORB_Controller);
      pragma Debug (O ("TPF " & Image (This_Task) & ": "
                       & "leaving Perform_Work"));

      Notify_Event (ORB.ORB_Controller, Job_Completed_E);
   end Perform_Work;

   -----------------------
   -- Try_Check_Sources --
   -----------------------

   procedure Try_Check_Sources
     (ORB       : access ORB_Type;
      This_Task : in out Task_Info.Task_Info);
   pragma Inline (Try_Check_Sources);
   --  Check ORB's AES attached to A_Monitor for any incoming event.
   --  Precondition:  Must be called from within ORB critical section.
   --  Postcondition: On exit, ORB critical section has been reasserted.
   --  Note: tasks running this function may exit ORB critical section
   --  temporarily.

   procedure Try_Check_Sources
     (ORB       : access ORB_Type;
      This_Task : in out Task_Info.Task_Info)
   is
      use PolyORB.Task_Info;

   begin
      --  Inside the ORB critical section

      pragma Debug (O ("Try_Check_Sources: task "
                       & Image (This_Task)
                       & " about to Check_Sources."));

      Leave_ORB_Critical_Section (ORB.ORB_Controller);

      declare
         Events : constant AES_Array :=
                    Check_Sources
                      (Selector (This_Task), Task_Info.Timeout (This_Task));
         --  This_Task will block on this action until an event occurs on a
         --  source monitored by A_Monitor, or Abort_Check_Sources is called
         --  on A_Monitor.

      begin

         --  Reenter critical section to update ORB state

         Enter_ORB_Critical_Section (ORB.ORB_Controller);

         pragma Debug (O ("Try_Check_Sources: task "
                          & Image (This_Task)
                          & " returned from Check_Sources with"
                          & Integer'Image (Events'Length)
                          & " event(s)."));

         --  Queue events, if any

         declare
            Note : AES_Note;

         begin
            for J in Events'Range loop
               Get_Note (Notepad_Of (Events (J)).all, Note);
               Notify_Event
                 (ORB.ORB_Controller,
                  Event'(Kind      => Queue_Event_Job,
                         Event_Job => Job_Access (Note.Handler),
                         By_Task   => Id (This_Task)));
            end loop;
         end;

         --  Notify ORB controller of completion

         Notify_Event (ORB.ORB_Controller,
                       Event'(Kind => End_Of_Check_Sources,
                              On_Monitor => Selector (This_Task)));

         --  Inside ORB critical section

      end;
   end Try_Check_Sources;

   ---------
   -- Run --
   ---------

   procedure Run
     (ORB            : access ORB_Type;
      Exit_Condition :        Exit_Condition_T := (null, null);
      May_Poll       :        Boolean := False)
   is
      use PolyORB.Task_Info;

      This_Task : aliased Task_Info.Task_Info
        (Task_Kind_For_Exit_Condition (Exit_Condition.Condition = null));

   begin
      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      --  Set up task information for This_Task

      Set_Id (This_Task);
      Set_Exit_Condition (This_Task, Exit_Condition.Condition);
      Set_Polling (This_Task, May_Poll);

      if Exit_Condition.Task_Info /= null then
         --  This pointer must be reset to null before exiting Run so as to
         --  not leave a dangling reference.

         Exit_Condition.Task_Info.all := This_Task'Unchecked_Access;
      end if;

      Register_Task (ORB.ORB_Controller, This_Task'Unchecked_Access);

      --  ORB Main loop

      Main_Loop :
      loop
         Schedule_Task (ORB.ORB_Controller, This_Task'Unchecked_Access);

         case State (This_Task) is
            when Running =>

               --  This task will process one job

               Perform_Work (ORB, This_Task);

            when Blocked =>

               --  This task will block on event sources, waiting for events

               Try_Check_Sources (ORB, This_Task);

            when Idle =>

               --  This task is going idle. We are still inside the ORB
               --  critical section at this point. The tasking policy will
               --  release it while we are idle, and re-assert it before
               --  returning.

               Idle (ORB.Tasking_Policy, This_Task, ORB_Access (ORB));
               Notify_Event
                 (ORB.ORB_Controller,
                  Event'(Kind          => Idle_Awake,
                         Awakened_Task => This_Task'Unchecked_Access));

            when Terminated =>

               --  This task has reached its exit condition: leave main loop

               exit Main_Loop;

            when Unscheduled =>

               --  This task is still unscheduled, this should not happen!

               raise Program_Error;

         end case;

         Set_State_Unscheduled (This_Task);

         --  Condition at end of loop: inside the ORB critical section

      end loop Main_Loop;

      --  Remove reference to This_Task

      if Exit_Condition.Task_Info /= null then
         Exit_Condition.Task_Info.all := null;
      end if;

      Unregister_Task (ORB.ORB_Controller, This_Task'Unchecked_Access);

      pragma Debug (O ("Run: leave."));

      Leave_ORB_Critical_Section (ORB.ORB_Controller);

   exception
      when E : others =>

         --  At this point it is assumed that we are not in the ORB
         --  critical section.

         O ("ORB.Run got exception:", Error);
         O (Ada.Exceptions.Exception_Information (E), Error);

         --  Remove reference to This_Task

         if Exit_Condition.Task_Info /= null then
            Exit_Condition.Task_Info.all := null;
         end if;

         Set_State_Terminated (This_Task);
         Unregister_Task (ORB.ORB_Controller, This_Task'Unchecked_Access);

         raise;
   end Run;

   ------------------
   -- Work_Pending --
   ------------------

   function Work_Pending (ORB : access ORB_Type) return Boolean is
      Result : Boolean;

   begin
      Enter_ORB_Critical_Section (ORB.ORB_Controller);
      Result := Is_A_Job_Pending (ORB.ORB_Controller);
      Leave_ORB_Critical_Section (ORB.ORB_Controller);

      return Result;
   end Work_Pending;

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work (ORB : access ORB_Type) is
      Job : Job_Access;

   begin
      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      if Is_A_Job_Pending (ORB.ORB_Controller) then
         Job := Get_Pending_Job (ORB.ORB_Controller);
         Leave_ORB_Critical_Section (ORB.ORB_Controller);
         Run (Job);

      else
         Leave_ORB_Critical_Section (ORB.ORB_Controller);
      end if;
   end Perform_Work;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (ORB                 : access ORB_Type;
      Wait_For_Completion :        Boolean := True)
   is
   begin

      pragma Debug (O ("Shutdown: enter"));

      --  Stop accepting incoming connections

      --  XXX TBD

      --  Shutdown the ORB

      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      Notify_Event (ORB.ORB_Controller, ORB_Shutdown_E);

      --  Wait for completion of pending requests, if required

      if Wait_For_Completion then
         ORB_Controller.Wait_For_Completion (ORB.ORB_Controller);
      end if;

      Leave_ORB_Critical_Section (ORB.ORB_Controller);

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
      Chain :        PF.Factories_Access;
      PF    :        PBD.Profile_Factory_Access)
   is
      New_AES : constant Asynch_Ev_Source_Access :=
                  Create_Event_Source (TAP);
      A_Note  : AES_Note;

   begin
      pragma Debug (O ("Register_Access_Point: enter"));

      --  Set link from AES to TAP, Chain and PF

      Get_Note (Notepad_Of (New_AES).all, A_Note);
      declare
         Handler : constant AES_Event_Handler_Access := A_Note.Handler;
         TAP_Handler : TAP_AES_Event_Handler
           renames TAP_AES_Event_Handler (Handler.all);
      begin
         Handler.AES := New_AES;
         TAP_Handler.ORB := ORB_Access (ORB);
         TAP_Handler.TAP := TAP;
         TAP_Handler.Filter_Factory_Chain := Chain;
         TAP_Handler.Profile_Factory := PF;
      end;

      --  Set link from TAP to PF, and from TAP to AES

      Set_Note (Notepad_Of (TAP).all,
                TAP_Note'(Note with Profile_Factory => PF, AES => New_AES));

      Enter_ORB_Critical_Section (ORB.ORB_Controller);
      pragma Debug (O ("Inserting new source: Access Point"));
      TAP_Lists.Append (ORB.Transport_Access_Points, TAP);
      Leave_ORB_Critical_Section (ORB.ORB_Controller);

      Insert_Source (ORB, New_AES);

      pragma Debug (O ("Register_Access_Point: leave"));
   end Register_Access_Point;

   ----------------------
   -- Is_Profile_Local --
   ----------------------

   function Is_Profile_Local
     (ORB : access ORB_Type;
      P   : access Binding_Data.Profile_Type'Class) return Boolean
   is
   begin
      if P.all in Binding_Data.Local.Local_Profile_Type then
         return True;
      end if;

      Enter_ORB_Critical_Section (ORB.ORB_Controller);
      declare
         use TAP_Lists;
         It     : Iterator := First (ORB.Transport_Access_Points);
         PF     : Profile_Factory_Access;
         Result : Boolean := False;

      begin
         All_Access_Points :
         while not Last (It) loop
            PF := Profile_Factory_Of (Value (It).all);

            if PF /= null then
               if Binding_Data.Is_Local_Profile (PF, P) then
                  Result := True;
                  exit All_Access_Points;
               end if;
            end if;

            Next (It);
         end loop All_Access_Points;
         Leave_ORB_Critical_Section (ORB.ORB_Controller);

         return Result;
      end;
   end Is_Profile_Local;

   -----------------------------
   -- Register_Binding_Object --
   -----------------------------

   procedure Register_Binding_Object
     (ORB  : access ORB_Type;
      BO   :        Smart_Pointers.Ref;
      Role :        Endpoint_Role)
   is
      TE         : constant Transport.Transport_Endpoint_Access :=
                     Binding_Objects.Get_Endpoint (BO);
      New_AES    : constant Asynch_Ev_Source_Access :=
                     Create_Event_Source (TE);
      --  New_AES is null for output-only endpoints

      A_Note  : AES_Note;
      ORB_Acc : constant ORB_Access := ORB_Access (ORB);
   begin
      pragma Debug
        (O ("Register_Binding_Object (" & Role'Img & "): enter"));

      declare
         BO_Acc : constant Binding_Object_Access :=
                    Binding_Object_Access (Smart_Pointers.Entity_Of (BO));
         It : BO_Lists.Iterator;
      begin

         Enter_ORB_Critical_Section (ORB.ORB_Controller);

         --  Register BO in the Binding_Objects list of ORB

         BO_Lists.Prepend (ORB.Binding_Objects, BO_Acc);

         --  Save the position

         It := BO_Lists.First (ORB.Binding_Objects);

         --  Record in BO a reference to its position on the list so that it
         --  can remove itself properly at finalization.

         Register_Reference_Information
                                 (BO => BO_Acc,
                                  Referenced_In => Component_Access (ORB),
                                  Referenced_At => It);

         Leave_ORB_Critical_Section (ORB.ORB_Controller);
      end;

      Emit_No_Reply
        (Component_Access (TE),
         Filters.Iface.Set_Server'
         (Server         => Component_Access (ORB),
          Binding_Object =>
            Binding_Objects.Binding_Object_Access
          (Smart_Pointers.Entity_Of (BO))));

      if New_AES /= null then

         --  This is not a write only Endpoint

         Get_Note (Notepad_Of (New_AES).all, A_Note);

         declare
            Handler : constant AES_Event_Handler_Access
              := A_Note.Handler;
            TE_Handler : TE_AES_Event_Handler
              renames TE_AES_Event_Handler (Handler.all);

         begin

            --  Register link from AES to TE

            Handler.AES := New_AES;
            TE_Handler.ORB := ORB_Access (ORB);
            TE_Handler.TE := TE;
         end;
      end if;

      --  Register link from TE to AES

      Set_Note
        (Notepad_Of (TE).all,
         TE_Note'(Annotations.Note with AES => New_AES));

      --  Assign execution resources to the newly-created connection

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

      pragma Debug (O ("Register_Binding_Object: leave"));
   end Register_Binding_Object;

   -------------------------------
   -- Unregister_Binding_Object --
   -------------------------------

   procedure Unregister_Binding_Object
     (ORB  : Components.Component_Access;
      It   :        PBO.BO_Lists.Iterator)
   is
      ORB_Acc : constant ORB_Access := ORB_Access (ORB);
      Variable_It : BO_Lists.Iterator := It;
   begin
      pragma Debug (O ("Unregister_Binding_Object: enter"));
      Enter_ORB_Critical_Section (ORB_Acc.ORB_Controller);
      BO_Lists.Remove (ORB_Acc.Binding_Objects, Variable_It);
      Leave_ORB_Critical_Section (ORB_Acc.ORB_Controller);
      pragma Debug (O ("Unregister_Binding_Object: leave"));
   end Unregister_Binding_Object;

   ------------------------
   -- Set_Object_Adapter --
   ------------------------

   procedure Set_Object_Adapter
     (ORB : access ORB_Type;
      OA  :        Obj_Adapters.Obj_Adapter_Access)
   is
      use type Obj_Adapters.Obj_Adapter_Access;

   begin
      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      pragma Assert (ORB.Obj_Adapter = null);
      ORB.Obj_Adapter := OA;

      Leave_ORB_Critical_Section (ORB.ORB_Controller);
   end Set_Object_Adapter;

   --------------------
   -- Object_Adapter --
   --------------------

   function Object_Adapter (ORB : access ORB_Type)
     return Obj_Adapters.Obj_Adapter_Access is
   begin
      --  Per construction, ORB.Obj_Adapter is a read-only component.
      --  No critical section is required.

      return ORB.Obj_Adapter;
   end Object_Adapter;

   -------------------
   -- Insert_Source --
   -------------------

   procedure Insert_Source
     (ORB : access ORB_Type;
      AES :        Asynch_Ev_Source_Access)
   is
   begin
      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      pragma Debug (O ("Insert_Source: enter"));
      pragma Debug
        (O ("Source type: " & Ada.Tags.External_Tag (AES.all'Tag)));

      pragma Assert (AES /= null);

      declare
         Monitors : constant Monitor_Array :=
                      Get_Monitors (ORB.ORB_Controller);
         Success : Boolean := False;
      begin
         for J in Monitors'Range loop

            --  Try to register the source to an existing monitor

            Disable_Polling (ORB.ORB_Controller, Monitors (J));

            Register_Source (Monitors (J), AES, Success);
            Enable_Polling (ORB.ORB_Controller, Monitors (J));
            if Success then
               Notify_Event (ORB.ORB_Controller,
                             Event'(Kind           => Event_Sources_Added,
                                    Add_In_Monitor => Monitors (J)));
               exit;
            end if;
         end loop;

         if not Success then

            --  Create a new monitor and register the source

            pragma Debug (O ("Creating new monitor"));

            declare
               New_AEM : constant Asynch_Ev_Monitor_Access :=
                           AEM_Factory_Of (AES.all).all;
            begin
               pragma Debug (O ("AEM: "
                                & Ada.Tags.External_Tag (New_AEM.all'Tag)));
               Create (New_AEM.all);

               --  In this situation, no task can be polling this monitor yet,
               --  so no need to disable polling.

               Register_Source (New_AEM, AES, Success);
               pragma Assert (Success);

               Notify_Event (ORB.ORB_Controller,
                             Event'(Kind           => Event_Sources_Added,
                                    Add_In_Monitor => New_AEM));

               --  Enable polling on this new monitor

               Enable_Polling (ORB.ORB_Controller, New_AEM);
            end;
         end if;
      end;

      pragma Debug (O ("Insert_Source: leave"));

      Leave_ORB_Critical_Section (ORB.ORB_Controller);
   end Insert_Source;

   -------------------
   -- Delete_Source --
   -------------------

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access)
   is
      Success : Boolean;
      Monitor : constant Asynch_Ev_Monitor_Access := AEM_Of (AES.all);
   begin

      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      pragma Debug (O ("Delete_Source: enter"));

      --  Disable polling to enable safe modification of AES list

      Disable_Polling (ORB.ORB_Controller, Monitor);

      --  Remove source

      Unregister_Source (Monitor.all, AES, Success);

      if Success then
         Notify_Event (ORB.ORB_Controller, Event_Sources_Deleted_E);
      end if;

      --  Modification completed, enable polling

      Enable_Polling (ORB.ORB_Controller, Monitor);

      Leave_ORB_Critical_Section (ORB.ORB_Controller);

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

   procedure Run (J : access Request_Job) is
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

         if J.Request.Completed then

            --  The request can be already marked as completed in the case
            --  where an error has been detected during immediate argument
            --  unmarshalling (case of a malformed SOAP argument
            --  representation, for example).

            pragma Debug (O ("Request completed due to early error"));

            Emit_No_Reply (J.Requestor,
                           Servants.Iface.Executed_Request'
                           (Req => J.Request));
            return;

         elsif Is_Set (Sync_None, J.Request.Req_Flags) then

            --  At this point, the request has been queued, the Sync_None
            --  synchronisation policy has been completed.
            --  We bounce back the response to the requesting task.

            pragma Debug (O ("Sync_None completed"));

            Emit_No_Reply (J.Requestor,
                           Servants.Iface.Executed_Request'
                           (Req => J.Request));

            J.Request.Completed := True;
         end if;

         --  Bind target reference to a servant if this is a local reference,
         --  or a surrogate if this is remote reference.

         declare
            use PolyORB.Errors;

            Error : Error_Container;

         begin
            References.Binding.Bind
              (J.Request.Target,
               J.ORB,
               Request_QoS.Get_Request_QoS (J.Request),
               Surrogate,
               Pro,
               False,
               Error);
            --  XXX potential race condition, we may protect this
            --  call, to be discussed.

            if Found (Error) then
               pragma Debug (O ("Run_Request: Got an error when binding: "
                                & Error_Id'Image (Error.Kind)));

               --  Any error except ForwardLocation_E caught at this level
               --  implies a problem within the object adapter. We bounce the
               --  exception to the user for further processing.

               Set_Exception (J.Request, Error);
               Catch (Error);

               Emit_No_Reply (J.Requestor,
                              Servants.Iface.Executed_Request'
                              (Req => J.Request));
               return;
            end if;
         end;

         --  At this point, the server has been contacted, a binding has been
         --  created, a servant manager has been reached. We are about to send
         --  the request to the target.

         if Is_Set (Sync_With_Server, J.Request.Req_Flags)
           and then Is_Profile_Local (J.ORB, Pro)
         then
            --  We are on the server side, and use Sync_With_Server
            --  synchronization: we can send an Executed_Request
            --  message to the client prior to run the request.

            pragma Debug (O ("With_Server completed, sending"
                             & " Acknowledge_Request message"));

            Emit_No_Reply (J.Requestor,
                           Servants.Iface.Acknowledge_Request'
                           (Req => J.Request));
         end if;

         --  Setup_Environment (Oid);
         --  XXX for 'Current' (applicative personality API for access
         --  to the oid of the current called instance, in the context
         --  of a servant handling multiple oids.)

         declare
            Result : constant Components.Message'Class
              := Emit (Surrogate,
                       Servants.Iface.Execute_Request'
                       (Req => J.Request,
                        Pro => Pro));
         begin
            --  Unsetup_Environment ();
            --  Unbind (J.Req.Target, J.ORB, Servant);
            --  XXX Unbind must Release_Servant.

            --  XXX Actually cannot unbind here: if the binding
            --    object is destroyed that early, we won't
            --    have the opportunity to receive a reply...
            pragma Debug (O ("Run_Request: got "
              & Ada.Tags.External_Tag (Result'Tag)));

            if Result not in Null_Message then
               begin
                  Emit_No_Reply (J.Requestor, Result);
                  --  XXX issue: if we are on the server side, and the
                  --  transport layer has detected a disconnection while we
                  --  were processing the request, the Requestor (Session)
                  --  object here could have become invalid. For now we hack
                  --  around this issue in an ugly fashion by catching
                  --  all exceptions.
               exception
                  when E : others =>
                     O ("Got exception sending Executed_Request:" & ASCII.LF
                        & Ada.Exceptions.Exception_Information (E), Error);
               end;

            end if;
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
      Typ : String;
      Ref :    out References.Ref)
   is
   begin
      pragma Debug (O ("Create_Reference: enter"));

      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      declare
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

                  --  Null profile factories may occur for access points that
                  --  have an ad hoc protocol stack, but no binding data
                  --  information.

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

         Leave_ORB_Critical_Section (ORB.ORB_Controller);

         References.Create_Reference
           (Profiles (Profiles'First .. Last_Profile), Typ, Ref);
      end;
      pragma Debug (O ("Create_Reference: leave"));
   end Create_Reference;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (ORB : access ORB_Type;
      Msg :        PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Servants.Iface;

      Nothing : Components.Null_Message;

   begin
      pragma Debug (O ("Handling message of type "
                       & Ada.Tags.External_Tag (Msg'Tag)));

      if Msg in Iface.Queue_Request then

         declare
            QR  : Iface.Queue_Request renames Iface.Queue_Request (Msg);
            Req : Requests.Request_Access renames QR.Request;
            J   : constant Job_Access := new Request_Job;
         begin
            pragma Debug (O ("Queue_Request: enter"));

            Request_Job (J.all).ORB := ORB_Access (ORB);
            Request_Job (J.all).Request := Req;

            if QR.Requestor = null then

               --  If the request was queued directly by a client, then the
               --  ORB is responsible for setting its state to completed on
               --  reply from the object.

               Request_Job (J.all).Requestor := Component_Access (ORB);
            else

               Request_Job (J.all).Requestor := QR.Requestor;

            end if;

            Req.Requesting_Component := Request_Job (J.all).Requestor;

            Enter_ORB_Critical_Section (ORB.ORB_Controller);
            Notify_Event (ORB.ORB_Controller,
                          Event'(Kind        => Queue_Request_Job,
                                 Request_Job => J,
                                 Target      => Req.Target));
            Leave_ORB_Critical_Section (ORB.ORB_Controller);

            pragma Debug (O ("Queue_Request: leave"));
         end;

      elsif Msg in Executed_Request then
         declare
            use PolyORB.Task_Info;

            Req : Requests.Request
              renames Executed_Request (Msg).Req.all;

         begin

            --  The processing of Executed_Request must be done in the ORB
            --  critical section, because it must not take place between the
            --  time an ORB task checks its exit condition and the moment the
            --  task goes idle.

            Enter_ORB_Critical_Section (ORB.ORB_Controller);

            Req.Completed := True;

            --  XXX The correctness of the following is not
            --  completely determined.
            --  Is this mutitask-safe????

            --  As of 20021122, the answer is NO.
            --  Run evoluted DSA tests with -n 2 -c 100 -s 1
            --  and Thread_Pool server.

            pragma Debug (O ("Request completed."));
            if Req.Requesting_Task /= null then

               --  Notify the requesting task

               pragma Debug
                 (O ("... requesting task is "
                     & Task_State'Image (State (Req.Requesting_Task.all))));

               Notify_Event (ORB.ORB_Controller,
                             Event'(Kind             => Request_Result_Ready,
                                    Requesting_Task  => Req.Requesting_Task));

            else

               --  The requesting task has already taken note of the completion
               --  of the request: nothing to do.

               null;
            end if;
            Leave_ORB_Critical_Section (ORB.ORB_Controller);
         end;

      elsif Msg in Iface.Monitor_Endpoint then
         declare
            TE : constant Transport_Endpoint_Access
              := Iface.Monitor_Endpoint (Msg).TE;
            Note : TE_Note;

         begin
            Get_Note (Notepad_Of (TE).all, Note);
            --  Notes.AES is null for write only Endpoint

            if Note.AES /= null then

               --  Monitor only read/write or read only Endpoint

               pragma Debug (O ("Inserting source: Monitor_Endpoint"));
               Insert_Source (ORB, Note.AES);
            end if;
         end;

      elsif Msg in Iface.Monitor_Access_Point then
         declare
            TAP : constant Transport_Access_Point_Access
              := Iface.Monitor_Access_Point (Msg).TAP;
            Note : TAP_Note;

         begin
            Get_Note (Notepad_Of (TAP).all, Note);

            pragma Debug (O ("Inserting source: Monitor_Access_Point"));
            Insert_Source (ORB, Note.AES);
         end;

      elsif Msg in Iface.Unregister_Endpoint then
         declare
            Note : TE_Note;

         begin
            Get_Note
              (Notepad_Of
               (Iface.Unregister_Endpoint (Msg).TE).all, Note);

            if Note.AES /= null then
               Delete_Source (ORB, Note.AES);
            end if;
         end;

      else
         pragma Debug
           (O ("ORB received unhandled message of type "
               & Ada.Tags.External_Tag (Msg'Tag)));
         raise Program_Error;
      end if;

      return Nothing;
   end Handle_Message;

   -------------------------
   -- Get_Binding_Objects --
   -------------------------

   function Get_Binding_Objects (ORB : access ORB_Type)
     return BO_Ref_List
   is
      use BO_Lists;
      use Smart_Pointers;

      It : BO_Lists.Iterator;
      Result : BO_Ref_List;
   begin
      Enter_ORB_Critical_Section (ORB.ORB_Controller);

      It := First (ORB.Binding_Objects);

      All_Binding_Objects :
      while not Last (It) loop
         declare
            Ref : Smart_Pointers.Ref;
         begin
            Smart_Pointers.Set (Ref, Entity_Ptr (Value (It).all));
            BO_Ref_Lists.Prepend (Result, Ref);
         end;

         Next (It);
      end loop All_Binding_Objects;

      Leave_ORB_Critical_Section (ORB.ORB_Controller);

      return Result;
   end Get_Binding_Objects;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (ORB : access ORB_Type)
     return Annotations.Notepad_Access
   is
   begin
      return ORB.Notepad'Access;
   end Notepad_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
      The_Controller : POC.ORB_Controller_Access;
   begin
      Create (The_Controller);

      Setup.The_ORB := new ORB_Type (Setup.The_Tasking_Policy, The_Controller);
      Create (Setup.The_ORB.all);
   end Initialize;

   ---------------------
   -- Shutdown Module --
   ---------------------

   procedure Shutdown_Module (Wait_For_Completion : Boolean);

   procedure Shutdown_Module (Wait_For_Completion : Boolean) is
   begin
      Shutdown (Setup.The_ORB, Wait_For_Completion);
   end Shutdown_Module;

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
       & "orb_controller"
       & "protocols.srp?"
       & "protocols.giop?"
       & "protocols.soap?"
       & "smart_pointers"
       & "tasking.threads",
       Provides => Empty,
       Implicit => False,
       Init     => Initialize'Access,
       Shutdown => Shutdown_Module'Access));
end PolyORB.ORB;
