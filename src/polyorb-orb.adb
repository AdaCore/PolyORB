------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . O R B                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
with Ada.Real_Time;
with Ada.Tags;
with Ada.Unchecked_Deallocation;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Binding_Data.Local;
with PolyORB.Constants;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Objects.Interface;
with PolyORB.ORB.Interface;
with PolyORB.References;
with PolyORB.References.Binding;
with PolyORB.Setup;
with PolyORB.Soft_Links;
with PolyORB.Task_Info;
with PolyORB.Transport;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body PolyORB.ORB is

   use PolyORB.Annotations;
   use PolyORB.Asynch_Ev;
   use PolyORB.Components;
   use PolyORB.Filters;
   use PolyORB.Jobs;
   use PolyORB.Log;
   use PolyORB.Requests;
   use PolyORB.Soft_Links;
   use PolyORB.Transport;

   use PolyORB.Binding_Data;
   use PolyORB.Types;
   use PolyORB.References;

   package L is new PolyORB.Log.Facility_Log ("polyorb.orb");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------------------------
   -- Tasking policy generic operations --
   ---------------------------------------

   ----------------------
   -- Run_And_Free_Job --
   ----------------------

   procedure Run_And_Free_Job
     (J : in out Jobs.Job_Access) is
   begin
      Run (J);
      Free (J);
   exception
      when others =>
         if J /= null then
            Free (J);
            raise;
         end if;
   end Run_And_Free_Job;

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

   --------------------------------------------------
   -- Event handlers used for the various kinds of --
   -- asynchronous event sources.                  --
   --------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (AES_Event_Handler'Class, AES_Event_Handler_Access);

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

      Enter (ORB.ORB_Lock.all);

      Create (ORB.Idle_Tasks);
      ORB.Job_Queue := PolyORB.Jobs.Create_Queue;
      ORB.Shutdown := False;
      ORB.Polling  := False;
      Create (ORB.Polling_Watcher);
      Leave (ORB.ORB_Lock.all);
      Create (ORB.Idle_Lock);
      Enter (ORB.Idle_Lock);
      ORB.Idle_Counter := 0;
      Leave (ORB.Idle_Lock);
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
     return Boolean is
   begin
      if not Is_Empty (Q) then
         declare
            Job : Job_Access := Fetch_Job (Q);
         begin
            Leave (ORB.ORB_Lock.all);

            pragma Assert (Job /= null);
            Run_And_Free_Job (Job);
            return True;
         end;
      else
         return False;
      end if;
   end Try_Perform_Work;

   ------------------
   -- Handle_Event --
   ------------------

   procedure Handle_Event
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access);
   pragma Inline (Handle_Event);
   --  Process an event that occurred on AES.

   procedure Handle_Event
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access)
   is
      Note : AES_Note;
   begin

      pragma Debug (O ("Handle_Event: enter"));

      Get_Note (Notepad_Of (AES).all, Note);
      Handle_Event (Note.Handler, ORB_Access (ORB), AES);
      if AES = null then
         Free (Note.Handler);
      end if;
   end Handle_Event;

   package body Event_Handlers is

      ------------------
      -- Handle_Event --
      ------------------

      procedure Handle_Event
        (H   : access TAP_AES_Event_Handler;
         ORB :        ORB_Access;
         AES : in out Asynch_Ev_Source_Access)
      is
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
         end;

         Insert_Source (ORB, AES);
         --  Continue monitoring the TAP's AES.
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

            Insert_Source (ORB, AES);
            --  Continue monitoring this source.

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

      Do_Idle : Boolean;

   begin
      Enter (ORB.ORB_Lock.all);
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
            exit Main_Loop
            when (Exit_Condition.Condition /= null
                  and then Exit_Condition.Condition.all)
              or else ORB.Shutdown;

            exit Check_Condition
            when not Try_Perform_Work (ORB, ORB.Job_Queue);

            Enter (ORB.ORB_Lock.all);
            --  Try_Perform_Work has released ORB_Lock and
            --  executed a job from the queue. Reassert ORB_Lock.
         end loop Check_Condition;

         --  ORB_Lock is held.

         Do_Idle := True;

         if May_Poll and then not ORB.Polling then
            declare
               Monitors : constant Monitor_Seqs.Element_Array
                 := Monitor_Seqs.To_Element_Array (ORB.Monitors);

               Poll_Interval : constant Duration := 0.1;
               --  XXX Poll_Interval should be configurable.

               Timeout : Duration;

            begin
               if Monitors'Length = 1 then
                  Timeout := PolyORB.Constants.Forever;
               else
                  Timeout := 0.0;
               end if;

               --  ORB.ORB_Lock is held.

               for I in Monitors'Range loop
                  ORB.Polling := True;
                  ORB.Selector := Monitors (I);
                  Set_Status_Blocked (This_Task, Monitors (I));

                  <<Retry>>
                  ORB.Source_Deleted := False;
                  Lookup (ORB.Polling_Watcher, ORB.Polling_Version);
                  Leave (ORB.ORB_Lock.all);

                  pragma Debug (O ("Run: task " & Image (Current_Task)
                                        & " about to Check_Sources."));
                  declare
                     Events : AES_Array
                       := Check_Sources (Monitors (I), Timeout);
                  begin
                     pragma Debug (O ("Run: task " & Image (Current_Task)
                                        & " returned from Check_Sources."));
                     Enter (ORB.ORB_Lock.all);
                     Update (ORB.Polling_Watcher);
                     if ORB.Source_Deleted then
                        --  An asynchronous event source was unregistered while
                        --  we were blocking, and may now have been destroyed.
                        goto Retry;
                     end if;

                     for J in Events'Range loop
                        Unregister_Source (Monitors (I).all, Events (J));
                     end loop;

                     ORB.Polling := False;
                     ORB.Selector := null;
                     Set_Status_Running (This_Task);

                     for J in Events'Range loop
                        Handle_Event (ORB, Events (J));
                        --  XXX here one task will do *all*
                        --  the I/O on events, this is not
                        --  optimal. Rather, I/O jobs should
                        --  be scheduled.
                     end loop;
                  end;
               end loop;

               --  ORB.ORB_Lock is held.

               Update (ORB.Idle_Tasks);

               --  Waiting for an event to happend when in polling
               --  situation: ORB.ORB_Lock is not held.

               if Monitors'Length /= 1 then
                  declare
                     use Ada.Real_Time;

                     Poll_Expire : constant Time
                       := Clock + To_Time_Span (Poll_Interval);
                  begin
                     Leave (ORB.ORB_Lock.all);
                     delay until Poll_Expire;
                     Enter (ORB.ORB_Lock.all);
                  end;
               end if;
            end;
            Do_Idle := False;
         end if;

         if Do_Idle then

            --  This task is going idle. We are still holding
            --  ORB_Lock at this point.

            Set_Status_Idle (This_Task, ORB.Idle_Tasks);
            Leave (ORB.ORB_Lock.all);

            begin
               Idle (ORB.Tasking_Policy, ORB_Access (ORB));
               --  XXX Dunno if this is the right interface
               --  between ORB and TP for idling.
            exception
               when others =>
                  Enter (ORB.ORB_Lock.all);
                  raise;
            end;

            Enter (ORB.ORB_Lock.all);
            Set_Status_Running (This_Task);

            --  XXX memo for selves:
            --  How to idle with style:
            --  Lookup (ORB.Idle_Tasks, V);
            --  Differ (ORB.Idle_Tasks, V);
         end if;

         --  Condition at end of loop: ORB_Lock is held.

      end loop Main_Loop;
      pragma Debug (O ("Run: leave."));

      if Exit_Condition.Task_Info /= null then
         Exit_Condition.Task_Info.all := null;
      end if;
      Leave (ORB.ORB_Lock.all);

   exception
      when E : others =>
         --  XXX at this point it is assumed that ORB_Lock is
         --  not being held by this task.

         O ("ORB main loop got exception:", Error);
         O (Ada.Exceptions.Exception_Information (E), Error);

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
      Enter (ORB.ORB_Lock.all);
      Result := not Is_Empty (ORB.Job_Queue);
      Leave (ORB.ORB_Lock.all);
      return Result;
   end Work_Pending;

   ------------------
   -- Perform_Work --
   ------------------

   procedure Perform_Work (ORB : access ORB_Type) is
   begin
      Enter (ORB.ORB_Lock.all);
      if not Try_Perform_Work (ORB, ORB.Job_Queue) then
         Leave (ORB.ORB_Lock.all);
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

      Enter (ORB.ORB_Lock.all);
      ORB.Shutdown := True;
      Leave (ORB.ORB_Lock.all);

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
      New_AES : Asynch_Ev_Source_Access;
      Handler : constant AES_Event_Handler_Access
        := new Event_Handlers.TAP_AES_Event_Handler;
      TAP_Handler : Event_Handlers.TAP_AES_Event_Handler
        renames Event_Handlers.TAP_AES_Event_Handler (Handler.all);
   begin
      New_AES := Create_Event_Source (TAP.all);
      --  Create associated asynchronous event source.

      TAP_Handler.TAP := TAP;
      TAP_Handler.Filter_Factory_Chain := Chain;
      TAP_Handler.Profile_Factory := PF;

      Set_Note (Notepad_Of (New_AES).all,
                AES_Note'(Annotations.Note with Handler => Handler));
      --  Set link from AES to TAP, Chain and PF.

      Set_Note (Notepad_Of (TAP).all,
                TAP_Note'(Note with Profile_Factory => PF));
      --  Set link from TAP to PF.

      Enter (ORB.ORB_Lock.all);
      Insert_Source (ORB, New_AES);
      TAP_Seqs.Append (ORB.Transport_Access_Points, TAP);
      Leave (ORB.ORB_Lock.all);
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

      Enter (ORB.ORB_Lock.all);
      declare
         TAPs : constant TAP_Seqs.Element_Array
           := TAP_Seqs.To_Element_Array
           (ORB.Transport_Access_Points);
         Found : Boolean := False;
      begin
         Leave (ORB.ORB_Lock.all);
         for I in TAPs'Range loop
            if Binding_Data.Is_Local_Profile
              (Profile_Factory_Of (TAPs (I)), P)
            then
               Found := True;
            end if;

            exit when Found;
         end loop;
         return Found;
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
      New_AES    : Asynch_Ev_Source_Access;
      Handler : constant AES_Event_Handler_Access
        := new Event_Handlers.TE_AES_Event_Handler;
      TE_Handler : Event_Handlers.TE_AES_Event_Handler
        renames Event_Handlers.TE_AES_Event_Handler (Handler.all);
   begin
      New_AES := Create_Event_Source (TE.all);
      --  Create associated asynchronous event source.

      Connect_Upper (TE, Component_Access (Filter_Stack));
      Connect_Lower (Filter_Stack, Component_Access (TE));
      --  Connect filter to transport.

      Emit_No_Reply
        (Component_Access (TE),
         Filters.Interface.Set_Server'
         (Server => Component_Access (ORB)));

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
      AES : Asynch_Ev_Source_Access) is
   begin

      pragma Assert (AES /= null);

      Enter (ORB.ORB_Lock.all);

      declare
         use Monitor_Seqs;

         Monitors : constant Element_Array
           := To_Element_Array (ORB.Monitors);
         Success : Boolean;
      begin
         Success := False;
         for I in Monitors'Range loop
            Register_Source (Monitors (I), AES, Success);
            exit when Success;
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
         Abort_Check_Sources (ORB.Selector.all);
      end if;
      Leave (ORB.ORB_Lock.all);
   end Insert_Source;

   -------------------
   -- Delete_Source --
   -------------------

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : in out Asynch_Ev_Source_Access)
   is
      Polling : Boolean;
      Polling_Version : Soft_Links.Version_Id;
   begin
      Enter (ORB.ORB_Lock.all);

      Unregister_Source (AES);

      --  After this point we know that this source won't be
      --  considered in a call for Check_Sources, unless Polling
      --  is true already.

      Polling := ORB.Polling;
      if ORB.Polling then
         pragma Assert (ORB.Selector /= null);

         --  The current Check_Sources might consider
         --  this event source, so we need to cause it
         --  to be restarted:

         Abort_Check_Sources (ORB.Selector.all);
         --  1. abort it

         ORB.Source_Deleted := True;
         --  2. invalidate its result (this value will
         --  be tested with ORB_Lock held).

         Polling_Version := ORB.Polling_Version;
         --  3. prepare for notification by the Check_Sources
         --  task that we can safely destroy the AES.

      end if;

      Leave (ORB.ORB_Lock.all);
      if Polling then
         Differ (ORB.Polling_Watcher.all, ORB.Polling_Version);
         --  Need to wait for the blocked task to complete its
         --  call to Check_Sources before destroying the AES.
      end if;
      Destroy (AES);
   end Delete_Source;

   ----------------------------------
   -- Job type for object requests --
   ----------------------------------

   ---------
   -- Run --
   ---------

   procedure Run (J : access Request_Job) is
   begin
      Handle_Request_Execution
        (P => J.ORB.Tasking_Policy, ORB => J.ORB, RJ => J);
   end Run;

   -----------------
   -- Run_Request --
   -----------------

   procedure Run_Request (J : access Request_Job) is
   begin
      pragma Debug (O ("Run Request_Job: enter"));
      pragma Assert (J.Request /= null);

      declare
         Surrogate : Components.Component_Access;
         Pro : PolyORB.Binding_Data.Profile_Access;
      begin
         pragma Debug (O ("Task " & Image (Current_Task)
                          & " executing: "
                          & Requests.Image (J.Request.all)));

         References.Binding.Bind
           (J.Request.Target, J.ORB, Surrogate, Pro);

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
         pragma Debug (O ("Run Request_Job: executed request"));
      end;

   exception
      when E : others =>
         pragma Debug (O ("Run Request_Job: Got exception "
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
      Enter (ORB.ORB_Lock.all);
      declare
         use PolyORB.Binding_Data;
         use TAP_Seqs;

         TAPs : constant Element_Array
           := To_Element_Array (ORB.Transport_Access_Points);

         Profiles : References.Profile_Array (TAPs'Range);
         Last_Profile : Integer := Profiles'First - 1;
      begin
         Leave (ORB.ORB_Lock.all);
         for I in TAPs'Range loop
            declare
               PF : constant Profile_Factory_Access
                 := Profile_Factory_Of (TAPs (I));
            begin
               if PF /= null then
                  --  Null profile factories may occur for access points
                  --  that have an ad hoc protocol stack, but no binding
                  --  data information.
                  Last_Profile := Last_Profile + 1;
                  Profiles (Last_Profile) := Create_Profile
                    (Profile_Factory_Of (TAPs (I)), Oid.all);
                  pragma Assert (Profiles (Last_Profile) /= null);
               end if;
            end;
         end loop;

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
            Update (ORB.Idle_Tasks);
         elsif ORB.Polling then
            pragma Assert (ORB.Selector /= null);
            Abort_Check_Sources (ORB.Selector.all);
         else
            null;
            --  No task is blocked: assume that one will
            --  eventually loop in ORB.Run and process this job.
         end if;

         Leave (ORB.ORB_Lock.all);

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
            Req.Completed := True;

            --  XXX The correctness of the following is not
            --  completely determined.
            --  Is this mutitask-safe????
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
                     Update
                       (Watcher (Req.Requesting_Task.all));
               end case;
            else
               O ("ARGH! No requesting task,"
                  & " discarding Executed_Request.", Error);
               null;
            end if;
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
       Depends => +"soft_links" & "orb.tasking_policy",
       Provides => Empty,
       Init => Initialize'Access));
end PolyORB.ORB;
