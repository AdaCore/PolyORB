--  $Id$

with Ada.Real_Time;
with Ada.Exceptions;

with Droopi.Annotations;
with Droopi.Components;
with Droopi.Constants;
with Droopi.Filters;
with Droopi.Filters.Interface;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.Objects.Interface;
with Droopi.ORB.Interface;
with Droopi.ORB.Task_Info;
with Droopi.References.Binding;
with Droopi.Requests;
with Droopi.Soft_Links;
with Droopi.Transport;

package body Droopi.ORB is

   use Droopi.Annotations;
   use Droopi.Asynch_Ev;
   use Droopi.Components;
   use Droopi.Filters;
   use Droopi.Jobs;
   use Droopi.Log;
   use Droopi.Requests;
   use Droopi.Soft_Links;
   use Droopi.Transport;

   package L is new Droopi.Log.Facility_Log ("droopi.orb");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------------------------------
   -- Job type for method execution requests --
   --------------------------------------------

   type Request_Job is new Job with record
      ORB       : ORB_Access;
      Request   : Request_Access;
      Requestor : Components.Component_Access;
   end record;

   procedure Run (J : access Request_Job);

   ---------------------------------------
   -- Tasking policy generic operations --
   ---------------------------------------

   procedure Run_Job
     (P : access Tasking_Policy_Type;
      J : Jobs.Job_Access) is
   begin
      if J.all in Request_Job then
         Handle_Request_Execution
           (P   => Tasking_Policy_Access (P),
            ORB => Request_Job (J.all).ORB,
            RJ  => J);
      else
         Run (J);
      end if;
   end Run_Job;

   ---------------------------
   -- ORB object operations --
   ---------------------------

   procedure Create (ORB : in out ORB_Type) is
   begin
      Create (ORB.ORB_Lock);
      --  From now on access to ORB state is protected by this mutex.

      Enter (ORB.ORB_Lock.all);

      Create (ORB.Idle_Tasks);

      ORB.Job_Queue := Droopi.Jobs.Create_Queue;
      ORB.Shutdown := False;
      ORB.Polling  := False;
      Leave (ORB.ORB_Lock.all);
   end Create;

   procedure Start (ORB : access ORB_Type);

   procedure Start (ORB : access ORB_Type) is
   begin
      --  Start accepting incoming connections.
      --  XXX TBD
      raise Not_Implemented;
   end Start;

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
      if not Empty (Q) then
         declare
            Job : Job_Access := Fetch_Job (Q);
         begin
            Leave (ORB.ORB_Lock.all);

            pragma Assert (Job /= null);
            Run_Job (ORB.Tasking_Policy, Job);
            Free (Job);
            return True;
         end;
      else
         return False;
      end if;
   end Try_Perform_Work;

   type AES_Note_Kind is
     (A_TAP_AES,
      --  Annotation for an asynchronous event source
      --  associated with a transport access point.

      A_TE_AES
      --  Annotation for an asynchronous event source
      --  associated with a transport endpoint.
      );

   type AES_Note_Data (Kind : AES_Note_Kind := AES_Note_Kind'First)
   is record
      case Kind is
         when A_TAP_AES =>
            TAP : Transport_Access_Point_Access;
            --  Factory of Transport_Endpoint components.

            Filter_Factory_Chain : Filters.Factory_Access;
            --  Factory of Filter (protocol stack) components.

            Profile_Factory : Binding_Data.Profile_Factory_Access;
            --  Factory of profiles capable of associating the
            --  address of TAP and the specification of the
            --  protocol implemented by Filter_Factory_Chain
            --  with an object id.

         when A_TE_AES =>
            TE : Transport_Endpoint_Access;
            --  Transport_Endpoint component (connected to a
            --  protocol stack).
      end case;
   end record;

   type AES_Note is new Note with record
      D : AES_Note_Data;
   end record;

   type TAP_Note is new Note with record
      Profile_Factory : Binding_Data.Profile_Factory_Access;
   end record;

   procedure Handle_Event
     (ORB : access ORB_Type;
      AES : Asynch_Ev_Source_Access);
   --  Process an event that occurred on AES.

   procedure Handle_Event
     (ORB : access ORB_Type;
      AES : Asynch_Ev_Source_Access)
   is
      Note : AES_Note;

   begin
      Get_Note (Notepad_Of (AES).all, Note);
      case Note.D.Kind is
         when A_TAP_AES =>
            declare
               New_TE     : Transport_Endpoint_Access;
            begin
               Accept_Connection (Note.D.TAP.all, New_TE);
               --  Create transport endpoint.

               Register_Endpoint
                 (ORB, New_TE, Note.D.Filter_Factory_Chain, Server);
            end;

            Insert_Source (ORB, AES);
            --  Continue monitoring the TAP's AES.

         when A_TE_AES =>
            begin
               Emit_No_Reply
                 (Component_Access (Note.D.TE),
                  Filters.Interface.Data_Indication'
                    (null record));

               Insert_Source (ORB, AES);
               --  Continue monitoring this source.

            exception
               when E : others =>
                  O ("Got exception while sending Data_Indication");
                  O (Ada.Exceptions.Exception_Information (E));
                  --  XXX What to do?
                  --  raise; ???
            end;
      end case;
   end Handle_Event;

   procedure Run
     (ORB            : access ORB_Type;
      Exit_Condition : Exit_Condition_Access := null;
      May_Poll       : Boolean := False)
   is
      use Task_Info;

      Task_Kind_For_Exit_Condition : constant array (Boolean)
        of Task_Kind := (True => Permanent, False => Transient);
      --  The task kind according to whether Exit_Condition
      --  is null (True) or not.

      This_Task : aliased Task_Info.Task_Info
        (Task_Kind_For_Exit_Condition (Exit_Condition = null));

   begin
      loop
         pragma Debug (O ("Run: enter loop."));
         Enter (ORB.ORB_Lock.all);

         if (Exit_Condition /= null and then Exit_Condition.all)
           or else ORB.Shutdown
         then
            Leave (ORB.ORB_Lock.all);
            exit;
         end if;

         if Try_Perform_Work (ORB, ORB.Job_Queue) then
            null;
         elsif May_Poll then
            pragma Debug (O ("About to poll external event sources."));
            declare
               Monitors : constant Monitor_Seqs.Element_Array
                 := Monitor_Seqs.To_Element_Array (ORB.Monitors);

               Poll_Interval : constant Duration := 0.1;
               --  XXX Poll_Interval should be configurable.

               Timeout : Duration;

               Event_Happened : Boolean := False;

            begin

               if Monitors'Length = 1 then
                  Timeout := Droopi.Constants.Forever;
               else
                  Timeout := 0.0;
               end if;

               --  ORB.ORB_Lock is held.

               for I in Monitors'Range loop
                  ORB.Polling := True;
                  ORB.Selector := Monitors (I);
                  Set_Status_Blocked (This_Task, Monitors (I));

                  Leave (ORB.ORB_Lock.all);

                  declare
                     Events : constant AES_Array
                       := Check_Sources (Monitors (I), Timeout);
                  begin
                     Enter (ORB.ORB_Lock.all);
                     ORB.Polling := False;
                     ORB.Selector := null;
                     Set_Status_Running (This_Task);

                     if Events'Length > 0 then
                        Event_Happened := True;
                     end if;
                     for I in Events'Range loop
                        Handle_Event (ORB, Events (I));
                     end loop;
                  end;
               end loop;

               --  ORB.ORB_Lock is held.

               if Event_Happened then
                  Update (ORB.Idle_Tasks);
               end if;
               Leave (ORB.ORB_Lock);

               --  Waiting for an event to happend when in polling
               --  situation: ORB.ORB_Lock is not held.

               if not (Event_Happened or else Monitors'Length = 1) then
                  declare
                     use Ada.Real_Time;

                     Poll_Expire : constant Time
                       := Clock + To_Time_Span (Poll_Interval);
                  begin
                     delay until Poll_Expire;
                  end;
               end if;
            end;

         else

            --  This task is going idle.

            Enter (ORB.ORB_Lock.all);
            Set_Status_Idle (This_Task, ORB.Idle_Tasks);
            Leave (ORB.ORB_Lock.all);

            Idle (ORB.Tasking_Policy, ORB_Access (ORB));
            --  XXX Dunno if this is the right interface
            --  between ORB and TP for idling.

            Enter (ORB.ORB_Lock.all);
            Set_Status_Running (This_Task);
            Leave (ORB.ORB_Lock.all);

            --  XXX memo for selves:
            --  How to idle with style:
            --  Lookup (ORB.Idle_Tasks, V);
            --  Differ (ORB.Idle_Tasks, V);
         end if;
      end loop;
      pragma Debug (O ("Run: leave."));
   end Run;

   function Work_Pending (ORB : access ORB_Type) return Boolean
   is
      Result : Boolean;
   begin
      Enter (ORB.ORB_Lock.all);
      Result := not Empty (ORB.Job_Queue);
      Leave (ORB.ORB_Lock.all);
      return Result;
   end Work_Pending;

   procedure Perform_Work (ORB : access ORB_Type) is
   begin
      Enter (ORB.ORB_Lock.all);
      if not Try_Perform_Work (ORB, ORB.Job_Queue) then
         Leave (ORB.ORB_Lock.all);
      end if;
   end Perform_Work;

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

   procedure Register_Access_Point
     (ORB   : access ORB_Type;
      TAP   : Transport_Access_Point_Access;
      Chain : Filters.Factory_Access;
      PF    : Binding_Data.Profile_Factory_Access)
   is
      New_AES : Asynch_Ev_Source_Access;
   begin
      New_AES := Create_Event_Source (TAP.all);
      --  Create associated asynchronous event source.

      Set_Note (Notepad_Of (New_AES).all,
                AES_Note'(Annotations.Note with D =>
                            (Kind   => A_TAP_AES,
                             TAP    => TAP,
                             Filter_Factory_Chain => Chain,
                             Profile_Factory => PF)));
      --  Set link from AES to TAP, Chain and PF.

      Set_Note (Notepad_Of (TAP).all,
                TAP_Note'(Note with Profile_Factory => PF));
      --  Set link from TAP to PF.

      Enter (ORB.ORB_Lock.all);
      Insert_Source (ORB, New_AES);
      TAP_Seqs.Append (ORB.Transport_Access_Points, TAP);
      Leave (ORB.ORB_Lock.all);
   end Register_Access_Point;

   function Is_Profile_Local
     (ORB : access ORB_Type;
      P   : Binding_Data.Profile_Access)
     return Boolean is
   begin
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

   procedure Register_Endpoint
     (ORB   : access ORB_Type;
      TE    : Transport_Endpoint_Access;
      Chain : Filters.Factory_Access;
      Role  : Endpoint_Role)
   is
      New_AES    : Asynch_Ev_Source_Access;
      New_Filter : Filter_Access;
   begin
      New_AES := Create_Event_Source (TE.all);
      --  Create associated asynchronous event source.

      New_Filter := Create_Filter_Chain (Chain);
      --  Create filter/protocol stack.

      Connect_Upper (TE, Component_Access (New_Filter));
      Connect_Lower (New_Filter, Component_Access (TE));
      --  Connect filter to transport.

      Emit_No_Reply
        (Component_Access (New_Filter),
         Filters.Interface.Set_Server'
         (Server => Component_Access (ORB)));

      Set_Note
        (Notepad_Of (New_AES).all,
         AES_Note'(Annotations.Note with D =>
                     (Kind   => A_TE_AES,
                      TE     => TE)));
      --  Register link from AES to TE.

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

   procedure Set_Object_Adapter
     (ORB : access ORB_Type;
      OA  : Obj_Adapters.Obj_Adapter_Access)
   is
      use Obj_Adapters;
   begin
      pragma Assert (ORB.Obj_Adapter = null);
      ORB.Obj_Adapter := OA;
   end Set_Object_Adapter;

   function Object_Adapter (ORB : access ORB_Type)
     return Obj_Adapters.Obj_Adapter_Access is
   begin
      return ORB.Obj_Adapter;
   end Object_Adapter;

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
            Register_Source (Monitors (I).all, AES, Success);
            exit when Success;
         end loop;

         if not Success then
            declare
               New_AEM : constant Asynch_Ev_Monitor_Access
                 := AEM_Factory_Of (AES.all).all;
            begin
               Create (New_AEM.all);
               Append (ORB.Monitors, New_AEM);
               Register_Source (New_AEM.all, AES, Success);
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

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : Asynch_Ev_Source_Access) is
   begin
      Enter (ORB.ORB_Lock.all);

      Unregister_Source (AES);

      if ORB.Polling then
         pragma Assert (ORB.Selector /= null);
         Abort_Check_Sources (ORB.Selector.all);
      end if;
      Leave (ORB.ORB_Lock.all);
   end Delete_Source;

   ----------------------------------
   -- Job type for object requests --
   ----------------------------------

   procedure Run (J : access Request_Job) is
   begin
      pragma Debug (O ("Run Request_Job: enter"));
      pragma Assert (J.Request /= null);

      declare
--           Oid : constant Objects.Object_Id
--             := Extract_Local_Object_Id (J.Req.Target);
         Servant : constant Objects.Servant_Access
           := References.Binding.Bind (J.Request.Target, J.ORB);
      begin
         pragma Debug (O ("Executing: "
                          & Requests.Image (J.Request.all)));
         --  Setup_Environment (Oid);

         declare
            Result : constant Components.Message'class
              := Emit (Component_Access (Servant),
                       Objects.Interface.Execute_Request'
                       (Req => J.Request));
         begin
            --  Unsetup_Environment ();
            --  Unbind (J.Req.Target, J.ORB, Servant);
            --  XXX Unbind must Release_Servant.

            Emit_No_Reply (J.Requestor, Result);
            --  Send back answer.

            --  XXX Should that be Emit? Should there be a reply
            --      from Requestor?

            --  The client is responsible for destroying
            --  the request object after use.

         end;
         pragma Debug (O ("Run Request_Job: executed request"));
      end;

   exception
      when E : others =>
         pragma Debug (O ("Run Request_Job: Got exception "
                          & Ada.Exceptions.Exception_Information (E)));
         raise;

   end Run;

   procedure Create_Reference
     (ORB : access ORB_Type;
      Oid : Objects.Object_Id_Access;
      Ref : out References.Ref) is
   begin
      Enter (ORB.ORB_Lock.all);
      declare
         use Droopi.Binding_Data;
         use TAP_Seqs;

         TAPs : constant Element_Array
           := To_Element_Array (ORB.Transport_Access_Points);

         Profiles : References.Profile_Array (TAPs'Range);
      begin
         Leave (ORB.ORB_Lock.all);
         for I in TAPs'Range loop
            Profiles (I) := Create_Profile
              (Profile_Factory_Of (TAPs (I)), TAPs (I), Oid.all);
         end loop;

         References.Create_Reference (Profiles, Ref);
      end;
   end Create_Reference;

   function Handle_Message
     (ORB : access ORB_Type;
      Msg : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class
   is
      use Droopi.Objects.Interface;

      Result : Components.Null_Message;
   begin
      if Msg in Interface.Queue_Job then
         Queue_Job (ORB.Job_Queue,
                    Interface.Queue_Job (Msg).Job);
      elsif Msg in Interface.Queue_Request then
         declare
            J : constant Job_Access := new Request_Job;
            QR : Interface.Queue_Request
              renames Interface.Queue_Request (Msg);
            Req : Requests.Request_Access renames QR.Request;
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
               --  XXX Annotate Req with a pointer to the requesting
               --  task's Task_Info.
               --  Set_Note (Req.Notepad, Request_Note'
               --        (Requesting_ORB_Task => This_Task));
            else
               Request_Job (J.all).Requestor := QR.Requestor;
            end if;

            Queue_Job (ORB.Job_Queue, J);
         end;
      elsif Msg in Executed_Request then
         Executed_Request (Msg).Req.Completed := True;

         --  XXX NOTIFY ALL SLEEPING ORB TASKS THAT
         --  A WAIT CONDITION HAS BEEN UPDATED!
         --  If Request is annotated with a pointer
         --  to a task_info, this means that the corresponding
         --  task is a transient one that must be waked up
         --  because its exit condition may have changed.

         --  Get_Note (req, request_note)
         --  if Task_Info /= null then
         --     case Status (Task_Info) is
         --        when Running => null;
         --        when Blocked => Abort (Selector (Task_Info));
         --        when Idle => Update (Watcher (Task_Info));
         --     end case;
         --  end if;

         --  Must also ensure that proper locking is performed.

      else
         raise Components.Unhandled_Message;
      end if;

      return Result;
   end Handle_Message;

end Droopi.ORB;
