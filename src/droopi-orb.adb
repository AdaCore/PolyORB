--  $Id$

with Ada.Exceptions;

with Droopi.Annotations;
with Droopi.Components;
with Droopi.Filters;
with Droopi.Log;
with Droopi.Soft_Links;
with Droopi.Transport;

package body Droopi.ORB is

   use Droopi.Annotations;
   use Droopi.Asynchronous_Events;
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

   ------------------------------
   -- Server object operations --
   ------------------------------

   procedure Run
     (ORB : access ORB_Type; Exit_When : Exit_Condition_Access) is
   begin
      Run (ORB, Exit_When, True);
   end Run;

   function Create_ORB
     (Tasking_Policy : Tasking_Policy_Access)
     return ORB_Access
   is
      ORB : constant ORB_Access := new ORB_Type (Tasking_Policy);
   begin
      Create (ORB.ORB_Lock);
      --  From now on access to ORB state is protected by this mutex.

      Enter (ORB.ORB_Lock.all);

      Create (ORB.Idle_Tasks);

      ORB.Job_Queue := Droopi.Jobs.Create_Queue;
      ORB.Shutdown := False;
      ORB.Polling  := False;
      Leave (ORB.ORB_Lock.all);

      return ORB;
   end Create_ORB;

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
            Run (Job);
            Free (Job);
            return True;
         end;
      else
         return False;
      end if;
   end Try_Perform_Work;

   type ORB_Note_Kind is
     (A_TAP_AES,
      --  Annotation for an asynchronous event source
      --  associated with a transport access point.

      A_TE_AES
      --  Annotation for an asynchronous event source
      --  associated with a transport endpoint.
      );

   type ORB_Note_Data (Kind : ORB_Note_Kind := ORB_Note_Kind'First)
   is record
      case Kind is
         when A_TAP_AES =>
            TAP : Transport_Access_Point_Access;
            --  Factory of Transport_Endpoint components.

            Filter_Factory_Chain : Filters.Factory_Chain_Access;
            --  Factory of Filter (protocol stack) components.

         when A_TE_AES =>
            TE : Transport_Endpoint_Access;
            --  Transport_Endpoint component (connected to a
            --  protocol stack).
      end case;
   end record;

   type ORB_Note is new Note with record
      D : ORB_Note_Data;
   end record;

   procedure Handle_Event
     (ORB : access ORB_Type;
      AES : Asynchronous_Event_Source_Access);
   --  Process an event that occurred on AES.

   procedure Handle_Event
     (ORB : access ORB_Type;
      AES : Asynchronous_Event_Source_Access)
   is
      Note : ORB_Note;
   begin
      Get_Note (Notepad_Of (AES).all, Note);
      case Note.D.Kind is
         when A_TAP_AES =>
            declare
               New_TE     : Transport_Endpoint_Access;
               New_AES    : Asynchronous_Event_Source_Access;
               New_Filter : Filter_Access;
            begin
               Accept_Connection (Note.D.TAP.all, New_TE);
               --  Create transport endpoint.

               New_AES := Create_Event_Source (New_TE.all);
               --  Create associated asynchronous event source.

               New_Filter := Create_Filter_Chain
                 (Note.D.Filter_Factory_Chain);
               --  Create filter/protocol stack.

               Connect_Upper (New_TE, Component_Access (New_Filter));
               Connect_Lower (New_Filter, Component_Access (New_TE));
               --  Connect filter to transport.

               Set_Note (Notepad_Of (New_AES).all,
                         ORB_Note'(Annotations.Note with D =>
                                     (Kind   => A_TE_AES,
                                      TE     => New_TE)));
               --  Register link from AES to TE.

               Handle_New_Connection
                 (ORB.Tasking_Policy,
                  ORB_Access (ORB),
                  Active_Connection'(AES => New_AES, TE => New_TE));
               --  Assign execution resources to the newly-created connection.
            end;
         when A_TE_AES =>
            Emit (Component_Access (Note.D.TE),
                  Filters.Data_Units.Data_Indication'(null record));
      end case;
   end Handle_Event;

   procedure Run
     (ORB            : access ORB_Type;
      Exit_Condition : Exit_Condition_Access := null;
      May_Poll       : Boolean := False) is
   begin
      loop
         pragma Debug (O ("Run: enter loop."));
         Enter (ORB.ORB_Lock.all);

         if (Exit_Condition /= null and then Exit_Condition.all)
           or else ORB.Shutdown then
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

               ORB.Polling := True;
               Leave (ORB.ORB_Lock.all);

               if Monitors'Length = 1 then
                  Timeout := Droopi.Asynchronous_Events.Forever;
               else
                  Timeout := 0.0;
               end if;

               for I in Monitors'Range loop
                  declare
                     Events : constant AES_Array
                       := Check_Sources (Monitors (I), Timeout);
                  begin
                     if Events'Length > 0 then
                        Event_Happened := True;
                     end if;
                     for I in Events'Range loop
                        Handle_Event (ORB, Events (I));
                     end loop;
                  end;
               end loop;

               if not (Event_Happened or else Monitors'Length = 1) then
                  delay Poll_Interval;
               end if;

               Enter (ORB.ORB_Lock.all);
               ORB.Polling := False;
               if Event_Happened then
                  Update (ORB.Idle_Tasks);
               end if;
            end;

--  XXX remove.
--
--              declare
--                 Monitored_Set : constant Sock_Seqs.Element_Array
--                   := Sock_Seqs.To_Element_Array (ORB.ORB_Sockets);
--
--                 R_Set : Socket_Set_Type;
--                 W_Set : Socket_Set_Type;
--                 Status : Selector_Status;
--
--              begin
--                 ORB.Polling := True;
--                 Leave (ORB.ORB_Lock.all);
--
--                 for I in Monitored_Set'Range loop
--                    Set (R_Set, Monitored_Set (I).Socket);
--                    pragma Debug
--                      (O ("Monitoring socket"
--                          & Image (Monitored_Set (I).Socket)));
--                 end loop;
--                 Empty (W_Set);
--
--                 if ORB.Selector = null then
--                    ORB.Selector := new Selector_Type;
--                    pragma Assert (ORB.Selector /= null);
--                    Create_Selector (ORB.Selector.all);
--                 end if;
--
--                 pragma Debug (O ("Checking selector..."));
--                 Check_Selector
--                   (Selector     => ORB.Selector.all,
--                    R_Socket_Set => R_Set,
--                    W_Socket_Set => W_Set,
--                    Status       => Status);
--                 pragma Debug (O ("Selector returned status "
--                                  & Status'Img));
--
--                 Enter (ORB.ORB_Lock.all);
--                 ORB.Polling := False;
--
--                 for I in Monitored_Set'Range loop
--                    if Is_Set (R_Set, Monitored_Set (I).Socket) then
--                       Delete_Socket (ORB, Monitored_Set (I));
--                       pragma Debug
--                         (O ("Got event on socket"
--                             & Image (Monitored_Set (I).Socket)));
--                       declare
--                          J : constant Job_Access := new Socket_Ev_Job;
--                       begin
--                          Socket_Ev_Job (J.all).ORB := ORB_Access (ORB);
--                          Socket_Ev_Job (J.all).AS  := Monitored_Set (I);
--
--                          Queue_Job (ORB.Job_Queue, J);
--                       end;
--                    end if;
--                 end loop;
--                 Update (ORB.Idle_Tasks);
--                 --  Wake up any task that is waiting for something to do.
--                 Leave (ORB.ORB_Lock.all);
--              end;
         else

            --  This task is going idle.
            --  It should first ask for persmission to
            --  do so from the tasking policy object.

            --  XX TBD
            raise Not_Implemented;

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

   procedure Insert_Source
     (ORB : access ORB_Type;
      AES : Asynchronous_Event_Source_Access) is
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
               New_AEM : constant Asynchronous_Event_Monitor_Access
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
         Abort_Check_Sources (ORB.Selector.all);
      end if;
      Leave (ORB.ORB_Lock.all);
   end Insert_Source;

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : Asynchronous_Event_Source_Access) is
   begin
      Enter (ORB.ORB_Lock.all);

      Unregister_Source (AES);

      if ORB.Polling then
         Abort_Check_Sources (ORB.Selector.all);
      end if;
      --  XXX Destroy AES.
      --  XXX Destroy associated TE, associated TAP ?
      --  The requirement to destroy all associated resources
      --  would advocate the creation of a type "ORB Entity"
      --  catpuring the association of a set of resources,
      --  eg (AES + TAP) or (AES + TE).
      Leave (ORB.ORB_Lock.all);
   end Delete_Source;

   ----------------------------------
   -- Job type for object requests --
   ----------------------------------

   type Request_Job is new Job with record
      ORB : ORB_Access;
      Req : Request_Access;
   end record;

   procedure Run (J : access Request_Job);
   procedure Run (J : access Request_Job)
   is
      Res : Requests.Result;
   begin
      pragma Debug (O ("Run Request_Job: enter"));
      pragma Assert (J.Req /= null);
      Requests.Execute (J.Req.all, Res);
      pragma Debug (O ("Run Request_Job: executed request"));
      --  Execute request.

      --  Send_Result (Session, Result);
      --  Send back answer.

      Destroy (Res);
      --  Clear result.

      Destroy_Request (J.Req);
      --  Destroy request.
   exception
      when E : others =>
         pragma Debug (O ("Run Request_Job: Got exception "
                          & Ada.Exceptions.Exception_Information (E)));
         raise;

   end Run;

   procedure Queue_Job
     (ORB : access ORB_Type;
      J   : Droopi.Jobs.Job_Access) is
   begin
      Queue_Job (ORB.Job_Queue, J);
   end Queue_Job;

   procedure Queue_Request
     (ORB : access ORB_Type;
      R   : Droopi.Requests.Request_Access)
   is
      J : constant Job_Access := new Request_Job;
   begin
      pragma Debug (O ("Queue_Request: enter"));
      Request_Job (J.all).ORB := ORB_Access (ORB);
      Request_Job (J.all).Req := R;

      Queue_Job (ORB, J);
   end Queue_Request;

end Droopi.ORB;
