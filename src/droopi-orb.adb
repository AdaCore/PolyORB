--  $Id$

with Ada.Exceptions;

with Droopi.Filters.Sockets;
with Droopi.Log;
with Droopi.Soft_Links;

package body Droopi.ORB is

   use Droopi.Asynchronous_Events;
   use Droopi.Filters;
   use Droopi.Jobs;
   use Droopi.Log;
   use Droopi.Requests;
   use Droopi.Soft_Links;

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
      Create (ORB.Idle_Tasks);

      ORB.Job_Queue := Droopi.Jobs.Create_Queue;
      ORB.Shutdown := False;
      ORB.Polling  := False;

      return ORB;
   end Create_ORB;

   procedure Start (ORB : access ORB_Type);

   procedure Start (ORB : access ORB_Type) is
   begin
      --  Start accepting incoming connections.
      --  XXX TBD
      raise Not_Implemented;
   end Start;

   function Try_Perform_Work (Q : access Job_Queue) return Boolean;
   --  Perform one item of work from Q, if available.
   --  Precondition: This function must be called from within a
   --    critical section.
   --  Postcondition: if a job has been executed, then the critical
   --    section has been left, and True is returned.
   --    If no job was available, the critical section is not left,
   --    and False is returned.

   function Try_Perform_Work (Q : access Job_Queue) return Boolean is
   begin
      if not Empty (Q) then
         declare
            Job : Job_Access := Fetch_Job (Q);
         begin
            Leave_Critical_Section;

            pragma Assert (Job /= null);
            Run (Job);
            Free (Job);
            return True;
         end;
      else
         return False;
      end if;
   end Try_Perform_Work;

   procedure Run
     (ORB            : access ORB_Type;
      Exit_Condition : Exit_Condition_Access := null;
      May_Poll       : Boolean := False) is
   begin
      loop
         pragma Debug (O ("Run: enter."));
         Enter_Critical_Section;

         if (Exit_Condition /= null and then Exit_Condition.all)
           or else ORB.Shutdown then
            Leave_Critical_Section;
            exit;
         end if;

         if Try_Perform_Work (ORB.Job_Queue) then
            null;
         elsif May_Poll then

            declare
               Monitors : constant Monitor_Seqs.Element_Array
                 := Monitor_Seqs.To_Element_Array (ORB.Monitors);

               Poll_Interval : constant Duration := 0.1;
               --  XXX Poll_Interval should be configurable.

               Timeout : Duration;

               Event_Happened : Boolean := False;

            begin

               ORB.Polling := True;
               Leave_Critical_Section;

               if Monitors'Length = 1 then
                  Timeout := Droopi.Asynchronous_Events.Forever;
               else
                  Timeout := 0.0;
               end if;

               for I in Monitors'Range loop
                  declare
                     Work : constant Job_Array
                       := Check_Sources (Monitors (I).all, Timeout);
                  begin
                     if Work'Length > 0 then
                        Event_Happened := True;
                     end if;
                     for I in Work'Range loop
                        Queue_Job (ORB, Work (I));
                     end loop;
                  end;
               end loop;

               if not (Event_Happened or else Monitors'Length = 1) then
                  delay Poll_Interval;
               end if;

               Enter_Critical_Section;
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
--                 Leave_Critical_Section;
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
--                 Enter_Critical_Section;
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
--                 Leave_Critical_Section;
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
      Enter_Critical_Section;
      Result := not Empty (ORB.Job_Queue);
      Leave_Critical_Section;
      return Result;
   end Work_Pending;

   procedure Perform_Work (ORB : access ORB_Type) is
   begin
      Enter_Critical_Section;
      if not Try_Perform_Work (ORB.Job_Queue) then
         Leave_Critical_Section;
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

      Enter_Critical_Section;
      ORB.Shutdown := True;
      Leave_Critical_Section;

   end Shutdown;

   procedure Insert_Socket
     (ORB : access ORB_Type;
      AS  : Active_Socket) is
   begin
--        pragma Assert (AS.Kind /= Invalid_Sk);
--
--        Enter_Critical_Section;
--        Sock_Seqs.Append (ORB.ORB_Sockets, AS);
--
--        if ORB.Polling then
--           Abort_Selector (ORB.Selector.all);
--        end if;
--        Leave_Critical_Section;
      null;
   end Insert_Socket;

   procedure Delete_Socket
     (ORB : access ORB_Type;
      AS  : Active_Socket)
   is
      Deleted : Boolean := False;
   begin
--        Enter_Critical_Section;
--
--        declare
--           Sockets : constant Sock_Seqs.Element_Array
--             := Sock_Seqs.To_Element_Array (ORB.ORB_Sockets);
--        begin
--           All_Sockets :
--           for I in Sockets'Range loop
--              if Sockets (I) = AS then
--                 Sock_Seqs.Delete
--                   (Source  => ORB.ORB_Sockets,
--                    From    => 1 + I - Sockets'First,
--                    Through => 1 + I - Sockets'First);
--                 Deleted := True;
--                 exit All_Sockets;
--              end if;
--           end loop All_Sockets;
--
--           pragma Assert (Deleted);
--        end;
--
--        if ORB.Polling then
--           Abort_Selector (ORB.Selector.all);
--        end if;
--        Leave_Critical_Section;
      null;
   end Delete_Socket;

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
