------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.ORB_CONTROLLER.HALF_SYNC_HALF_ASYNC                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2009, Free Software Foundation, Inc.          --
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

with Ada.Tags;

with PolyORB.Asynch_Ev;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.ORB_Controller.Half_Sync_Half_Async is

   use PolyORB.Asynch_Ev;
   use PolyORB.Task_Info;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;

   ---------------------
   -- Disable_Polling --
   ---------------------

   procedure Disable_Polling
     (O : access ORB_Controller_Half_Sync_Half_Async;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      AEM_Index : constant Natural := Index (O.all, M);

   begin
      --  Force all tasks currently waiting on event sources to abort

      if O.AEM_Infos (AEM_Index).TI /= null
        and then State (O.AEM_Infos (AEM_Index).TI.all) = Blocked
      then
         --  First condition is a guard for the case where no monitoring task
         --  has been registered yet for this AEM (can this actually happen???)

         --  Second condition handles the fact that the designated monitoring
         --  task may not be Blocked (can be Running while processing detected
         --  events).

         pragma Debug (C1, O1 ("Disable_Polling: Aborting polling task"));
         PTI.Request_Abort_Polling (O.AEM_Infos (AEM_Index).TI.all);
         PolyORB.Asynch_Ev.Abort_Check_Sources
           (Selector (O.AEM_Infos (AEM_Index).TI.all).all);

         pragma Debug (C1, O1 ("Disable_Polling: waiting abort is complete"));
         O.AEM_Infos (AEM_Index).Polling_Abort_Counter
           := O.AEM_Infos (AEM_Index).Polling_Abort_Counter + 1;

         Wait (O.AEM_Infos (AEM_Index).Polling_Completed, O.ORB_Lock);

         O.AEM_Infos (AEM_Index).Polling_Abort_Counter
           := O.AEM_Infos (AEM_Index).Polling_Abort_Counter - 1;

         pragma Debug (C1, O1 ("Disable_Polling: aborting done"));
      end if;
   end Disable_Polling;

   --------------------
   -- Enable_Polling --
   --------------------

   procedure Enable_Polling
     (O : access ORB_Controller_Half_Sync_Half_Async;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      AEM_Index : constant Natural := Index (O.all, M);

   begin
      pragma Debug (C1, O1 ("Enable_Polling: enter"));

      if O.AEM_Infos (AEM_Index).Polling_Abort_Counter = 0
        and then O.Monitoring_Tasks (AEM_Index).Idle then
         --  Awake monitoring task

         O.Monitoring_Tasks (AEM_Index).Idle := False;
         pragma Debug (C1, O1 ("Enable_Polling: awake monitoring task"));
         Signal (O.Monitoring_Tasks (AEM_Index).CV);
      end if;
   end Enable_Polling;

   ------------------
   -- Notify_Event --
   ------------------

   procedure Notify_Event
     (O : access ORB_Controller_Half_Sync_Half_Async;
      E :        Event)
   is
      use type PRS.Request_Scheduler_Access;
      use type PolyORB.Tasking.Threads.Thread_Id;

   begin
      pragma Debug (C1, O1 ("Notify_Event: " & Event_Kind'Image (E.Kind)));

      case E.Kind is

         when End_Of_Check_Sources =>
            declare
               AEM_Index : constant Natural := Index (O.all, E.On_Monitor);
            begin
               --  A task completed polling on a monitor

               pragma Debug (C1, O1 ("End of check sources on monitor #"
                                 & Natural'Image (AEM_Index)
                                 & Ada.Tags.External_Tag
                                 (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));

               if O.AEM_Infos (AEM_Index).Polling_Abort_Counter > 0 then
                  --  This task has been aborted by one or more tasks, we
                  --  broadcast them.

                  Broadcast (O.AEM_Infos (AEM_Index).Polling_Completed);
               end if;
            end;

         when Event_Sources_Added =>
            declare
               AEM_Index : Natural := Index (O.all, E.Add_In_Monitor);
            begin
               if AEM_Index = 0 then
                  --  This monitor was not yet registered, register it

                  pragma Debug (C1, O1 ("Adding new monitor"));

                  for J in O.AEM_Infos'Range loop
                     if O.AEM_Infos (J).Monitor = null then
                        O.AEM_Infos (J).Monitor := E.Add_In_Monitor;
                        AEM_Index := J;
                        exit;
                     end if;
                  end loop;
               end if;
               pragma Debug (C1, O1 ("Added monitor at index:" & AEM_Index'Img
                                 & " " & Ada.Tags.External_Tag
                                 (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));

               if O.AEM_Infos (AEM_Index).TI /= null
                 and then not O.AEM_Infos (AEM_Index).Polling_Scheduled
                 and then O.Monitoring_Tasks (AEM_Index).Idle
               then
                  --  No task is currently polling, allocate one

                  O.AEM_Infos (AEM_Index).Polling_Scheduled := True;

                  O.Monitoring_Tasks (AEM_Index).Idle := False;
                  Signal (O.Monitoring_Tasks (AEM_Index).CV);
               end if;
            end;

         when Event_Sources_Deleted =>
            null;

         when Job_Completed =>

            --  A task has completed the execution of a job

            null;

         when ORB_Shutdown =>

            --  ORB shutdown has been requested

            O.Shutdown := True;

            --  Awake all idle tasks

            Awake_All_Idle_Tasks (O.Idle_Tasks);

            --  Unblock blocked tasks

            for J in O.AEM_Infos'Range loop
               if O.AEM_Infos (J).TI /= null then
                  PTI.Request_Abort_Polling (O.AEM_Infos (J).TI.all);
                  PolyORB.Asynch_Ev.Abort_Check_Sources
                    (Selector (O.AEM_Infos (J).TI.all).all);
               end if;
            end loop;

         when Queue_Event_Job =>

            --  Queue event to monitoring job queue; the corresponding AES
            --  has been removed from its monitor.

            --  Inefficient to scan the Monitoring_Tasks array, this should
            --  be an annotation on the originating BO or AES???

            for J in O.Monitoring_Tasks'Range loop
               if E.By_Task = Id (O.AEM_Infos (J).TI.all) then

                  pragma Debug (C1, O1 ("Job queued by monitoring task"));
                  PJ.Queue_Job (O.Monitoring_Tasks (J).Job_Queue, E.Event_Job);
                  return;
               end if;
            end loop;

            --  Failure to queue event job denotes an abnormal situation, since
            --  by construction an associated monitoring task should have been
            --  established, associated with the binding object.

            raise Program_Error;

         when Queue_Request_Job =>
            declare
               Job_Queued : Boolean := False;
            begin
               if O.RS /= null then
                  Leave_ORB_Critical_Section (O);
                  Job_Queued := PRS.Try_Queue_Request_Job
                    (O.RS, E.Request_Job, E.Target);
                  Enter_ORB_Critical_Section (O);
               end if;

               if not Job_Queued then
                  --  Default: Queue request to main job queue

                  pragma Debug (C1, O1 ("Queue Request_Job to default queue"));

                  PJ.Queue_Job (O.Job_Queue, E.Request_Job);
                  Try_Allocate_One_Task
                    (O, Allow_Transient => not Is_Upcall (E.Request_Job.all));
                  --  We don't want the ORB to borrow a transient task to
                  --  make an upcall to application code, because this could
                  --  take a long time or even deadlock.
               end if;
            end;

         when Request_Result_Ready =>

            --  A Request has been completed and a response is available. We
            --  must forward it to requesting task. Ensure the requesting task
            --  is rescheduled now.

            Reschedule_Task (E.Requesting_Task.all);

         when Idle_Awake =>

            --  A task has left Idle state

            Remove_Idle_Task (O.Idle_Tasks, E.Awakened_Task);

         when Task_Registered =>

            --  The O.AEM_Infos'Length first registered tasks will poll
            --  the corresponding event monitors.

            for J in O.AEM_Infos'Range loop
               if O.AEM_Infos (J).TI = null then
                  pragma Debug (C1, O1 ("Registered monitoring task"));
                  O.AEM_Infos (J).TI := E.Registered_Task;
                  pragma Assert (E.Registered_Task.Kind = Permanent);

                  --  Prevent task from terminating when going idle

                  Set_May_Exit (E.Registered_Task.all, May_Exit => False);
               end if;
            end loop;

         when Task_Unregistered =>
            for J in O.AEM_Infos'Range loop
               if O.AEM_Infos (J).TI = E.Unregistered_Task then
                  --  Unregistering one of the designated monitoring tasks
                  --  (happens during partition termination).

                  O.AEM_Infos (J).TI := null;
               end if;
            end loop;

            Note_Task_Unregistered (O);
      end case;

      pragma Debug (C2, O2 (Status (O.all)));
   end Notify_Event;

   -------------------
   -- Schedule_Task --
   -------------------

   procedure Schedule_Task
     (O  : access ORB_Controller_Half_Sync_Half_Async;
      TI : PTI.Task_Info_Access)
   is
      AEM_Index : Natural := 0;
   begin
      pragma Debug (C1, O1 ("Schedule_Task "
                    & PTI.Image (TI.all) & ": enter"));

      if State (TI.all) = Terminated then
         pragma Debug (C1, O1 ("Schedule_Task: task is terminated"));
         return;
      end if;

      Set_State_Unscheduled (O.Summary, TI.all);

      --  Recompute TI status

      if Exit_Condition (TI.all)
        or else (O.Shutdown
                 and then not Has_Pending_Job (O)
                 and then TI.Kind = Permanent)
      then
         Set_State_Terminated (O.Summary, TI.all);

         pragma Debug (C1, O1 ("Task is now terminated"));
         pragma Debug (C2, O2 (Status (O.all)));

      else
         for J in O.AEM_Infos'Range loop
            if TI = O.AEM_Infos (J).TI then
               AEM_Index := J;
               exit;
            end if;
         end loop;

         if AEM_Index > 0 then
            --  Task is the monitoring task

            pragma Debug (C1, O1 ("Scheduling monitoring task"));

            if not PJ.Is_Empty (O.Monitoring_Tasks (AEM_Index).Job_Queue) then
               --  Process event on the monitor

               Set_State_Running
                 (O.Summary,
                  TI.all,
                  PJ.Fetch_Job (O.Monitoring_Tasks (AEM_Index).Job_Queue));

            elsif O.AEM_Infos (AEM_Index).Polling_Abort_Counter = 0
              and then O.AEM_Infos (AEM_Index).Monitor /= null
              and then Has_Sources (O.AEM_Infos (AEM_Index).Monitor.all)
            then
               --  Monitor

               O.AEM_Infos (AEM_Index).Polling_Scheduled := False;

               Set_State_Blocked
                 (O.Summary,
                  TI.all,
                  O.AEM_Infos (AEM_Index).Monitor,
                  O.AEM_Infos (AEM_Index).Polling_Timeout);

               pragma Debug (C1, O1 ("Task is now blocked"));
               pragma Debug (C2, O2 (Status (O.all)));

            else
               --  Go idle

               O.Monitoring_Tasks (AEM_Index).Idle := True;

               pragma Debug (C1, O1 ("Task is now idle"));
               pragma Debug (C2, O2 (Status (O.all)));

               Set_State_Idle
                 (O.Summary,
                  TI.all,
                  O.Monitoring_Tasks (AEM_Index).CV, O.ORB_Lock);
            end if;

         else
            --  Task is a processing task

            if Has_Pending_Job (O) then
               --  Case of the pending job being an upcall when the current
               --  task is transient???

               Set_State_Running
                 (O.Summary, TI.all, PJ.Fetch_Job (O.Job_Queue));

               pragma Debug (C1, O1 ("Task is now running a job"));
               pragma Debug (C2, O2 (Status (O.all)));

            else
               Set_State_Idle
                 (O.Summary,
                  TI.all,
                  Insert_Idle_Task (O.Idle_Tasks, TI),
                  O.ORB_Lock);

               pragma Debug (C1, O1 ("Task is now idle"));
               pragma Debug (C2, O2 (Status (O.all)));

            end if;
         end if;
      end if;
   end Schedule_Task;

   ------------
   -- Create --
   ------------

   function Create
     (OCF : ORB_Controller_Half_Sync_Half_Async_Factory)
      return ORB_Controller_Access
   is
      pragma Unreferenced (OCF);
      OC : ORB_Controller_Half_Sync_Half_Async_Access;
      RS : PRS.Request_Scheduler_Access;

   begin
      PRS.Create (RS);
      OC := new ORB_Controller_Half_Sync_Half_Async (RS);

      for J in OC.Monitoring_Tasks'Range loop
         Create (OC.Monitoring_Tasks (J).CV);
         OC.Monitoring_Tasks (J).Job_Queue := PolyORB.Jobs.Create_Queue;
      end loop;

      Initialize (ORB_Controller (OC.all));

      return ORB_Controller_Access (OC);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_ORB_Controller_Factory (OCF);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb_controller.half_sync_half_async",
       Conflicts => +"orb.no_tasking",
       Depends   => +"tasking.condition_variables"
         & "tasking.mutexes"
         & "request_scheduler?",
       Provides  => +"orb_controller!",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.ORB_Controller.Half_Sync_Half_Async;
