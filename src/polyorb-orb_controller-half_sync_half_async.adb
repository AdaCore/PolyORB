------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.ORB_CONTROLLER.HALF_SYNC_HALF_ASYNC                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

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
   begin
      pragma Assert (M = O.AEM_Infos (1).Monitor);

      --  Force all tasks currently waiting on event sources to abort

      if O.AEM_Infos (1).TI /= null
        and then State (O.AEM_Infos (1).TI.all) = Blocked
      then
         --  XXX Can we suppress the first test ?

         --  In this implementation, only one task may be blocked on
         --  event sources. We abort it.

         pragma Debug (O1 ("Disable_Polling: Aborting polling task"));
         PTI.Request_Abort_Polling (O.AEM_Infos (1).TI.all);
         PolyORB.Asynch_Ev.Abort_Check_Sources
           (Selector (O.AEM_Infos (1).TI.all).all);

         pragma Debug (O1 ("Disable_Polling: waiting abort is complete"));

         O.AEM_Infos (1).Polling_Abort_Counter
           := O.AEM_Infos (1).Polling_Abort_Counter + 1;

         Wait (O.AEM_Infos (1).Polling_Completed, O.ORB_Lock);

         O.AEM_Infos (1).Polling_Abort_Counter
           := O.AEM_Infos (1).Polling_Abort_Counter - 1;

         pragma Debug (O1 ("Disable_Polling: aborting done"));
      end if;
   end Disable_Polling;

   --------------------
   -- Enable_Polling --
   --------------------

   procedure Enable_Polling
     (O : access ORB_Controller_Half_Sync_Half_Async;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
   begin
      pragma Assert (M = O.AEM_Infos (1).Monitor);

      pragma Debug (O1 ("Enable_Polling: enter"));

      if O.AEM_Infos (1).Polling_Abort_Counter = 0
        and then O.Monitoring_Task_Idle
      then
         --  Awake monitoring task

         O.Monitoring_Task_Idle := False;
         pragma Debug (O1 ("Enable_Polling: awake monitoring task"));
         Signal (O.Monitoring_Task_CV);
      end if;
   end Enable_Polling;

   ------------------
   -- Notify_Event --
   ------------------

   procedure Notify_Event
     (O : access ORB_Controller_Half_Sync_Half_Async;
      E :        Event)
   is
      use type PAE.Asynch_Ev_Monitor_Access;
      use type PRS.Request_Scheduler_Access;
      use type PolyORB.Tasking.Threads.Thread_Id;

   begin
      pragma Debug (O1 ("Notify_Event: " & Event_Kind'Image (E.Kind)));

      case E.Kind is

         when End_Of_Check_Sources =>

            --  A task completed polling on a monitor

            O.Counters (Blocked) := O.Counters (Blocked) - 1;
            O.Counters (Unscheduled) := O.Counters (Unscheduled) + 1;
            pragma Assert (ORB_Controller_Counters_Valid (O));

            if O.AEM_Infos (1).Polling_Abort_Counter > 0 then

               --  This task has been aborted by one or more tasks, we
               --  broadcast them.
               Enter (O.Internal_ORB_Lock);
               Broadcast (O.AEM_Infos (1).Polling_Completed);
               Leave (O.Internal_ORB_Lock);
            end if;

         when Event_Sources_Added =>

            --  An AES has been added to monitored AES list

            if O.AEM_Infos (1).Monitor = null then

               --  There was no monitor registred yet, register new monitor

               O.AEM_Infos (1).Monitor := E.Add_In_Monitor;

            else
               --  Under this implementation, there can be at most one
               --  monitor. Ensure this assertion is correct.

               pragma Assert (E.Add_In_Monitor = O.AEM_Infos (1).Monitor);
               null;
            end if;

            if O.AEM_Infos (1).TI = null
              and then not O.AEM_Infos (1).Polling_Scheduled
              and then O.Monitoring_Task_Idle
            then

               --  No task is currently polling, awake monitoring task

               O.AEM_Infos (1).Polling_Scheduled := True;
               O.Monitoring_Task_Idle := False;

               Signal (O.Monitoring_Task_CV);
            end if;

         when Event_Sources_Deleted =>

            --  An AES has been removed from monitored AES list

            pragma Assert (O.AEM_Infos (1).Monitor /= null);
            null;

         when Job_Completed =>

            --  A task has completed the execution of a job

            O.Counters (Running) := O.Counters (Running) - 1;
            O.Counters (Unscheduled) := O.Counters (Unscheduled) + 1;
            pragma Assert (ORB_Controller_Counters_Valid (O));

         when ORB_Shutdown =>

            --  ORB shutdown has been requested

            O.Shutdown := True;

            --  Awake all idle tasks

            for J in 1 .. O.Counters (Idle) loop
               Awake_One_Idle_Task (O.Idle_Tasks);
            end loop;

            --  Unblock blocked tasks

            if O.AEM_Infos (1).TI /= null then

               PTI.Request_Abort_Polling (O.AEM_Infos (1).TI.all);
               PolyORB.Asynch_Ev.Abort_Check_Sources
                 (Selector (O.AEM_Infos (1).TI.all).all);

            end if;

         when Queue_Event_Job =>

            --  Queue event to monitoring job queue; the corresponding AES
            --  has been removed from its monitor.

            pragma Assert (E.By_Task = Id (O.AEM_Infos (1).TI.all));

            pragma Debug (O1 ("Job queued by monitoring task"));
            PJ.Queue_Job (O.Monitoring_Task_Job_Queue, E.Event_Job);

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

                  pragma Debug (O1 ("Queue Request_Job to default queue"));

                  O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs + 1;
                  PJ.Queue_Job (O.Job_Queue, E.Request_Job);
                  Try_Allocate_One_Task (O);
               end if;
            end;

         when Request_Result_Ready =>

            --  A Request has been completed and a response is
            --  available. We must forward it to requesting task. We
            --  ensure this task will stop its current action and ask
            --  for rescheduling.

            case State (E.Requesting_Task.all) is
               when Running =>

                  --  We cannot abort a running task. We let it
                  --  complete its job and ask for rescheduling.

                  null;

               when Blocked =>

                  --  We abort this task. It will then leave Blocked
                  --  state and ask for rescheduling.

                  declare
                     Sel : Asynch_Ev_Monitor_Access
                       renames Selector (E.Requesting_Task.all);

                  begin
                     pragma Debug (O1 ("About to abort block"));

                     pragma Assert (Sel /= null);
                     Abort_Check_Sources (Sel.all);

                     pragma Debug (O1 ("Aborted."));
                  end;

               when Idle =>

                  --  We awake this task. It will then leave Idle
                  --  state and ask for rescheduling.

                  pragma Debug (O1 ("Signal requesting task"));

                  Signal (Condition (E.Requesting_Task.all));

               when Terminated
                 | Unscheduled =>

                  --  Nothing to do.

                  null;
            end case;

         when Idle_Awake =>

            O.Counters (Idle) := O.Counters (Idle) - 1;
            O.Counters (Unscheduled) := O.Counters (Unscheduled) + 1;
            pragma Assert (ORB_Controller_Counters_Valid (O));

            --  A task has left Idle state

            Remove_Idle_Task (O.Idle_Tasks, E.Awakened_Task);

         when Task_Registered =>

            O.Registered_Tasks := O.Registered_Tasks + 1;
            O.Counters (Unscheduled) := O.Counters (Unscheduled) + 1;
            pragma Assert (ORB_Controller_Counters_Valid (O));

            if O.AEM_Infos (1).TI = null then

               --  The first registered task will monitor sources

               pragma Debug (O1 ("Registered monitoring task"));
               O.AEM_Infos (1).TI := E.Registered_Task;
            end if;

         when Task_Unregistered =>

            O.Counters (Terminated) := O.Counters (Terminated) - 1;
            O.Registered_Tasks := O.Registered_Tasks - 1;
            pragma Assert (ORB_Controller_Counters_Valid (O));

      end case;

      pragma Debug (O2 (Status (O)));
   end Notify_Event;

   -------------------
   -- Schedule_Task --
   -------------------

   procedure Schedule_Task
     (O  : access ORB_Controller_Half_Sync_Half_Async;
      TI :        PTI.Task_Info_Access)
   is
   begin
      pragma Debug (O1 ("Schedule_Task " & PTI.Image (TI.all) & ": enter"));

      pragma Assert (PTI.State (TI.all) = Unscheduled);

      --  Recompute TI status

      if Exit_Condition (TI.all)
        or else (O.Shutdown
                 and then O.Number_Of_Pending_Jobs = 0)
      then

         O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
         O.Counters (Terminated) := O.Counters (Terminated) + 1;
         pragma Assert (ORB_Controller_Counters_Valid (O));

         Set_State_Terminated (TI.all);

         pragma Debug (O1 ("Task is now terminated"));
         pragma Debug (O2 (Status (O)));

      else

         if TI = O.AEM_Infos (1).TI then
            --  Task is the monitoring task

            pragma Debug (O1 ("Scheduling monitor task"));

            if not PJ.Is_Empty (O.Monitoring_Task_Job_Queue) then
               --  Process event on the monitor

               O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
               O.Counters (Running) := O.Counters (Running) + 1;
               pragma Assert (ORB_Controller_Counters_Valid (O));

               Set_State_Running
                 (TI.all,
                  PJ.Fetch_Job (O.Monitoring_Task_Job_Queue));

            elsif O.AEM_Infos (1).Polling_Abort_Counter = 0
              and then O.AEM_Infos (1).Monitor /= null
              and then Has_Sources (O.AEM_Infos (1).Monitor.all)
            then
               --  Monitor

               O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
               O.Counters (Blocked) := O.Counters (Blocked) + 1;
               pragma Assert (ORB_Controller_Counters_Valid (O));

               O.AEM_Infos (1).Polling_Scheduled := False;

               Set_State_Blocked
                 (TI.all,
                  O.AEM_Infos (1).Monitor,
                  O.AEM_Infos (1).Polling_Timeout);

               pragma Debug (O1 ("Task is now blocked"));
               pragma Debug (O2 (Status (O)));

            else
               --  Go idle

               O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
               O.Counters (Idle) := O.Counters (Idle) + 1;
               pragma Assert (ORB_Controller_Counters_Valid (O));

               O.Monitoring_Task_Idle := True;

               pragma Debug (O1 ("Task is now idle"));
               pragma Debug (O2 (Status (O)));

               Set_State_Idle (TI.all, O.Monitoring_Task_CV, O.ORB_Lock);

            end if;

         else
            --  Task is a processing task

            if O.Number_Of_Pending_Jobs > 0 then

               O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
               O.Counters (Running) := O.Counters (Running) + 1;
               pragma Assert (ORB_Controller_Counters_Valid (O));

               O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs - 1;

               Set_State_Running (TI.all, PJ.Fetch_Job (O.Job_Queue));

               pragma Debug (O1 ("Task is now running a job"));
               pragma Debug (O2 (Status (O)));

            else
               O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
               O.Counters (Idle) := O.Counters (Idle) + 1;
               pragma Assert (ORB_Controller_Counters_Valid (O));

               Set_State_Idle
                 (TI.all,
                  Insert_Idle_Task (O.Idle_Tasks, TI),
                  O.ORB_Lock);

               pragma Debug (O1 ("Task is now idle"));
               pragma Debug (O2 (Status (O)));

            end if;
         end if;
      end if;
   end Schedule_Task;

   ------------
   -- Create --
   ------------

   function Create
     (OCF : access ORB_Controller_Half_Sync_Half_Async_Factory)
     return ORB_Controller_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (OCF);
      pragma Warnings (On);

      OC : ORB_Controller_Half_Sync_Half_Async_Access;
      RS : PRS.Request_Scheduler_Access;

   begin
      PRS.Create (RS);
      OC := new ORB_Controller_Half_Sync_Half_Async (RS);

      Create (OC.Internal_ORB_Lock);
      Create (OC.Monitoring_Task_CV);
      OC.Monitoring_Task_Job_Queue := PolyORB.Jobs.Create_Queue;

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
       Provides  => +"orb_controller",
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.ORB_Controller.Half_Sync_Half_Async;
