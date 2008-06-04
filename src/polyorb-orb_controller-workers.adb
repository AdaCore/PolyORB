------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B _ C O N T R O L L E R . W O R K E R S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

package body PolyORB.ORB_Controller.Workers is

   use PolyORB.Asynch_Ev;
   use PolyORB.Task_Info;
   use PolyORB.Tasking.Condition_Variables;

   ---------------------
   -- Disable_Polling --
   ---------------------

   procedure Disable_Polling
     (O : access ORB_Controller_Workers;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      AEM_Index : constant Natural := Index (O, M);

   begin
      --  Force all tasks currently waiting on this monitor to abort

      if O.AEM_Infos (AEM_Index).TI /= null then
         pragma Debug (O1 ("Disable_Polling: Aborting polling task"));
         PTI.Request_Abort_Polling (O.AEM_Infos (AEM_Index).TI.all);
         PolyORB.Asynch_Ev.Abort_Check_Sources
           (Selector (O.AEM_Infos (AEM_Index).TI.all).all);

         pragma Debug (O1 ("Disable_Polling: waiting abort is complete"));
         O.AEM_Infos (AEM_Index).Polling_Abort_Counter
           := O.AEM_Infos (AEM_Index).Polling_Abort_Counter + 1;

         Wait (O.AEM_Infos (AEM_Index).Polling_Completed, O.ORB_Lock);

         O.AEM_Infos (AEM_Index).Polling_Abort_Counter
           := O.AEM_Infos (AEM_Index).Polling_Abort_Counter - 1;

         pragma Debug (O1 ("Disable_Polling: aborting done"));
      end if;
   end Disable_Polling;

   --------------------
   -- Enable_Polling --
   --------------------

   procedure Enable_Polling
     (O : access ORB_Controller_Workers;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      AEM_Index : constant Natural := Index (O, M);

   begin
      pragma Debug (O1 ("Enable_Polling"));

      if O.AEM_Infos (AEM_Index).Polling_Abort_Counter = 0 then

         --  Allocate one task to poll on AES

         Try_Allocate_One_Task (O);
      end if;
   end Enable_Polling;

   ------------------
   -- Notify_Event --
   ------------------

   procedure Notify_Event
     (O : access ORB_Controller_Workers;
      E :        Event)
   is
      use type PRS.Request_Scheduler_Access;

   begin
      pragma Debug (O1 ("Notify_Event: " & Event_Kind'Image (E.Kind)));

      case E.Kind is

         when End_Of_Check_Sources =>
            declare
               AEM_Index : constant Natural := Index (O, E.On_Monitor);
            begin
               --  A task completed polling on a monitor

               pragma Debug (O1 ("End of check sources on monitor #"
                                 & Natural'Image (AEM_Index)
                                 & Ada.Tags.External_Tag
                                 (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));

               O.Counters (Blocked) := O.Counters (Blocked) - 1;
               O.Counters (Unscheduled) := O.Counters (Unscheduled) + 1;
               pragma Assert (ORB_Controller_Counters_Valid (O));

               --  Reset TI

               O.AEM_Infos (AEM_Index).TI := null;

               if O.AEM_Infos (AEM_Index).Polling_Abort_Counter > 0 then

                  --  This task has been aborted by one or more tasks,
                  --  we broadcast them.

                  Broadcast (O.AEM_Infos (AEM_Index).Polling_Completed);
               end if;
            end;

         when Event_Sources_Added =>
            declare
               AEM_Index : Natural := Index (O, E.Add_In_Monitor);
            begin
               if AEM_Index = 0 then
                  --  This monitor was not yet registered, register it
                  pragma Debug (O1 ("Adding new monitor"));

                  for J in O.AEM_Infos'Range loop
                     if O.AEM_Infos (J).Monitor = null then
                        O.AEM_Infos (J).Monitor := E.Add_In_Monitor;
                        AEM_Index := J;
                        exit;
                     end if;
                  end loop;
               end if;
               pragma Debug (O1 ("Added monitor at index:" & AEM_Index'Img
                                 & " " & Ada.Tags.External_Tag
                                 (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));

               if O.AEM_Infos (AEM_Index).TI /= null
                 and then not O.AEM_Infos (AEM_Index).Polling_Scheduled
               then
                  --  No task is currently polling, allocate one

                  O.AEM_Infos (AEM_Index).Polling_Scheduled := True;
                  Try_Allocate_One_Task (O);
               end if;
            end;

         when Event_Sources_Deleted =>

            --  An AES has been removed from monitored AES list

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

            for J in O.AEM_Infos'Range loop
               if O.AEM_Infos (J).TI /= null then
                  PTI.Request_Abort_Polling (O.AEM_Infos (J).TI.all);
                  PolyORB.Asynch_Ev.Abort_Check_Sources
                    (Selector (O.AEM_Infos (J).TI.all).all);
               end if;
            end loop;

         when Queue_Event_Job =>

            --  Queue event to main job queue

            pragma Debug (O1 ("Queue Event_Job to default queue"));

            O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs + 1;
            PJ.Queue_Job (O.Job_Queue, E.Event_Job);
            Try_Allocate_One_Task (O);

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

                  --  Nothing to do
                  --  XXX hummm, does it make sense to have these states ?

                  null;
            end case;

         when Idle_Awake =>

            --  Update Scheduler status

            O.Counters (Idle) := O.Counters (Idle) - 1;
            O.Counters (Unscheduled) := O.Counters (Unscheduled) + 1;
            pragma Assert (ORB_Controller_Counters_Valid (O));

            --  A task has left Idle state

            Remove_Idle_Task (O.Idle_Tasks, E.Awakened_Task);

         when Task_Registered =>

            O.Registered_Tasks := O.Registered_Tasks + 1;
            O.Counters (Unscheduled) := O.Counters (Unscheduled) + 1;
            pragma Assert (ORB_Controller_Counters_Valid (O));

         when Task_Unregistered =>

            O.Counters (Terminated) := O.Counters (Terminated) - 1;
            O.Registered_Tasks := O.Registered_Tasks - 1;
            pragma Assert (ORB_Controller_Counters_Valid (O));

            if Need_Polling_Task (O) > 0
              and then O.Counters (Unscheduled) = 0
            then
               Try_Allocate_One_Task (O);
            end if;

            Note_Task_Unregistered (O);
      end case;

      pragma Debug (O2 (Status (O)));
   end Notify_Event;

   -------------------
   -- Schedule_Task --
   -------------------

   procedure Schedule_Task
     (O  : access ORB_Controller_Workers;
      TI :        PTI.Task_Info_Access)
   is
   begin
      pragma Debug (O1 ("Schedule_Task: enter " & Image (TI.all)));

      pragma Assert (PTI.State (TI.all) = Unscheduled);

      --  Recompute TI status

      if Exit_Condition (TI.all)
        or else (O.Shutdown
                 and then O.Number_Of_Pending_Jobs = 0
                 and then TI.Kind = Permanent)
      then

         O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
         O.Counters (Terminated) := O.Counters (Terminated) + 1;
         pragma Assert (ORB_Controller_Counters_Valid (O));

         Set_State_Terminated (TI.all);

         pragma Debug (O1 ("Task is now terminated"));
         pragma Debug (O2 (Status (O)));

      elsif O.Number_Of_Pending_Jobs > 0 then

         O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
         O.Counters (Running) := O.Counters (Running) + 1;
         pragma Assert (ORB_Controller_Counters_Valid (O));

         O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs - 1;

         Set_State_Running (TI.all, PJ.Fetch_Job (O.Job_Queue));

         pragma Debug (O1 ("Task is now running a job"));
         pragma Debug (O2 (Status (O)));

      elsif May_Poll (TI.all) then
         declare
            AEM_Index : constant Natural := Need_Polling_Task (O);
         begin
            if AEM_Index > 0 then
               O.Counters (Unscheduled) := O.Counters (Unscheduled) - 1;
               O.Counters (Blocked) := O.Counters (Blocked) + 1;
               pragma Assert (ORB_Controller_Counters_Valid (O));

               O.AEM_Infos (AEM_Index).Polling_Scheduled := False;
               O.AEM_Infos (AEM_Index).TI := TI;

               Set_State_Blocked
                 (TI.all,
                  O.AEM_Infos (AEM_Index).Monitor,
                  O.AEM_Infos (AEM_Index).Polling_Timeout);

               pragma Debug (O1 ("Task is now blocked on monitor"
                                 & Natural'Image (AEM_Index)
                                 & " " & Ada.Tags.External_Tag
                                 (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));

               pragma Debug (O2 (Status (O)));
            end if;
         end;
      end if;

      if PTI.State (TI.all) = Unscheduled then
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
   end Schedule_Task;

   ------------
   -- Create --
   ------------

   function Create
     (OCF : access ORB_Controller_Workers_Factory)
     return ORB_Controller_Access
   is
      pragma Unreferenced (OCF);

      OC : ORB_Controller_Workers_Access;
      RS : PRS.Request_Scheduler_Access;

   begin
      PRS.Create (RS);
      OC := new ORB_Controller_Workers (RS);

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
      (Name      => +"orb_controller.workers",
       Conflicts => Empty,
       Depends   => +"tasking.condition_variables"
       & "tasking.mutexes"
       & "request_scheduler?",
       Provides  => +"orb_controller",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.ORB_Controller.Workers;
