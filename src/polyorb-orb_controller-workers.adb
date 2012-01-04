------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B _ C O N T R O L L E R . W O R K E R S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
      AEM_Index : constant Natural := Index (O.all, M);

   begin
      --  Force all tasks currently waiting on this monitor to abort

      if O.AEM_Infos (AEM_Index).TI /= null then
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
     (O : access ORB_Controller_Workers;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      AEM_Index : constant Natural := Index (O.all, M);

   begin
      pragma Debug (C1, O1 ("Enable_Polling"));

      if O.AEM_Infos (AEM_Index).Polling_Abort_Counter = 0 then

         --  Allocate one task to poll on AES

         Try_Allocate_One_Task (O, Allow_Transient => True);
      end if;
   end Enable_Polling;

   ------------------
   -- Notify_Event --
   ------------------

   procedure Notify_Event
     (O : access ORB_Controller_Workers;
      E : Event)
   is
      use type PRS.Request_Scheduler_Access;

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
               then
                  --  No task is currently polling, allocate one

                  O.AEM_Infos (AEM_Index).Polling_Scheduled := True;
                  Try_Allocate_One_Task (O, Allow_Transient => True);
               end if;
            end;

         when Event_Sources_Deleted =>

            --  An AES has been removed from monitored AES list

            null;

         when Job_Completed =>

            --  A task has completed the execution of a job
            null;

         when ORB_Shutdown =>
            pragma Debug (C1, O1 ("ORB shutdown"));

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

            --  Queue event to main job queue

            pragma Debug (C1, O1 ("Queue Event_Job to default queue"));

            PJ.Queue_Job (O.Job_Queue, E.Event_Job);
            Try_Allocate_One_Task (O, Allow_Transient => True);

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
                  --  Note that if there is a blocked permanent task and no
                  --  idle or running permanent tasks, then we might need
                  --  to abort the block???
               end if;
            end;

         when Request_Result_Ready =>

            --  A Request has been completed and a response is available. We
            --  must forward it to requesting task. We ensure this task will
            --  stop its current action and ask for rescheduling.

            Reschedule_Task (O, E.Requesting_Task);

         when Idle_Awake =>

            --  A task has left Idle state

            Remove_Idle_Task (O.Idle_Tasks, E.Awakened_Task);

         when Task_Registered =>
            null;

         when Task_Unregistered =>
            if Need_Polling_Task (O) > 0 then
               Try_Allocate_One_Task (O, Allow_Transient => True);
               --  ??? Is this necessary? Won't this awake a task only to get
               --  it back to idle immediately?
            end if;

            Note_Task_Unregistered (O);
      end case;

      pragma Debug (C2, O2 (Status (O.all)));
   end Notify_Event;

   -------------------
   -- Schedule_Task --
   -------------------

   procedure Schedule_Task
     (O  : access ORB_Controller_Workers;
      TI : PTI.Task_Info_Access)
   is
      function Is_Schedulable (J : PJ.Job'Class) return Boolean;
      --  True if J is schedulable for this task (i.e. not an upcall job
      --  if the task is transient).

      --------------------
      -- Is_Schedulable --
      --------------------

      function Is_Schedulable (J : PJ.Job'Class) return Boolean is
      begin
         return TI.Kind = Permanent or else not Is_Upcall (J);
      end Is_Schedulable;

   --  Start of processing for Schedule_Task

   begin
      pragma Debug (C1, O1 ("Schedule_Task: enter " & Image (TI.all)));

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
         return;
      end if;

      declare
         use type PJ.Job_Access;
         Job : constant PJ.Job_Access :=
                 PJ.Fetch_Job (O.Job_Queue, Is_Schedulable'Access);
      begin
         if Job /= null then
            Set_State_Running (O.Summary, TI.all, Job);

            pragma Debug (C1, O1 ("Task is now running a job"));
            pragma Debug (C2, O2 (Status (O.all)));
            return;
         end if;
      end;

      declare
         AEM_Index : constant Natural := Need_Polling_Task (O);
      begin
         if AEM_Index > 0 then
            O.AEM_Infos (AEM_Index).Polling_Scheduled := False;
            O.AEM_Infos (AEM_Index).TI := TI;

            Set_State_Blocked
              (O.Summary,
               TI.all,
               O.AEM_Infos (AEM_Index).Monitor,
               O.AEM_Infos (AEM_Index).Polling_Timeout);

            pragma Debug (C1, O1 ("Task is now blocked on monitor"
                              & Natural'Image (AEM_Index)
                              & " " & Ada.Tags.External_Tag
                              (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));

            pragma Debug (C2, O2 (Status (O.all)));
            return;
         end if;
      end;

      Set_State_Idle
        (O.Summary,
         TI.all,
         Insert_Idle_Task (O.Idle_Tasks, TI),
         O.ORB_Lock);

      pragma Debug (C1, O1 ("Task is now idle"));
      pragma Debug (C2, O2 (Status (O.all)));
   end Schedule_Task;

   ------------
   -- Create --
   ------------

   function Create
     (OCF : ORB_Controller_Workers_Factory) return ORB_Controller_Access
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
       Provides  => +"orb_controller!",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.ORB_Controller.Workers;
