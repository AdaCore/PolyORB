------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . S C H E D U L E R                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Asynch_Ev;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.Scheduler is

   use PolyORB.Log;
   use PolyORB.Task_Info;
   use PolyORB.Tasking.Condition_Variables;

   package L is new PolyORB.Log.Facility_Log ("polyorb.scheduler");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   package L2 is new PolyORB.Log.Facility_Log ("polyorb.scheduler_status");
   procedure O2 (Message : in String; Level : Log_Level := Debug)
     renames L2.Output;

   ------------------------------
   -- Management of Idle tasks --
   ------------------------------

   --  Under this scheduler implementation, several tasks may go
   --  idle. Each idle task waits on a specific condition variable.

   package CV_Lists is new PolyORB.Utils.Chained_Lists (Condition_Access);

   Used_CV : CV_Lists.List;
   --  Used_CV is the list of CV used by some tasks when they are idle.
   --  Signaling one of these CV will awake the corresponding idle task.

   Free_CV : CV_Lists.List;
   --  Free_CV is the list of pre-allocated CV. When scheduling a task
   --  to idle state, the scheduler first looks for an availble CV in
   --  this list; or else allocates one new CV. When a task leaves
   --  idle state, the scheduler puts its CV in Free_CV.

   function Allocate_CV return Condition_Access;
   --  Return one CV.

   procedure Awake_One_Idle_Task;
   --  Awake one idle task, if any. Else raise Program_Error.

   ---------------------------------
   -- Management of Blocked Tasks --
   ---------------------------------

   --  Under this scheduler implementation, at most on task may enter
   --  blocked state. We store its Task_Info for later use.

   Blocked_Task_Info : PTI.Task_Info_Access;

   -------------------------------------------
   -- Scheduler internal status information --
   -------------------------------------------

   Registered_Tasks  : Natural := 0;
   Unscheduled_Tasks : Natural := 0;
   Idle_Tasks        : Natural := 0;
   Running_Tasks     : Natural := 0;
   Blocked_Tasks     : Natural := 0;

   Number_Of_Pending_Jobs : Natural := 0;
   Number_Of_AES          : Natural := 0;

   Shutdown : Boolean := False;

   Polling_Scheduled     : Boolean := False;

   Polling_Abort_Counter : Natural := 0;
   --  Indicates number of tasks that requested abortion of polling.

   Polling_Completed     : Condition_Access;
   --  This condition is signalled after polling is completed. It
   --  is used by tasks for the polling task to release any
   --  reference to source list that is to be modified.

   Scheduler_Mutex :        PTM.Mutex_Access;

   function Status return String;
   --  Output status of task running Broker, for debug purposes.

   procedure Try_Allocate_One_Task;

   ------------
   -- Status --
   ------------

   function Status return String is
   begin
      return "Tot: " & Natural'Image (Registered_Tasks)
        & " U: " & Natural'Image (Unscheduled_Tasks)
        & " R:" & Natural'Image (Running_Tasks)
        & " B: " & Natural'Image (Blocked_Tasks)
        & " I: " & Natural'Image (Idle_Tasks)
        & "| PJ: " & Natural'Image (Number_Of_Pending_Jobs);
   end Status;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (S     : access Scheduling_Policy;
      Mutex :        PTM.Mutex_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnings (On);

   begin
      Scheduler_Mutex := Mutex;
   end Initialize;

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task
     (S  : access Scheduling_Policy;
      TI :        PTI.Task_Info)
   is
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Unreferenced (TI);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Register_Task: enter"));

      Registered_Tasks := Registered_Tasks + 1;
      Unscheduled_Tasks := Unscheduled_Tasks + 1;

      pragma Debug (O ("Register_Task: leave"));
      pragma Debug (O2 (Status));
   end Register_Task;

   ---------------------
   -- Disable_Polling --
   ---------------------

   procedure Disable_Polling (S : access Scheduling_Policy)
   is
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnigns (On);

   begin

      --  Prevent all tasks to poll.

      Polling_Abort_Counter := Polling_Abort_Counter + 1;

      --  Force all tasks currently waiting on event sources to abort

      if Blocked_Tasks > 0 then

         --  In this implementation, only one task may be blocked on
         --  event sources. We abort it.

         pragma Debug (O ("Disable_Polling: Aborting polling task"));
         PTI.Request_Abort_Polling (Blocked_Task_Info.all);
         PolyORB.Asynch_Ev.Abort_Check_Sources
           (Selector (Blocked_Task_Info.all).all);

         pragma Debug (O ("Disable_Polling: waiting abort is complete"));
         Wait (Polling_Completed, Scheduler_Mutex);

         pragma Debug (O ("Disable_Polling: aborting done"));
      end if;
   end Disable_Polling;

   --------------------
   -- Enable_Polling --
   --------------------

   procedure Enable_Polling (S : access Scheduling_Policy)
   is
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnigns (On);

   begin

      Polling_Abort_Counter := Polling_Abort_Counter - 1;

      if Polling_Abort_Counter = 0 then

         --  Allocate one task to poll on AES

         Try_Allocate_One_Task;
      end if;
   end Enable_Polling;

   ------------------
   -- Notify_Event --
   ------------------

   procedure Notify_Event
     (S : access Scheduling_Policy;
      E :        Event)
   is
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Notify_Event: " & Event_Kind'Image (E.Kind)));

      case E.Kind is

         when End_Of_Check_Sources =>

            --  A task completed polling on a monitor

            Blocked_Tasks := Blocked_Tasks - 1;
            Blocked_Task_Info := null;
            Unscheduled_Tasks := Unscheduled_Tasks + 1;

            if Polling_Abort_Counter > 0 then

               --  This task has been aborted by one or more tasks, we
               --  broadcast them.

               Broadcast (Polling_Completed);
            end if;

         when Event_Sources_Added =>

            --  An AES has been added to monitored AES list

            Number_Of_AES := Number_Of_AES + 1;

            if Blocked_Tasks = 0
              and then not Polling_Scheduled then

               --  No task is currently polling, allocate one.

               Polling_Scheduled := True;
               Try_Allocate_One_Task;

            end if;

         when Event_Sources_Deleted =>

            --  An AES has been removed from monitored AES list

            Number_Of_AES := Number_Of_AES - 1;

         when Executing_Job =>

            --  A task is executing a job

            Number_Of_Pending_Jobs := Number_Of_Pending_Jobs - 1;

         when Job_Completed =>

            --  A task has completed the execution of a job

            Running_Tasks := Running_Tasks - 1;
            Unscheduled_Tasks := Unscheduled_Tasks + 1;

         when Job_Queued =>

            --  A job has been queued

            Number_Of_Pending_Jobs := Number_Of_Pending_Jobs + 1;
            Try_Allocate_One_Task;

         when ORB_Shutdown =>

            --  ORB shutdiwn has been requested

            Shutdown := True;

            --  Awake all idle tasks

            for J in 1 .. Idle_Tasks loop
               Awake_One_Idle_Task;
            end loop;

            --  Unblock blocked tasks

            if Blocked_Tasks > 0 then

               PTI.Request_Abort_Polling (Blocked_Task_Info.all);
               PolyORB.Asynch_Ev.Abort_Check_Sources
                 (Selector (Blocked_Task_Info.all).all);

            end if;

         when Request_Result_Ready =>

            --  A Request has been completed and a resonse is
            --  available. We must forward it to requesting task. We
            --  ensure this task will stop its current action and ask
            --  for rescheduling.

            case State (E.TI.all) is
               when Running =>

                  --  We cannot abort a running task. We let it
                  --  complete its job and ask for rescheduling.

                  null;

               when Blocked =>

                  --  We abort this task. It will then leave Blocked
                  --  state and ask for rescheduling.

                  declare
                     use PolyORB.Asynch_Ev;

                     Sel : constant Asynch_Ev_Monitor_Access :=
                       Selector (E.TI.all);
                  begin
                     pragma Debug (O ("About to abort block"));

                     Blocked_Tasks := Blocked_Tasks - 1;
                     Unscheduled_Tasks := Unscheduled_Tasks + 1;

                     pragma Assert (Sel /= null);
                     Abort_Check_Sources (Sel.all);
                     pragma Debug (O ("Aborted."));
                  end;

               when Idle =>

                  --  We awake this task. It will then leave Idle
                  --  state and ask for rescheduling.

                  pragma Debug (O ("Signal requesting task"));

                  Idle_Tasks := Idle_Tasks - 1;
                  Unscheduled_Tasks := Unscheduled_Tasks + 1;
                  Signal (Condition (E.TI.all));

               when Terminated | Unscheduled =>

                  --  Nothing to do.

                  null;

            end case;

      end case;

      pragma Debug (O2 (Status));
   end Notify_Event;

   -------------------
   -- Schedule_Task --
   -------------------

   function Schedule_Task
     (S  : access Scheduling_Policy;
      TI : access PTI.Task_Info)
     return PTI.Task_State
   is
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Schedule_Task: enter"));

      pragma Assert (PTI.State (TI.all) = Unscheduled);

      --  Update counters

      Unscheduled_Tasks := Unscheduled_Tasks - 1;

      --  Recompute TI status

      if Exit_Condition (TI.all) or else Shutdown then

         pragma Debug (O ("Task is now terminated"));
         pragma Debug (O2 (Status));

         return PTI.Terminated;

      elsif Number_Of_Pending_Jobs > 0 then

         Running_Tasks := Running_Tasks + 1;

         pragma Debug (O ("Task is now running a job"));
         pragma Debug (O2 (Status));

         return PTI.Running;

      elsif May_Poll (TI.all)
        and then Number_Of_AES > 0
        and then Polling_Abort_Counter = 0
        and then Blocked_Tasks = 0
      then

         Blocked_Tasks := Blocked_Tasks + 1;
         Polling_Scheduled := False;

         Blocked_Task_Info := Task_Info_Access (TI);

         pragma Debug (O ("Task is now blocked"));
         pragma Debug (O2 (Status));

         return PTI.Blocked;

      else

         Idle_Tasks := Idle_Tasks + 1;
         Set_State_Idle (TI.all, Allocate_CV, Scheduler_Mutex);

         pragma Debug (O ("Task is now idle"));
         pragma Debug (O2 (Status));

         return PTI.Idle;

      end if;
   end Schedule_Task;

   ---------------------
   -- Unregister_Task --
   ---------------------

   procedure Unregister_Task
     (S  : access Scheduling_Policy;
      TI :        PTI.Task_Info)
   is
      pragma Warnings (Off);
      pragma Unreferenced (S);
      pragma Unreferenced (TI);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Unregister_Task: enter"));

      Registered_Tasks := Registered_Tasks - 1;

      pragma Debug (O2 (Status));
      pragma Debug (O ("Unregister_Task: leave"));
   end Unregister_Task;

   -----------------
   -- Allocate_CV --
   -----------------

   function Allocate_CV return Condition_Access
   is
      Result : Condition_Access;

   begin
      if CV_Lists.Length (Free_CV) > 0 then

         --  Use an existing CV, from Free_CV list

         CV_Lists.Extract_First (Free_CV, Result);
      else

         --  else allocate a new one

         Create (Result);
      end if;

      CV_Lists.Append (Used_CV, Result);
      return Result;
   end Allocate_CV;

   -------------------------
   -- Awake_One_Idle_Task --
   -------------------------

   procedure Awake_One_Idle_Task
   is
      Idle_Task_CV : Condition_Access;

   begin
      if Idle_Tasks > 0 then
         pragma Debug (O ("Awake one idle task"));

         --  Modify Scheduler status

         Idle_Tasks := Idle_Tasks - 1;
         Unscheduled_Tasks := Unscheduled_Tasks + 1;

         --  Signal one idle task, and puts its CV in Free_CV

         CV_Lists.Extract_First (Used_CV, Idle_Task_CV);
         Signal (Idle_Task_CV);
         CV_Lists.Append (Free_CV, Idle_Task_CV);

      else
         pragma Debug (O ("No idle task !"));
         raise Program_Error;

      end if;
   end Awake_One_Idle_Task;

   ---------------------------
   -- Try_Allocate_One_Task --
   ---------------------------

   procedure Try_Allocate_One_Task is
   begin

      pragma Debug (O ("Try_Allocate_One_Task: enter"));

      if Unscheduled_Tasks > 0 then

         --  Some tasks are not scheduled. We assume one of them will
         --  be allocated to handle current event.

         pragma Debug (O ("Assume one unaffected task will handle event"));
         null;

      else

         if Idle_Tasks > 0 then
            Awake_One_Idle_Task;

         else
            pragma Debug (O ("No idle tasks"));
            null;

         end if;
      end if;

      pragma Debug (O ("Try_Allocate_One_Task: end"));
   end Try_Allocate_One_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Create (Polling_Completed);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"scheduler",
       Conflicts => Empty,
       Depends   => +"tasking.condition_variables",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.Scheduler;
