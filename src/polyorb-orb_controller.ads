------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . O R B _ C O N T R O L L E R                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
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

--  ORB Controller for PolyORB ORB main loop

with PolyORB.Asynch_Ev;
with PolyORB.Jobs;
with PolyORB.Log;
with PolyORB.References;
with PolyORB.Request_Scheduler;
with PolyORB.Task_Info;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Idle_Tasks_Managers;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;

package PolyORB.ORB_Controller is

   --  An ORB Control Policy is responsible for the management of the global
   --  state of the ORB (running tasks, job processing, etc). It grants access
   --  to ORB internals and affects action to all registered tasks.

   --  It is the responsibility of the ORB Control Policy to ensure that all
   --  tasks work concurrently and access the ORB internals safely.

   --  An ORB Controller is an instance of an ORB Control Policy, attached to
   --  an ORB instance. It is a passive object, triggered by the occurence of
   --  some specific events within the ORB.

   package PAE  renames PolyORB.Asynch_Ev;
   package PJ   renames PolyORB.Jobs;
   package PR   renames PolyORB.References;
   package PRS  renames PolyORB.Request_Scheduler;
   package PTI  renames PolyORB.Task_Info;
   package PTM  renames PolyORB.Tasking.Mutexes;
   package PT   renames PolyORB.Tasking.Threads;
   package PTCV renames PolyORB.Tasking.Condition_Variables;

   -----------
   -- Event --
   -----------

   --  Events handled by ORB Controllers. Specific events trigger the ORB
   --  Controller, which then modify ORB global state.

   type Event_Kind is
     (End_Of_Check_Sources,
      --  A task completed Check_Sources on a monitor

      Event_Sources_Added,
      --  An AES has been added to monitored AES list

      Event_Sources_Deleted,
      --  An AES has been deleted from monitored AES list

      Job_Completed,
      --  A job has been completed

      ORB_Shutdown,
      --  ORB shutdown has been requested

      Queue_Event_Job,
      --  Queue an event job

      Queue_Request_Job,
      --  Queue a request job

      Request_Result_Ready,
      --  A Request has been completed

      Idle_Awake,
      --  A task has left Idle state

      Task_Registered,
      --  A task has joined the ORB pool and has been added to the summary

      Task_Unregistered
      --  A task has left the ORB pool and has been removed from the summary
      );

   --  Event type

   type Event (Kind : Event_Kind) is record
      case Kind is
         when End_Of_Check_Sources =>
            On_Monitor : PAE.Asynch_Ev_Monitor_Access;

         when Event_Sources_Added =>
            Add_In_Monitor : PAE.Asynch_Ev_Monitor_Access;
            --  Non null iff we add a source to a new monitor

         when Event_Sources_Deleted | Job_Completed | ORB_Shutdown =>
            null;

         when Queue_Event_Job =>
            Event_Job : PJ.Job_Access;
            By_Task   : PT.Thread_Id;

         when Queue_Request_Job =>
            Request_Job : PJ.Job_Access;
            Target      : PR.Ref;

         when Request_Result_Ready =>
            Requesting_Task : PTI.Task_Info_Access;

         when Idle_Awake =>
            Awakened_Task : PTI.Task_Info_Access;

         when Task_Registered =>
            Registered_Task : PTI.Task_Info_Access;

         when Task_Unregistered =>
            Unregistered_Task : PTI.Task_Info_Access;
      end case;
   end record;

   --------------------
   -- ORB_Controller --
   --------------------

   type ORB_Controller (RS : PRS.Request_Scheduler_Access) is
     abstract tagged limited private;

   type ORB_Controller_Access is access all ORB_Controller'Class;

   procedure Enter_ORB_Critical_Section (O : access ORB_Controller);
   pragma Inline (Enter_ORB_Critical_Section);
   --  Enter ORB critical section

   procedure Leave_ORB_Critical_Section (O : access ORB_Controller);
   pragma Inline (Leave_ORB_Critical_Section);
   --  Leave ORB critical section

   --  The following subprograms must be called from within the ORB critical
   --  section.

   procedure Register_Task
     (O  : access ORB_Controller; TI : PTI.Task_Info_Access);
   --  Register TI to the ORB Controller. TI may now be used by the ORB
   --  Controller to process ORB actions.

   procedure Terminate_Task
     (O  : access ORB_Controller; TI : PTI.Task_Info_Access);
   --  Record that the given task is terminating. If the task was idle, the
   --  ORB controller is notified. Task state is then set to Terminated.

   procedure Unregister_Task
     (O  : access ORB_Controller; TI : PTI.Task_Info_Access);
   --  Unregister terminated task TI from the ORB Controller

   procedure Notify_Event
     (O : access ORB_Controller;
      E : Event) is abstract;
   --  Notify ORB Controller O of the occurence of event E.
   --  This procedure may change the status of idle or blocked tasks.

   procedure Schedule_Task
     (O  : access ORB_Controller;
      TI : PTI.Task_Info_Access) is abstract;
   --  TI is the current task. Set its state to indicate the next action to be
   --  executed. This operation has no effect and returns immediately if the
   --  current state of the task is Terminated.

   procedure Disable_Polling
     (O : access ORB_Controller;
      M : PAE.Asynch_Ev_Monitor_Access) is abstract;
   --  Disable polling on AES monitored by M, abort polling task and waits for
   --  its completion, if required.
   --
   --  The ORB critical section is exited temporarily while waiting for
   --  completion of any ongoing polling operation: several tasks might be
   --  blocked concurrently in this procedure. The critical section is
   --  re-entered after the ongoing polling operation has been completed.

   procedure Enable_Polling
     (O : access ORB_Controller;
      M : PAE.Asynch_Ev_Monitor_Access) is abstract;
   --  Enable polling on AES monitored by M. If Disable_Polling has been called
   --  N times, Enable_Polling must be called N times to actually enable
   --  polling. It is the user's responsability to ensure that Enable_Polling
   --  actually enables polling in bounded time.

   function Has_Pending_Job (O : access ORB_Controller) return Boolean;
   --  Return true iff a job is pending

   function Get_Pending_Job (O : access ORB_Controller) return PJ.Job_Access;
   --  Return a pending job, null if there is none

   function Shutting_Down (O : ORB_Controller) return Boolean;
   pragma Inline (Shutting_Down);
   --  Return True if ORB shutdown is in progress

   function Is_Locally_Terminated
     (O                      : access ORB_Controller;
      Expected_Running_Tasks : Natural) return Boolean;
   --  Return true if the local node is locally terminated.
   --  Expected_Running_Tasks is the number of expected non terminated tasks
   --  when local termination is computed.

   type Monitor_Array is array (Natural range <>)
     of PAE.Asynch_Ev_Monitor_Access;

   function Get_Monitors (O : access ORB_Controller) return Monitor_Array;
   pragma Inline (Get_Monitors);
   --  Return monitors handled by the ORB

   function Get_Tasks_Count
     (OC    : ORB_Controller;
      Kind  : PTI.Any_Task_Kind  := PTI.Any;
      State : PTI.Any_Task_State := PTI.Any) return Natural;
   pragma Inline (Get_Tasks_Count);
   --  Return the count of tasks for the given kind and state

   procedure Wait_For_Completion (O : access ORB_Controller);
   --  When ORB shutdown has been requested, block until all pending jobs are
   --  processed. The ORB critical section is exited temporarily while waiting
   --  for completion, and reasserted afterwards.

   ----------------------------
   -- ORB_Controller_Factory --
   ----------------------------

   type ORB_Controller_Factory is abstract tagged limited null record;

   type ORB_Controller_Factory_Access is
     access all ORB_Controller_Factory'Class;

   function Create (OCF : ORB_Controller_Factory) return ORB_Controller_Access
      is abstract;
   --  Use factory to create a new ORB_Controller

   procedure Register_ORB_Controller_Factory
     (OCF : ORB_Controller_Factory_Access);
   --  Register an ORB_Controller factory

   procedure Create (O : out ORB_Controller_Access);
   --  Initialize an ORB_Controller by dispatching to Create function of the
   --  currently registered factory.

private

   use PolyORB.Log;
   use PolyORB.Tasking.Idle_Tasks_Managers;

   package L1 is new PolyORB.Log.Facility_Log ("polyorb.orb_controller");
   procedure O1 (Message : String; Level : Log_Level := Debug)
     renames L1.Output;
   function C1 (Level : Log_Level := Debug) return Boolean renames L1.Enabled;

   package L2 is
      new PolyORB.Log.Facility_Log ("polyorb.orb_controller_status");
   procedure O2 (Message : String; Level : Log_Level := Debug)
     renames L2.Output;
   function C2 (Level : Log_Level := Debug) return Boolean renames L2.Enabled;

   function Status (O : ORB_Controller) return String;
   --  Output status of task running Broker, for debugging purpose

   procedure Try_Allocate_One_Task
     (O : access ORB_Controller; Allow_Transient : Boolean);
   --  A job has been queued on the general ORB controller job queue:
   --  awake one idle task to process it. If no idle task is available, and no
   --  permanent running task is about to reschedule, unblock a polling task.

   function Need_Polling_Task (O : access ORB_Controller) return Natural;
   --  Return the index of the AEM_Info of a monitor waiting for polling task,
   --  else return 0. Note that the index of the last polled AEM is recorded
   --  in O to ensure fairness.

   function Index
     (O : ORB_Controller;
      M : PAE.Asynch_Ev_Monitor_Access) return Natural;
   pragma Inline (Index);
   --  Return the index of M held in O.AEM_Infos, 0 if not found

   type AEM_Info is record
      Monitor : PAE.Asynch_Ev_Monitor_Access;
      --  Monitor to be polled

      TI : PTI.Task_Info_Access;
      --  Store the Task_Info allocated to monitor this AEM

      Polling_Abort_Counter : Natural := 0;
      --  Indicates number of tasks that requested abortion of polling

      Polling_Completed : PTCV.Condition_Access;
      --  This condition is signalled after polling is completed. It is used by
      --  tasks for the polling task to release any reference to source list
      --  that is to be modified.

      Polling_Scheduled : Boolean := False;
      --  True iff a task will poll on AES

      Polling_Interval : Duration;
      --  XXX TO BE DOCUMENTED

      Polling_Timeout  : Duration;
      --  XXX TO BE DOCUMENTED
   end record;

   type AEM_Infos_Array is array (Natural range <>) of AEM_Info;

   Maximum_Number_Of_Monitors : constant := 2;

   type ORB_Controller (RS : PRS.Request_Scheduler_Access) is
   abstract tagged limited record
      ORB_Lock : PTM.Mutex_Access;
      --  Mutex used to enforce ORB critical section

      Job_Queue : PJ.Job_Queue_Access;
      --  The queue of jobs to be processed by ORB tasks

      AEM_Infos : AEM_Infos_Array (1 .. Maximum_Number_Of_Monitors);
      Last_Monitored_AEM : Natural := Maximum_Number_Of_Monitors;
      --  ??? Needs proper documentation of usage of this component.
      --  Half_Sync_Half_Async uses it to point to the designated monitoring
      --  task for each monitor; other ORB controllers use it for the currently
      --  blocked task (and in the latter only there is an invariant that the
      --  TI component of each slot points to the existing valid task info for
      --  a Blocked task.)

      Idle_Tasks : Idle_Tasks_Manager_Access;

      -----------------------------
      -- Global controller state --
      -----------------------------

      Summary : PTI.Task_Summary;
      --  Task counters

      Shutdown : Boolean := False;
      --  True iff ORB is to be shutdown

      Shutdown_CV : PTCV.Condition_Access;
      --  CV used by callers of Shutdown to wait for completion of all pending
      --  requests.
   end record;

   procedure Initialize (OC : in out ORB_Controller);
   --  Initialize OC elements

   procedure Note_Task_Unregistered (O : access ORB_Controller'Class);
   --  Called by concrete ORB controllers after processing a task
   --  unregistration notification.

   function Is_Upcall (J : PJ.Job'Class) return Boolean;
   --  Return True if job J that is queued on the ORB controller job queue
   --  involves an upcall to application code (in which case it must not be
   --  handled by a transient task). Note that this predicate is defined only
   --  for queued jobs.

   procedure Reschedule_Task
     (O  : access ORB_Controller;
      TI : PTI.Task_Info_Access);
   --  Cause the given task to be rescheduled (i.e. awakened if it is idle,
   --  unblocked if it is blocked). Used when a condition occurs that the task
   --  needs to be informed of.

end PolyORB.ORB_Controller;
