------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . S C H E D U L E R                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  Scheduling Policies for PolyORB ORB main loop.

--  A Scheduling Policy is responsible for the allocation of existing
--  tasks to complete specific ORB actions. Allocation is done upon
--  the notification of events by the ORB. A Scheduler is an instance
--  of a Scheduling Policy, attached to an ORB instance.

--  Upon the notification of an event:
--  * idle tasks may be awaken,
--  * blocked tasks may be interrupted.

--  Tasks may change status when they ask for scheduling.

--  Note:
--  1) No task are created. It is the responsability of the ORB
--     Tasking Policy and/or user application code to create tasks.
--  2) Scheduler must be called from within ORB critical section

with PolyORB.Task_Info;
with PolyORB.Tasking.Mutexes;

package PolyORB.Scheduler is

   package PTI renames PolyORB.Task_Info;
   package PTM renames PolyORB.Tasking.Mutexes;

   ----------------
   -- ORB Events --
   ----------------

   --  Event kinds handled by scheduling policies.

   type Event_Kind is
     (End_Of_Check_Sources,
      --  A task completed Check_Sources on a monitor

      Event_Sources_Added,
      --  An AES has been added to monitored AES list

      Event_Sources_Deleted,
      --  An AES has been deleted from monitored AES list

      Executing_Job,
      --  A task is executing a job,

      Job_Completed,
      --  A job has been queued

      Job_Queued,
      --  A job has been queued

      ORB_Shutdown,
      --  ORB shutdiwn has been requested

      Request_Result_Ready
      --  A Request has been completed

      );

   --  Event type

   type Event (Kind : Event_Kind) is record
      case Kind is
         when Request_Result_Ready =>
            TI : PTI.Task_Info_Access;

         when others =>
            null;
      end case;
   end record;

   --  Some events are used as 'tags', we declare them as constants.

   End_Of_Check_Sources_E  : constant Event (End_Of_Check_Sources);
   Event_Sources_Added_E   : constant Event (Event_Sources_Added);
   Event_Sources_Deleted_E : constant Event (Event_Sources_Deleted);
   Executing_Job_E         : constant Event (Executing_Job);
   Job_Completed_E         : constant Event (Job_Completed);
   Job_Queued_E            : constant Event (Job_Queued);
   ORB_Shutdown_E          : constant Event (ORB_Shutdown);

   -----------------------
   -- Scheduling_Policy --
   -----------------------

   type Scheduling_Policy is tagged limited private;
   --  XXX maybe abstract ..

   type Scheduling_Policy_Access is access all Scheduling_Policy'Class;

   procedure Initialize
     (S     : access Scheduling_Policy;
      Mutex :        PTM.Mutex_Access);
   --  Initialize scheduler mutex. Scheduler internals requires that
   --  Mutex is a valid reference to ORB lock.

   procedure Register_Task
     (S  : access Scheduling_Policy;
      TI :        PTI.Task_Info);
   --  Register TI to scheduler S. TI may now be scheduled by
   --  scheduler S to process ORB functions.

   procedure Unregister_Task
     (S  : access Scheduling_Policy;
      TI :        PTI.Task_Info);
   --  Unregister TI from Scheduler

   procedure Notify_Event
     (S : access Scheduling_Policy;
      E :        Event);
   --  Notify Scheduler S of the occurence of event E;
   --  This procedure may change status of idle or blocked tasks.

   function Schedule_Task
     (S  : access Scheduling_Policy;
      TI : access PTI.Task_Info)
     return PTI.Task_State;
   --  Return the new state for TI

   procedure Disable_Polling (S : access Scheduling_Policy);
   --  Disable polling on ORB's AES, abort polling task and waits for
   --  its completion, if required. ORB Lock (see Initialize above)
   --  is released when waiting for polling task completion: several
   --  tasks may enter this procedure. ORB Lock ensures they will
   --  leave it sequentially.

   procedure Enable_Polling (S : access Scheduling_Policy);
   --  Enable polling on AES. If Disable_Polling has been called N
   --  times, Enable_Polling must be called N times to actually enable
   --  polling. It is the user responsability to ensure that
   --  Enable_Polling actually enables polling in bounded time.

private

   type Scheduling_Policy is tagged limited null record;

   End_Of_Check_Sources_E : constant Event (End_Of_Check_Sources) :=
     Event'(Kind =>  End_Of_Check_Sources);

   Event_Sources_Added_E : constant Event (Event_Sources_Added) :=
     Event'(Kind => Event_Sources_Added);

   Event_Sources_Deleted_E : constant Event (Event_Sources_Deleted) :=
     Event'(Kind => Event_Sources_Deleted);

   Executing_Job_E : constant Event (Executing_Job) :=
     Event'(Kind => Executing_Job);

   Job_Completed_E : constant Event (Job_Completed) :=
     Event'(Kind => Job_Completed);

   Job_Queued_E : constant Event (Job_Queued) :=
     Event'(Kind => Job_Queued);

   ORB_Shutdown_E : constant Event (ORB_Shutdown) :=
     Event'(Kind => ORB_Shutdown);

end PolyORB.Scheduler;
