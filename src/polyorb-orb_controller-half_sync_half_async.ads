------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.ORB_CONTROLLER.HALF_SYNC_HALF_ASYNC                --
--                                                                          --
--                                 S p e c                                  --
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

--  Half-Sync/Half-Async ORB Controller for PolyORB ORB main loop.

--  It is a tasking ORB Controller implementation

--  It supports:
--   * multi-tasking ORB only
--   * one asynchronous event monitor

--  Under this implementation, a dedicated task monitors AES, other
--  tasks process requests.

with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Idle_Tasks_Managers;
with PolyORB.Tasking.Mutexes;

package PolyORB.ORB_Controller.Half_Sync_Half_Async is

   type ORB_Controller_Half_Sync_Half_Async is new ORB_Controller with private;

   type ORB_Controller_Half_Sync_Half_Async_Access is
     access all ORB_Controller_Half_Sync_Half_Async'Class;

   procedure Register_Task
     (O  : access ORB_Controller_Half_Sync_Half_Async;
      TI :        PTI.Task_Info_Access);

   procedure Unregister_Task
     (O  : access ORB_Controller_Half_Sync_Half_Async;
      TI :        PTI.Task_Info_Access);

   procedure Notify_Event
     (O : access ORB_Controller_Half_Sync_Half_Async;
      E :        Event);

   procedure Schedule_Task
     (O  : access ORB_Controller_Half_Sync_Half_Async;
      TI :        PTI.Task_Info_Access);

   procedure Disable_Polling (O : access ORB_Controller_Half_Sync_Half_Async);

   procedure Enable_Polling (O : access ORB_Controller_Half_Sync_Half_Async);

   procedure Enter_ORB_Critical_Section
     (O : access ORB_Controller_Half_Sync_Half_Async);

   procedure Leave_ORB_Critical_Section
     (O : access ORB_Controller_Half_Sync_Half_Async);

   type ORB_Controller_Half_Sync_Half_Async_Factory is
     new ORB_Controller_Factory with private;

   function Create
     (OCF : access ORB_Controller_Half_Sync_Half_Async_Factory)
     return ORB_Controller_Access;

private

   package PTM renames PolyORB.Tasking.Mutexes;
   package PTCV renames PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Idle_Tasks_Managers;

   --  Under this ORB controller implementation, several tasks may go
   --  idle. Each idle task waits on a specific condition variable.

   type ORB_Controller_Half_Sync_Half_Async is new ORB_Controller with record

      ORB_Lock : PTM.Mutex_Access;
      --  Mutex used to enforce ORB critical section

      Internal_ORB_Lock : PTM.Mutex_Access;

      Idle_Tasks : Idle_Tasks_Manager_Access;

      ---------------------
      -- Monitoring task --
      ---------------------

      Monitoring_Task_Info : PTI.Task_Info_Access;
      --  Under this ORB controller implementation, only one task may
      --  enter blocked state. The same task will continuously wait
      --  for incoming events. We store here its Task_Info.

      Monitoring_Task_Job_Queue : PJ.Job_Queue_Access;
      --  Specific job queue of jobs to be processed by the blocked task.
      --  XXX replace it by an array ?

      Monitoring_Task_CV : PTCV.Condition_Access;

      Monitoring_Task_Idle : Boolean := False;

      Polling_Abort_Counter : Natural := 0;
      --  Indicates number of tasks that requested abortion of polling.

      Polling_Completed : PTCV.Condition_Access;
      --  This condition is signalled after polling is completed. It
      --  is used by tasks for the polling task to release any
      --  reference to source list that is to be modified.

      Polling_Scheduled : Boolean := False;
      --  True iff a task will poll on AES

      Polling_Interval : Duration;
      Polling_Timeout  : Duration;
      Counter : Natural := 0;

   end record;

   type ORB_Controller_Half_Sync_Half_Async_Factory is
     new ORB_Controller_Factory with null record;

   OCF : constant ORB_Controller_Factory_Access
     := new ORB_Controller_Half_Sync_Half_Async_Factory;

end PolyORB.ORB_Controller.Half_Sync_Half_Async;
