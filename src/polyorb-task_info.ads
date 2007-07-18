------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T A S K _ I N F O                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

--  Information about running ORB tasks

--  This package is used to store and retrieve information
--  concerning the status of tasks that execute ORB functions.

with PolyORB.Asynch_Ev;
with PolyORB.Jobs;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Task_Info is

   pragma Elaborate_Body;

   package PAE  renames PolyORB.Asynch_Ev;
   package PTCV renames PolyORB.Tasking.Condition_Variables;
   package PTM  renames PolyORB.Tasking.Mutexes;

   type Task_Kind is (Permanent, Transient);
   --  A Permanent task executes ORB.Run indefinitely.
   --  A Transient task executes ORB.Run until a given exit condition
   --  is met. Transient tasks are lent to neutral core middleware by
   --  user activities.

   Task_Kind_For_Exit_Condition : constant array (Boolean)
     of Task_Kind := (True => Permanent, False => Transient);
   --  The task kind according to whether Exit_Condition
   --  is null (True) or not.

   type Task_State is (Unscheduled, Running, Blocked, Idle, Terminated);
   --  An Unscheduled task is waiting for rescheduling.
   --  A Running task is executing an ORB activity.
   --  A Blocked task is waiting for an external asynchronous event.
   --  An Idle task is waiting on a condition variable expecting
   --  another task to request ORB action.
   --  A Terminated task has been notified its exit condition is true.

   type Task_Info (Kind : Task_Kind) is limited private;
   --  Task Info holds information on tasks that run ORB.Run

   type Task_Info_Access is access all Task_Info;
   package Task_Lists is new PolyORB.Utils.Chained_Lists
     (Task_Info_Access, Doubly_Chained => True);

   ------------------------------------
   -- Task_Info components accessors --
   ------------------------------------

   procedure Set_State_Blocked
     (TI       : in out Task_Info;
      Selector :        Asynch_Ev.Asynch_Ev_Monitor_Access;
      Timeout  :        Duration);
   --  The task referred by TI will be blocked on Selector for Timeout seconds

   procedure Set_State_Idle
     (TI        : in out Task_Info;
      Condition :        PTCV.Condition_Access;
      Mutex     :        PTM.Mutex_Access);
   --  The task referred by TI will go Idle;
   --  signalling condition variable Condition will awake it.

   procedure Set_State_Running (TI : in out Task_Info; Job : Jobs.Job_Access);
   --  The task referred by TI is now in Running state, and will execute Job;
   --  this procedure resets Selector or Condition it was blocked on.

   procedure Set_State_Unscheduled (TI : in out Task_Info);
   --  The task referred by TI is now unaffected.

   procedure Set_State_Terminated (TI : in out Task_Info);
   --  The task referred by TI has terminated its job.

   function State (TI : Task_Info) return Task_State;
   --  Return the state of the task referred by TI

   function Selector
     (TI : Task_Info)
     return Asynch_Ev.Asynch_Ev_Monitor_Access;
   --  Return Selector the task referred by TI is blocked on

   function Timeout (TI : Task_Info) return Duration;
   --  Return Timeout before stopping blocking

   function Condition (TI : Task_Info) return PTCV.Condition_Access;
   --  Return Condition Variable the Task referred by TI is blocked on

   function Mutex (TI : Task_Info) return PTM.Mutex_Access;
   --  Return Mutex used by the Task referred by TI when blocking.

   procedure Set_Id (TI : in out Task_Info);
   --  Task_Info will hold Id of the current task, as provided by
   --  PolyORB tasking runtime.

   procedure Set_Polling (TI : in out Task_Info; May_Poll : Boolean);
   --  Set if TI may poll on event sources, i.e. be in blocked state

   function May_Poll (TI : Task_Info) return Boolean;
   --  Returns true iff TI may poll, i.e. be in blocked state

   procedure Set_Exit_Condition
     (TI             : in out Task_Info;
      Exit_Condition :        PolyORB.Types.Boolean_Ptr);
   --  Attach Exit_Condition to TI

   function Exit_Condition (TI : Task_Info) return Boolean;
   --  Return the value of TI's exit condition

   procedure Request_Abort_Polling (TI : in out Task_Info);
   --  Request TI to abort polling. Meaningful only if TI is in
   --  blocked state.

   function Abort_Polling (TI : Task_Info) return Boolean;
   --  Return true if TI must abort polling and leave blocked state.
   --  Meaningful only if TI is in blocked state.

   function Image (TI : Task_Info) return String;
   --  For debug purposes

   function Id (TI : Task_Info) return PolyORB.Tasking.Threads.Thread_Id;
   --  Return thread id associated to TI

   function Job (TI : Task_Info) return Jobs.Job_Access;
   --  Return job associated to TI

   procedure List_Attach
     (TI       : in out Task_Info;
      Position : Task_Lists.Iterator);
   --  Record that TI is on a list at the given Position. Clears the
   --  attachment information if Position is the end of a list.

   procedure List_Detach
     (TI   : in out Task_Info;
      List : in out Task_Lists.List);
   --  Remove TI from the list it was attached to (if any).

private

   type Task_Info (Kind : Task_Kind) is limited record

      Id : PolyORB.Tasking.Threads.Thread_Id;
      --  Task referred by Task_Info record

      State : Task_State := Unscheduled;
      --  Current Task status

      May_Poll : Boolean := False;
      --  True iff task may poll on event sources

      Abort_Polling : Boolean := False;
      --  True iff must abort polling

      Exit_Condition : PolyORB.Types.Boolean_Ptr := null;
      --  Exit condition; meaningful only when Kind = Transient

      Job : Jobs.Job_Access;
      --  Job to run, meaningful only when State is Running

      Selector  : Asynch_Ev.Asynch_Ev_Monitor_Access;
      --  Monitor on which Task referred by Id is blocked;
      --  meaningful only when State is Blocked.

      Timeout : Duration;
      --  Timeout before stopping polling when Blocked

      Condition : Tasking.Condition_Variables.Condition_Access;
      --  Condition Variable on which Task referred by Id is
      --  blocked; meaningful only when State is Idle.

      Mutex : Tasking.Mutexes.Mutex_Access;
      --  Mutex used by the Task referred by TI when blocking;
      --  meaningful only when State is Idle.

      Position : Task_Lists.Iterator;
      --  Iterator designating the position of this task on a
      --  list (allowing removal of the task from the list).
   end record;

   pragma Inline (Set_State_Blocked);
   pragma Inline (Set_State_Idle);
   pragma Inline (Set_State_Running);
   pragma Inline (Set_State_Unscheduled);
   pragma Inline (Set_State_Terminated);
   pragma Inline (State);
   pragma Inline (Selector);
   pragma Inline (Timeout);
   pragma Inline (Condition);
   pragma Inline (Mutex);
   pragma Inline (Set_Id);
   pragma Inline (Set_Polling);
   pragma Inline (May_Poll);
   pragma Inline (Set_Exit_Condition);
   pragma Inline (Exit_Condition);
   pragma Inline (Request_Abort_Polling);
   pragma Inline (Abort_Polling);
   pragma Inline (Image);
   pragma Inline (Id);
   pragma Inline (Job);

end PolyORB.Task_Info;
