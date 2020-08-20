------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T A S K _ I N F O                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2020, Free Software Foundation, Inc.          --
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

--  This package provides a facility for associating information with each
--  task executing the ORB main loop.

pragma Ada_2012;

with PolyORB.Asynch_Ev;
with PolyORB.Jobs;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;
with PolyORB.Types;
with PolyORB.Utils.Ilists;

package PolyORB.Task_Info is

   pragma Preelaborate;

   package PAE  renames PolyORB.Asynch_Ev;
   package PTCV renames PolyORB.Tasking.Condition_Variables;
   package PTM  renames PolyORB.Tasking.Mutexes;

   type Any_Task_Kind is (Any, Permanent, Transient);
   subtype Task_Kind is Any_Task_Kind range Permanent .. Transient;
   --  A Permanent task executes ORB.Run indefinitely.
   --  A Transient task executes ORB.Run until a given exit condition is met.
   --  Transient tasks are lent to neutral core middleware by user code.

   type Any_Task_State is
     (Any, Unscheduled, Running, Blocked, Idle, Terminated);
   subtype Task_State is Any_Task_State range Unscheduled .. Terminated;
   --  An Unscheduled task is waiting for rescheduling.
   --  A Running task is executing an ORB activity.
   --  A Blocked task is waiting for an external asynchronous event.
   --  An Idle task is waiting on a condition variable expecting another task
   --  to request ORB action.
   --  A Terminated task has been notified its exit condition is true.

   type Task_Info (Kind : Task_Kind) is limited private;
   type Task_Info_Access is access all Task_Info;
   --  Task Info holds information on tasks that run ORB.Run

   function Kind_Match (TI : Task_Info; Kind : Any_Task_Kind) return Boolean;
   --  True if Kind matches TI's Kind (Any matches any kind)

   type Task_Summary is limited private;
   --  Summary information: counter of registered tasks and of how many tasks
   --  of each kind are in each state.

   function Get_Count
     (Summary : Task_Summary;
      Kind    : Any_Task_Kind  := Any;
      State   : Any_Task_State := Any) return Natural;
   --  Return the count of tasks with the given kind and state. If Kind or
   --  State is Any, return sum for all Kinds, respectively for all States.

   function Task_Summary_Valid (Summary : Task_Summary) return Boolean;
   --  Check the total count against the sum of the partial counts (for
   --  assertions purpose).

   procedure Task_Created (Summary : in out Task_Summary; TI : Task_Info);
   procedure Task_Removed (Summary : in out Task_Summary; TI : Task_Info);
   --  Record creation / task removal of the given task

   ------------------------------------
   -- Task_Info components accessors --
   ------------------------------------

   function State (TI : Task_Info) return Task_State;
   --  Return the state of the task referred by TI

   procedure Set_State_Blocked
     (Summary  : in out Task_Summary;
      TI       : in out Task_Info;
      Selector : Asynch_Ev.Asynch_Ev_Monitor_Access;
      Timeout  : Duration);
   --  The task referred by TI will be blocked on Selector for Timeout seconds

   procedure Set_State_Idle
     (Summary   : in out Task_Summary;
      TI        : in out Task_Info;
      Condition : PTCV.Condition_Access;
      Mutex     : PTM.Mutex_Access);
   --  The task referred by TI will go Idle until Condition is signalled

   procedure Set_State_Running
     (Summary : in out Task_Summary;
      TI      : in out Task_Info;
      Job     : Jobs.Job_Access);
   --  The task referred by TI is now in Running state, and will execute Job;
   --  this procedure resets Selector or Condition it was blocked on.

   procedure Set_State_Unscheduled
     (Summary : in out Task_Summary;
      TI      : in out Task_Info);
   --  The task referred by TI is now in Unscheduled state.

   procedure Set_State_Terminated
     (Summary : in out Task_Summary;
      TI      : in out Task_Info);
   --  The task referred by TI has terminated its job.

   function Scope_Lock (TI : Task_Info) return PTM.Mutex_Access;
   procedure Set_Scope_Lock
     (TI : in out Task_Info;
      SL : PTM.Mutex_Access);
   --  Return the scope lock held on the ORB critical section

   function Selector
     (TI : Task_Info) return Asynch_Ev.Asynch_Ev_Monitor_Access;
   --  Return Selector the task referred by TI is blocked on

   function Timeout (TI : Task_Info) return Duration;
   --  Return Timeout before stopping blocking

   function Condition (TI : Task_Info) return PTCV.Condition_Access;
   --  Return Condition Variable the Task referred by TI is blocked on

   function Mutex (TI : Task_Info) return PTM.Mutex_Access;
   --  Return Mutex used by the Task referred by TI when blocking.

   procedure Set_Id (TI : in out Task_Info);
   --  Task_Info will hold Id of the current task, as provided by the PolyORB
   --  tasking subsystem.

   procedure Set_May_Exit
     (TI       : in out Task_Info;
      May_Exit : Boolean);
   --  Set the corresponding flags on TI

   function May_Exit (TI : Task_Info) return Boolean;
   --  Return the corresponding flags for TI

   procedure Set_Exit_Condition
     (TI             : in out Task_Info;
      Exit_Condition : Types.Boolean_Ptr);
   --  Attach Exit_Condition to TI

   function Exit_Condition (TI : Task_Info) return Boolean;
   --  Return the value of TI's exit condition

   procedure Request_Abort_Polling (TI : in out Task_Info);
   --  Request TI to abort polling. Meaningful only if TI is in blocked state

   function Abort_Polling (TI : Task_Info) return Boolean;
   --  Return true if TI must abort polling and leave blocked state.
   --  Meaningful only if TI is in blocked state.

   function Id (TI : Task_Info) return PolyORB.Tasking.Threads.Thread_Id;
   --  Return thread id associated to TI

   function Job (TI : Task_Info) return Jobs.Job_Access;
   --  Return job associated to TI

   type Task_List is private;
   --  A list of tasks

   function Is_Empty (List : Task_List) return Boolean;
   --  True when List has no elements

   function List_First (List : Task_List) return Task_Info_Access;
   --  Return the first element of List

   procedure List_Attach
     (TI   : access Task_Info;
      List : in out Task_List);
   --  Attach TI to the List. It must not already be on a list. Order of
   --  attachment and detachment is arbitrary.

   procedure List_Detach
     (TI   : access Task_Info;
      List : in out Task_List);
   --  Remove TI from the list it was attached to (if any)

   function On_List (TI : Task_Info) return Boolean;
   --  True if TI is attached on a list (for assertions)

   function Image (TI : Task_Info) return String;
   --  For debug purposes

private

   type Links_Type is
     array (Utils.Ilists.Link_Type) of aliased Task_Info_Access;

   type Task_Info (Kind : Task_Kind) is limited record
      Id : PolyORB.Tasking.Threads.Thread_Id;
      --  Task referred by Task_Info record

      State : Task_State := Unscheduled;
      --  Current Task status, not permitted to be changed except by internal
      --  procedure Task_State_Change, which in turn is called by each of the
      --  Set_State_xxx external procedures.

      May_Exit : Boolean := False;
      --  True iff ORB contoller is allowed to decide to terminate this task

      Abort_Polling : Boolean := False;
      --  True iff must abort polling

      Exit_Condition : PolyORB.Types.Boolean_Ptr := null;
      --  Null for Permanent tasks, in which case the exit condition is
      --  considered to be False. For Transient tasks, points to an initially
      --  False Boolean, which is set True when the task should exit ORB.Run.

      Job : Jobs.Job_Access;
      --  Job to run, meaningful only when State is Running

      Scope_Lock : PTM.Mutex_Access;
      --  Scope lock held by the task on the ORB critical section

      Selector  : Asynch_Ev.Asynch_Ev_Monitor_Access;
      --  Monitor on which Task referred by Id is blocked;
      --  meaningful only when State is Blocked.

      Timeout : Duration;
      --  Timeout before stopping polling when Blocked

      Condition : Tasking.Condition_Variables.Condition_Access;
      --  CV on which Task referred by Id is waiting in Idle state

      Mutex : Tasking.Mutexes.Mutex_Access;
      --  Mutex used by the Task referred by TI when blocking;
      --  meaningful only when State is Idle.

      Links : Links_Type;
      --  Pointers allowing the task to be attached to a (single) task list

      On_List : Boolean := False;
      --  True when task is attached to a task list
   end record;

   function Link
     (S     : access Task_Info;
      Which : Utils.Ilists.Link_Type) return access Task_Info_Access;
   pragma Inline (Link);
   --  Accessor for Links

   package Task_Lists is
     new Utils.Ilists.Lists
       (T             => Task_Info,
        T_Acc         => Task_Info_Access,
        Doubly_Linked => True);

   type Task_List is new Task_Lists.List;

   type Task_Counters is array (Any_Task_Kind, Any_Task_State) of Natural;

   type Task_Summary is limited record
      Counters : Task_Counters := (others => (others => 0));
      --  Count of tasks of each kind and state
   end record;

   pragma Inline (Get_Count);

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
   pragma Inline (Set_May_Exit);
   pragma Inline (May_Exit);
   pragma Inline (Set_Exit_Condition);
   pragma Inline (Exit_Condition);
   pragma Inline (Request_Abort_Polling);
   pragma Inline (Abort_Polling);
   pragma Inline (Image);
   pragma Inline (Id);
   pragma Inline (Job);

end PolyORB.Task_Info;
