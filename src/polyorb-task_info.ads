------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T A S K _ I N F O                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  Information about running ORB tasks.

--  This package is used to store and retrieve information
--  concerning the status of tasks that execute ORB functions.

--  $Id$

with PolyORB.Asynch_Ev;
with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Types;

package PolyORB.Task_Info is

   pragma Elaborate_Body;

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   type Task_Kind is (Permanent, Transient);
   --  A Permanent task executes ORB.Run indefinitely.
   --  A Transient task executes ORB.Run until a given exit condition
   --  is met. Transient tasks are lent to neutral core middleware by
   --  user activities.

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

   ------------------------------------
   -- Task_Info components accessors --
   ------------------------------------

   procedure Set_State_Blocked
     (TI       : in out Task_Info;
      Selector :        Asynch_Ev.Asynch_Ev_Monitor_Access);
   pragma Inline (Set_State_Blocked);
   --  The task refereed by TI will be blocked on Selector

   procedure Set_State_Idle
     (TI        : in out Task_Info;
      Condition :        PTCV.Condition_Access);
   pragma Inline (Set_State_Idle);
   --  The task refereed by TI will go Idle;
   --  signaling condition variable Condition will awake it.

   procedure Set_State_Running
     (TI : in out Task_Info);
   pragma Inline (Set_State_Running);
   --  The task refereed by TI is now running;
   --  this procedure resets Selector or Condition it was blocked on.

   procedure Set_State_Unscheduled
     (TI : in out Task_Info);
   pragma Inline (Set_State_Unscheduled);
   --  The task refereed by TI is now unaffected.

   function State (TI : Task_Info)
     return Task_State;
   pragma Inline (State);
   --  Return the state of the task referred by TI

   function Selector (TI : Task_Info)
     return Asynch_Ev.Asynch_Ev_Monitor_Access;
   pragma Inline (Selector);
   --  Return Selector the task referred by TI is blocked on

   function Condition (TI : Task_Info)
     return PTCV.Condition_Access;
   pragma Inline (Condition);
   --  Return Condition Variable the Task referred by TI is blocked on

   procedure Set_Id (TI : in out Task_Info);
   pragma Inline (Set_Id);
   --  Task_Info will hold Id of the current task, as provided by
   --  PolyORB tasking runtime.

   procedure Set_Polling
     (TI       : in out Task_Info;
      May_Poll :        Boolean);
   pragma Inline (Set_Polling);
   --  Set if TI may poll on event sources, i.e. be in blocked state

   function May_Poll (TI : Task_Info) return Boolean;
   pragma Inline (May_Poll);
   --  Returns true iff TI may poll, i.e. be in blocked state

   procedure Set_Exit_Condition
     (TI             : in out Task_Info;
      Exit_Condition :        PolyORB.Types.Boolean_Ptr);
   pragma Inline (Set_Exit_Condition);
   --  Attach Exit_Condition to TI

   function Exit_Condition (TI : Task_Info)
     return Boolean;
   pragma Inline (Exit_Condition);
   --  Return the value of TI's exit condition

   procedure Request_Abort_Polling (TI : in out Task_Info);
   pragma Inline (Request_Abort_Polling);
   --  Request TI to abort polling. Meaningful only if TI is in
   --  blocked state.

   function Abort_Polling (TI : Task_Info) return Boolean;
   pragma Inline (Abort_Polling);
   --  Return true if TI must abort polling and leave blocked state.
   --  Meaningful only if TI is in blocked state.

   function Image (TI : Task_Info) return String;
   pragma Inline (Image);
   --  For debug purposes

private

   type Task_Info (Kind : Task_Kind) is record

      Id        : PolyORB.Tasking.Threads.Thread_Id;
      --  Task referred by Task_Info record

      State    : Task_State := Unscheduled;
      --  Current Task status

      May_Poll : Boolean := False;
      --  True iff task may poll on event sources

      Abort_Polling : Boolean := False;
      --  True iff must abort polling

      Exit_Condition : PolyORB.Types.Boolean_Ptr := null;
      --  Exit condition; meaningful only when Kind = Transient

      Selector  : Asynch_Ev.Asynch_Ev_Monitor_Access;
      --  Monitor on which Task referred by Id is blocked;
      --  meaningful only when State is Blocked.

      Condition : Tasking.Condition_Variables.Condition_Access;
      --  Condition Variable on which Task referred by Id is
      --  blocked; meaningful only when State is Idle.

   end record;

end PolyORB.Task_Info;
