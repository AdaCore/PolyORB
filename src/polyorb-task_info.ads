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

package PolyORB.Task_Info is

   pragma Elaborate_Body;

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   type Task_Kind is (Permanent, Transient);
   --  A Permanent task executes ORB.Run indefinitely.
   --  A Transient task executes ORB.Run until a given condition is
   --  met. Transient tasks are lent to neutral core middleware by
   --  user activities.

   type Task_Status is (Running, Blocked, Idle);
   --  A Running task is executing an ORB activity.
   --  A Blocked task is waiting for an external
   --  asynchronous event.
   --  An Idle task is waiting on a condition variable expecting
   --  another task to request ORB action.

   type Task_Info (Kind : Task_Kind) is limited private;
   --  Task Info holds information on tasks that run ORB.Run

   type Task_Info_Access is access all Task_Info;

   ------------------------------------
   -- Task_Info components accessors --
   ------------------------------------

   procedure Set_Status_Blocked
     (TI       : in out Task_Info;
      Selector :        Asynch_Ev.Asynch_Ev_Monitor_Access);
   pragma Inline (Set_Status_Blocked);
   --  The task refereed by TI will be blocked on 'Selector'

   procedure Set_Status_Idle
     (TI        : in out Task_Info;
      Condition :        PTCV.Condition_Access);
   pragma Inline (Set_Status_Idle);
   --  The task refereed by TI will go Idle;
   --  signaling condition variable 'Condition' will awake it.

   procedure Set_Status_Running
     (TI : in out Task_Info);
   pragma Inline (Set_Status_Running);
   --  The task refereed by TI is now running;
   --  this procedure resets Selector or Conditions it was blocked on.

   function Status (TI : Task_Info)
     return Task_Status;
   pragma Inline (Status);
   --  Status for Task referred by TI

   function Selector (TI : Task_Info)
     return Asynch_Ev.Asynch_Ev_Monitor_Access;
   pragma Inline (Selector);
   --  Return Selector the Task referred by TI is blocked on

   function Condition (TI : Task_Info)
     return PTCV.Condition_Access;
   pragma Inline (Condition);
   --  Return Condition Variable the Task referred by TI is blocked on

   procedure Set_Id (TI : in out Task_Info);
   pragma Inline (Set_Id);
   --  Tazk_Info will hold Id of the current task, as provided by
   --  PolyORB tasking runtime.

   function Image (TI : Task_Info) return String;
   pragma Inline (Image);
   --  For debug purposes

private

   type Task_Info (Kind : Task_Kind) is record

      Id        : PolyORB.Tasking.Threads.Thread_Id;
      --  Task referred by Task_Info record

      Status    : Task_Status := Running;
      --  Current Task status

      Selector  : Asynch_Ev.Asynch_Ev_Monitor_Access;
      --  Monitor on which Task referred by 'Id' is blocked;
      --  meaningful only when Status = Blocked.

      Condition : Tasking.Condition_Variables.Condition_Access;
      --  Condition Variable on which Task referred by 'Id' is
      --  blocked; meaningful only when Status = Idle.
   end record;

end PolyORB.Task_Info;
