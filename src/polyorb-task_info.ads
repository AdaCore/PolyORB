------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T A S K _ I N F O                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Information about running ORB tasks.
--  This packages is used to store and retrieve information
--  concerning the status of tasks that execute ORB functions.

--  $Id$

with PolyORB.Asynch_Ev;
with PolyORB.Tasking.Soft_Links;

package PolyORB.Task_Info is

   pragma Elaborate_Body;

   type Task_Kind is (Permanent, Transient);
   --  A Permanent task executes ORB.Run indefinitely.
   --  A Transient task executes ORB.Run until a given condition
   --  is met. Transient tasks are lent to the middleware by
   --  user activities.

   type Task_Status is (Running, Blocked, Idle);
   --  A Running task is executing an ORB activity.
   --  A Blocked task is waiting for an external
   --  asynchronous event.
   --  An Idle task is waiting on a watcher expecting another
   --  task to request ORB action.

   type Task_Info (Kind : Task_Kind) is limited private;
   type Task_Info_Access is access all Task_Info;

   ------------------------------------
   -- Task_Info components accessors --
   ------------------------------------

   procedure Set_Status_Blocked
     (TI       : in out Task_Info;
      Selector : Asynch_Ev.Asynch_Ev_Monitor_Access);
   pragma Inline (Set_Status_Blocked);

   procedure Set_Status_Idle
     (TI      : in out Task_Info;
      Watcher : PolyORB.Tasking.Soft_Links.Watcher_Access);
   pragma Inline (Set_Status_Idle);

   procedure Set_Status_Running
     (TI : in out Task_Info);
   pragma Inline (Set_Status_Running);

   function Status (TI : Task_Info)
     return Task_Status;
   pragma Inline (Status);

   function Selector (TI : Task_Info)
     return Asynch_Ev.Asynch_Ev_Monitor_Access;
   pragma Inline (Selector);

   function Watcher (TI : Task_Info)
     return PolyORB.Tasking.Soft_Links.Watcher_Access;
   pragma Inline (Watcher);

private

   type Task_Info (Kind : Task_Kind) is record
      Status : Task_Status := Running;

      Selector : Asynch_Ev.Asynch_Ev_Monitor_Access;
      --  Meaningful only when Status = Blocked

      Watcher  : PolyORB.Tasking.Soft_Links.Watcher_Access;
      --  Meaningful only when Status = Idle
   end record;

end PolyORB.Task_Info;
