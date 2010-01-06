------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . T A S K I N G . A D V A N C E D _ M U T E X E S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

--  This package provides advanced mutual exclusion objects (mutexes).

with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Threads;

package PolyORB.Tasking.Advanced_Mutexes is

   pragma Preelaborate;

   type Adv_Mutex_Type is limited private;
   --  This is a classical mutual exclusion object except that it allows
   --  nested critical sections; that is, when a task tries to Enter a mutex
   --  several times without leaving it first, it is not blocked and can
   --  continue. Leave keeps track of the number of times Enter has been
   --  successful, and must be called the number of times that Enter has been
   --  called to free the lock.

   --  Example (assuming all calls below are done by one task):
   --
   --  Enter (My_Mutex);
   --  --  Enter the critical section.
   --
   --  Enter (My_Mutex);
   --  --  Enter the critical section again.
   --
   --  Do_Some_Stuff;
   --
   --  Leave (My_Mutex);
   --  --  Keep the lock.
   --
   --  Enter (My_Mutex);
   --  --  Reenter.
   --
   --  Leave (My_Mutex);
   --  --  Still keep the lock
   --
   --  Leave (My_Mutex);
   --  --  Leave the critical section and free the lock.

   type Adv_Mutex_Access is access all Adv_Mutex_Type;

   procedure Create (M : out Adv_Mutex_Access);
   --  Create an advanced mutex.  The object must have been allocated
   --  by the client of this package.

   procedure Destroy (M : in out Adv_Mutex_Access);
   --  Destroy the advanced mutex.  The deallocation, if needed, after
   --  "Destroy" and is the responsability of the client of this
   --  package.

   procedure Enter (M : access Adv_Mutex_Type);
   --  If the lock is free, or if the current task has it, get the
   --  lock and continue, entering a new critical section; else, wait
   --  until it is free.

   procedure Leave (M : access Adv_Mutex_Type);
   --  The current tasks exit of the current critical section. If it is
   --  the first critical section opened by the task, free the lock.

private
   package PTM renames PolyORB.Tasking.Mutexes;

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   type Adv_Mutex_Type is record
      Empty   : Boolean;
      pragma Atomic (Empty);
      --  If no there is no owner for this Mutex, True. else, False.

      Current     : Threads.Thread_Id;
      pragma Atomic (Current);
      --  Identity of the thread owning the mutex.

      Level       : Natural;
      pragma Atomic (Level);
      --  Number of times the Thread owning the Id enter the mutex
      --  minus the number of calls to Leave.

      Await_Count : Integer := 0;
      --  Number of tasks waiting on Enter.

      MMutex      : PTM.Mutex_Access;

      MCondition  : PTCV.Condition_Access;

      Passing     : Boolean;

   end record;

end PolyORB.Tasking.Advanced_Mutexes;
