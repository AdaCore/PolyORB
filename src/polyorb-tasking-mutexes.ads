------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T A S K I N G . M U T E X E S               --
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

--  This package provides mutual exclusion objects (mutexes).

--  $Id$

with PolyORB.Tasking.Monitors;
with PolyORB.Tasking.Threads;

package PolyORB.Tasking.Mutexes is

   pragma Preelaborate;

   type Adv_Mutex_Type is limited private;
   --  This is a classical mutual exclusion object except that when a
   --  task try to Enter a mutex several times without leaving it
   --  first, it is not blocked and can continue. Leave keeps track of
   --  the number of times Enter has been successful, and must be called
   --  the number of times that Enter has been called to free the lock.

   --  Example :
   --
   --  Enter (My_Mutex);
   --  --  Enter the first critical section.
   --
   --  Enter (My_Mutex);
   --  --  Enter the second critical section.
   --
   --  Do_Some_Stuff;
   --
   --  Leave (My_Mutex);
   --  --  Leave the second critical section and keep the lock.
   --
   --  Enter (My_Mutex);
   --  --  Reenter the second critical section.
   --
   --  Leave (My_Mutex);
   --  --  Leave the second critical section.
   --
   --  Leave (My_Mutex);
   --  --  Leave the first critical section and free the lock.

   type Adv_Mutex_Access is access all Adv_Mutex_Type;

   procedure Enter (M : in out Adv_Mutex_Type);
   --  If the lock is free, or if the current task has it, get the
   --  lock and continue, entering a new critical section; else, wait
   --  until it is free.

   procedure Leave (M : in out Adv_Mutex_Type);
   --  The current tasks exit of the current critical section. If it is
   --  the first critical section opened by the task, free the lock.

   procedure Create (M : in out Adv_Mutex_Type);
   --  Create an advanced mutex.  The object must have been allocated
   --  by the client of this package.

   procedure Destroy (M : in out Adv_Mutex_Type);
   --  Destroy the advanced mutex.  The deallocation, if needed, after
   --  "Destroy" and is the responsability of the client of this
   --  package.

private
   use PolyORB.Tasking.Monitors;

   type Adv_Mutex_Condition_Type is new Condition_Type with record
      Passing : Boolean := False;
   end record;
   --  Type of the Conditions used by the algorithm of Enter/Leave.
   --  Simple boolean condition.

   procedure Evaluate (C : in out Adv_Mutex_Condition_Type;
                       B : out Boolean);
   --  Return the value of the internal boolean.

   type Adv_Mutex_Type is record
      Empty   : Boolean;
      pragma Atomic (Empty);
      --  If no theres is no owner for this Mutex, True. else, False.

      Current : Threads.Thread_Id_Access;
      pragma Atomic (Current);
      --  Identity of the thread owning the mutex.

      Level     : Natural;
      pragma Atomic (Level);
      --  Number of time the Thread owning the Id enter the mutex
      --  minus the number of calls to Leave.

      M        : Monitors.Monitor_Access;
      --  The monitors used for the synchronisation in the watcher.

      Await_Count           : Integer := 0;
      --  Number of tasks waiting on Enter.



      Passing_Condition     : aliased Adv_Mutex_Condition_Type;
      --  Conditions used by the algorithm

   end record;

end PolyORB.Tasking.Mutexes;
