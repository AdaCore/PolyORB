------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . M O N I T O R S              --
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

--  Implementation of monitors. Monitors are objects which can only be accessed
--  by one task at the time. In these objects, you can wait for some conditions
--  to be fulfilled; The evaluation of these conditions are done when the
--  monitor is signaled.

--  A complete implementation of this package is provided for all
--  tasking profiles.

--  $Id$

package PolyORB.Tasking.Monitors is

   pragma Preelaborate;

   ----------------
   -- Conditions --
   ----------------

   type Condition_Type is abstract tagged private;
   type Condition_Access is access all Condition_Type'Class;
   --  Type for boolean conditions. Subclasses should be used in
   --  evoluted synchronisation objects.

   procedure Evaluate (C : in out Condition_Type; B : out Boolean)
      is abstract;
   --  Evaluate the condition.

   -------------
   -- Monitor --
   -------------

   type Monitor_Type is abstract tagged limited private;
   type Monitor_Access is access all Monitor_Type'Class;
   --  Type for monitors. Only one tasks at the time can enter the
   --  monitor. The others waits on "Enter" until the task inside leaves
   --  the monitor, or calls "Wait".  When a task call "Wait", she waits
   --  for a condition to be true. The condition is reevaluated when a
   --  task makes a call to "Signal".

   --  Example of use:
   --
   --  Enter (My_Monitor);
   --  --  Enter critical section, the other tasks can't access
   --  --  critical sections.
   --
   --  Do_Some_Work;
   --  --  procedure call under mutual exclusion.
   --
   --  Wait (My_Monitor, My_Condition);
   --  --  Wait, the other tasks can access critical section.
   --  --  When the condition is fulfilled, the task that was waiting
   --  --  re-take the lock.
   --
   --  Do_Some_Work;
   --  --  procedure call under mutual exclusion.
   --
   --  Leave (My_Monitor);
   --  --  The other tasks can enter the monitor.

   procedure Enter (M : in out Monitor_Type)
      is abstract;
   --  Enter critical section.

   procedure Leave (M : in out Monitor_Type)
      is abstract;
   --  Leave critical section.

   procedure Wait
     (M : in out Monitor_Type;
      C : access Condition_Type'Class)
      is abstract;
   --  A call to Wait must be done in a critical section, i.e. after
   --  a call to Enter and before the corresponding call to leave.
   --  If it doesn't, a Wait_Exception is raised.
   --  When a task calls Wait, it tests if the Condition is
   --  fulfilled by a call to Evaluate. If it is, it continues its
   --  execution. If it is not, it frees the lock of the Monitor
   --  and waits that another task call to Signal. When the
   --  Monitor is signaled, the waiting tasks evaluates their condition;
   --  If they are fulfilled, they take the lock, one after the others,
   --  according to the policy chosen in the tasking profile,
   --  and continue their executions.

   Wait_Exception : exception;
   --  This exception is raised when a call to Wait is done
   --  outside of the critical section of the Monitor.

   procedure Signal
     (M : in out Monitor_Type)
      is abstract;
   --  After a call to Signal, every task waiting in the monitor reevaluates
   --  their conditions.

   ---------------------
   -- Monitor_Factory --
   ---------------------

   type Monitor_Factory_Type is abstract tagged limited null record;
   --  This type is a factory for the Monitor type.
   --  A subclass of this factory exists for every tasking profile:
   --  Full Tasking, Ravenscar and No Tasking.
   --  This type provides functionalities for creating Monitors
   --  corresponding with the chosen tasking profile.

   type Monitor_Factory_Access is access all Monitor_Factory_Type'Class;

   function Create
     (MF   : access Monitor_Factory_Type;
      Name : String := "")
     return Monitor_Access
      is abstract;
   --  Create a new monitor, or get a preallocated one.
   --  Name will be used to get the configuration of this
   --  Monitor from the configuration module.

   procedure Destroy
     (MF : in out Monitor_Factory_Type;
      M  : in out Monitor_Access)
     is abstract;
   --  Destroy M, or just release it if it was preallocated.

   function Get_Monitor_Factory
     return Monitor_Factory_Access;
   pragma Inline (Get_Monitor_Factory);
   --  Get the Monitor_Factory object registered in this package.

   procedure Register_Monitor_Factory
     (MF : Monitor_Factory_Access);
   --  Register the factory corresponding to the chosen tasking profile.

private

   type Monitor_Type is abstract tagged limited null record;

   type Condition_Type is abstract tagged null record;

end PolyORB.Tasking.Monitors;
