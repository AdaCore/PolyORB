------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B - T A S K I N G - C O N D I T I O N _ V A R I A B L E S   --
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

--  Implementation of a POSIX-like condition variables

--  A complete implementation of this package is provided for each
--  tasking profile.

--  $Id$

with PolyORB.Tasking.Mutexes;

package PolyORB.Tasking.Condition_Variables is

   pragma Preelaborate;

   use PolyORB.Tasking.Mutexes;

   ----------------
   -- Conditions --
   ----------------

   type Condition_Type is abstract tagged limited private;
   type Condition_Access is access all Condition_Type'Class;
   --  Type for condition variables.

   procedure Wait
     (C : in out Condition_Type;
      M : access Mutex_Type'Class)
      is abstract;
   --  Wait for a notification on condition variable C.
   --  This procedure atomically:
   --    (1) leaves the critical section protected by M (which the
   --         caller must own);
   --    (2) blocks until a subsequent notification from another
   --        task (via the Signal or Broadcast operations described below);
   --  On return, M is owned again.

   procedure Broadcast
     (C : in out Condition_Type)
      is abstract;
   --  Unblock all tasks blocked on C.

   procedure Signal
     (C : in out Condition_Type)
      is abstract;
   --  Unblock one task blocked on C.

   -----------------------
   -- Condition_Factory --
   -----------------------

   type Condition_Factory_Type is abstract tagged limited null record;
   --  Factory of condition variables.
   --  A subclass of this factory exists for every tasking profile:
   --  Full Tasking, Ravenscar and No Tasking.

   type Condition_Factory_Access is access all Condition_Factory_Type'Class;

   function Create
     (MF   : access Condition_Factory_Type;
      Name : String := "")
     return Condition_Access
      is abstract;
   --  Create a new condition variable, or get a preallocated one.
   --  Name will be used to get the configuration of this
   --  condition variable from the configuration module.

   procedure Destroy
     (MF : in out Condition_Factory_Type;
      C  : in out Condition_Access)
     is abstract;
   --  Destroy C, or just release it if it was preallocated.

   function Get_Condition_Factory
     return Condition_Factory_Access;
   pragma Inline (Get_Condition_Factory);
   --  Get the Condition_Factory object registered in this package.

   procedure Register_Condition_Factory
     (MF : Condition_Factory_Access);
   --  Register the factory corresponding to the chosen tasking profile.

private

   type Condition_Type is abstract tagged limited null record;

end PolyORB.Tasking.Condition_Variables;
