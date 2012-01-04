------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T A S K I N G . C O N D I T I O N _ V A R I A B L E S   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Implementation of POSIX-like condition variables

--  A complete implementation of this package is provided for each
--  tasking profile.

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
     (C : access Condition_Type;
      M : access Mutex_Type'Class)
      is abstract;
   --  Wait for a notification on condition variable C.
   --  This procedure atomically:
   --    (1) leaves the critical section protected by M (which the
   --         caller must own);
   --    (2) blocks until a subsequent notification from another
   --        task (via the Signal or Broadcast operations described below);
   --  On return, M is owned again.

   procedure Broadcast (C : access Condition_Type) is abstract;
   --  Unblock all tasks blocked on C

   procedure Signal (C : access Condition_Type) is abstract;
   --  Unblock one task blocked on C

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
     (MF   : access Condition_Factory_Type;
      Cond : in out Condition_Access)
     is abstract;
   --  Destroy Cond, or just release it if it was preallocated.

   procedure Register_Condition_Factory
     (MF : Condition_Factory_Access);
   --  Register the factory corresponding to the chosen tasking profile.

   procedure Create (Cond : out Condition_Access; Name : String := "");
   procedure Destroy (Cond : in out Condition_Access);

private

   type Condition_Type is abstract tagged limited null record;

end PolyORB.Tasking.Condition_Variables;
