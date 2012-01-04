------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . T A S K I N G . M U T E X E S               --
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

--  Implementation of a POSIX-like mutexes

--  A complete implementation of this package is provided for all
--  tasking profiles.

package PolyORB.Tasking.Mutexes is

   pragma Preelaborate;

   -------------
   -- Mutexes --
   -------------

   type Mutex_Type is abstract tagged limited private;
   type Mutex_Access is access all Mutex_Type'Class;
   --  Mutual exclusion locks (mutexes) prevent multiple threads from
   --  simultaneously executing critical sections of code which access shared
   --  data (that is, mutexes are used to serialize the execution of
   --  threads). While a thread is in the critical section protected by a
   --  mutex, it is designated as the owner of that mutex.

   procedure Enter (M : access Mutex_Type) is abstract;
   --  A call to Enter locks mutex object M. If M is already locked,
   --  the caller is blocked until it gets unlocked. On exit from Enter,
   --  M is locked, and the caller is the owner.
   --  If the current owner of a mutex tries to enter it again, a
   --  deadlock occurs.

   procedure Leave (M : access Mutex_Type) is abstract;
   --  Release M. M must be locked, and the caller must be the owner.
   --  The scheduling policy determines which blocked thread is awakened
   --  next and obtains the mutex.
   --  It is erroneous for any process other than the owner of a mutex
   --  to invoke Leave.

   -------------------
   -- Mutex_Factory --
   -------------------

   type Mutex_Factory_Type is abstract tagged limited null record;
   --  This type is a factory for the mutex type.
   --  A subclass of this factory exists for every tasking profile:
   --  Full Tasking, Ravenscar and No Tasking.
   --  This type provides functionalities for creating mutexes
   --  corresponding to the chosen tasking profile.

   type Mutex_Factory_Access is access all Mutex_Factory_Type'Class;

   function Create
     (MF   : access Mutex_Factory_Type;
      Name : String := "")
     return Mutex_Access
      is abstract;
   --  Create a new mutex, or get a preallocated one.
   --  Name will be used to get the configuration of this
   --  mutex from the configuration module.

   procedure Destroy
     (MF : access Mutex_Factory_Type;
      M  : in out Mutex_Access)
     is abstract;
   --  Destroy M, or just release it if it was preallocated.

   procedure Register_Mutex_Factory
     (MF : Mutex_Factory_Access);
   --  Register the factory corresponding to the chosen tasking profile.

   procedure Create (M : out Mutex_Access; Name : String := "");
   procedure Destroy (M : in out Mutex_Access);

private

   type Mutex_Type is abstract tagged limited null record;

end PolyORB.Tasking.Mutexes;
