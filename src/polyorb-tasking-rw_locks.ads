------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . R W _ L O C K S              --
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

--  Inter-process synchronisation objects.

with PolyORB.Tasking.Condition_Variables;

package PolyORB.Tasking.Rw_Locks is

   pragma Preelaborate;

   ----------------------------
   -- A readers/writers lock --
   ----------------------------

   --  Several tasks can own the lock in read (shared) mode.
   --  Only one task can own the lock in write (exclusive) mode.
   --  No task can own the lock in exclusive mode while any other
   --  task owns it in read mode.

   --  The contention resolution policy gives priority to
   --  writers: as long as a task is blocked waiting for
   --  write access, requests for read access are declined.

   type Rw_Lock_Type is limited private;
   type Rw_Lock_Access is access all Rw_Lock_Type;

   procedure Create (L : out Rw_Lock_Access);
   procedure Destroy (L : in out Rw_Lock_Access);

   procedure Lock_W (L : access Rw_Lock_Type);
   --  Get lock in write mode.

   procedure Lock_R (L : access Rw_Lock_Type);
   --  Get lock in read mode.

   procedure Unlock_W (L : access Rw_Lock_Type);
   --  Release write mode lock.

   procedure Unlock_R (L : access Rw_Lock_Type);
   --  Release read mode lock.

   function Is_Set_W (L : access Rw_Lock_Type) return Boolean;
   --  Return True if the lock is held in write mode.

   function Is_Set_R (L : access Rw_Lock_Type) return Boolean;
   --  Return True if the lock is held in read mode.

   procedure Set_Max_Count
     (L : access Rw_Lock_Type;
      Max : Natural);
   --  Set maximum number of readers.

private

   type Rw_Lock_Type is limited record
      Guard_Values : Tasking.Condition_Variables.Condition_Access;
      --  This condition is signalled each time an attribute
      --  of the Rw_Lock_Type that is used in a guard clause
      --  is changed.

      Readers_Waiting : Natural := 0;
      Writers_Waiting : Natural := 0;

      Serial : Integer := 0;
      --  Debug information, Rw_Lock identifier.

      Count : Integer := 0;
      --  Current readers, or -1 if held for writing.
      --  If Count >  0, it is the number of tasks owning the lock in R mode.
      --     Count =  0, no tasks own the lock.
      --     Count = -1, a task is owning the lock in W mode.

      Max_Count : Natural := Natural'Last;
      --  Maximum number of readers.
   end record;

   pragma Inline (Is_Set_W);
   pragma Inline (Is_Set_R);

end PolyORB.Tasking.Rw_Locks;
