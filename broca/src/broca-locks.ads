------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                          B R O C A . L O C K S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Task_Identification; use Ada.Task_Identification;

with Broca.Soft_Links;

package Broca.Locks is

   --------------------
   -- A simple mutex --
   --------------------

   --  Only one task can own the lock, with LOCK and release it with UNLOCK.
   --  Only the task that has acquired the lock can release it.
   --  A task can try to own the lock via TRYLOCK, which must be a non blocking
   --  call.  If the lock is free, the current task owns the lock and return
   --  true.
   --  CHECK_OWNER raise program_error is the calling task is not the owner of
   --  the lock.
   --  At the creation, the lock is not owned.
   protected type Mutex_Type is
      entry Lock;
      procedure Unlock;
      procedure Check_Owner;
      procedure Trylock (Success : out Boolean);
   private
      --  Contains the owner of the lock.
      --  This is a checking lock:
      --  program_error is raised if a task other than the owner release the
      --  lock.
      Owner : Task_Id := Null_Task_Id;
   end Mutex_Type;

   ----------------------------
   -- A readers/writers lock --
   ----------------------------

   --  Several tasks can own the lock in R (read) mode.
   --  Only one task can own the lock in W (write) mode.
   --  If there is a request for writing, requests for reading are not accepted
   --  anymore.

   type Rw_Lock_Type is limited private;
   type Rw_Lock_Access is access all Rw_Lock_Type;

   procedure Create (L : out Rw_Lock_Access);
   procedure Destroy (L : in out Rw_Lock_Access);

   procedure Lock_W (L : access Rw_Lock_Type);
   procedure Lock_R (L : access Rw_Lock_Type);
   procedure Unlock_W (L : access Rw_Lock_Type);
   procedure Unlock_R (L : access Rw_Lock_Type);
   procedure Set_Max_Count
     (L : access Rw_Lock_Type;
      Max : Natural);

   ----------------------
   -- A broadcast lock --
   ----------------------

   --  If the object is locked (through a call to LOCK), WAIT is blocking.
   --  UNLOCK deblocks all the pending calls.

   protected type Bcast_Lock_Type is
      entry Wait;
      procedure Lock;
      procedure Unlock;
   private
      --  Current state.
      Locked : Boolean := True;
   end Bcast_Lock_Type;

private

   use Broca.Soft_Links;

   type Rw_Lock_Type is limited record
      Readers_Barrier : Barrier_Access;
      Writers_Barrier : Barrier_Access;
      Readers_Waiting : Natural := 0;
      Writers_Waiting : Natural := 0;

      Serial : Integer := 0;

      Count : Integer := 0;
      --  Current readers, or -1 if held for writing.
      --  If COUNT > 0, it is the number of tasks owning the lock in R mode.
      --  If COUNT = 0, no tasks own the lock.
      --  If COUNT = -1, a task is owning the lock in W mode.

      Max_Count : Natural := Natural'Last;
         --  Maximum number of readers.
   end record;

end Broca.Locks;
