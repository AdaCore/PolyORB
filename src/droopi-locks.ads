--  Inter-process synchronisation objects.

--  $Id: //droopi/main/src/droopi-locks.ads#2 $

with Droopi.Soft_Links;

package Droopi.Locks is

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

private

   use Droopi.Soft_Links;

   type Rw_Lock_Type is limited record
      Guard_Values : Watcher_Access;
      --  This watcher is updated each time an attribute
      --  of the Rw_Lock_Type that is used in a guard clause
      --  is changed.

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

end Droopi.Locks;
