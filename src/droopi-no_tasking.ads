--  $Id$

with Droopi.Soft_Links;

package Droopi.No_Tasking is

   pragma Elaborate_Body;

   procedure Initialize;

   ----------------------------------------------
   -- Critical Section for ORB with no tasking --
   ----------------------------------------------

   procedure Enter_Critical_Section;

   procedure Leave_Critical_Section;

   ----------------------------------
   -- Barrier for ORB with Tasking --
   ----------------------------------

   type Unprotected_Barrier_Type is new Soft_Links.Barrier_Type with private;

   function Create return Soft_Links.Barrier_Access;

   procedure Destroy (B : in out Unprotected_Barrier_Type);

   procedure Signal
     (B : in Unprotected_Barrier_Type;
      N : in Positive := 1);

   procedure Signal_All
     (B : in Unprotected_Barrier_Type;
      P : in Boolean := True);

   procedure Wait (B : in Unprotected_Barrier_Type);

   --------------------------------
   -- Mutex for PCS with Tasking --
   --------------------------------

   type Unprotected_Mutex_Type is new Soft_Links.Mutex_Type with private;

   function Create return Soft_Links.Mutex_Access;

   procedure Enter (M : in Unprotected_Mutex_Type);

   procedure Destroy (M : in out Unprotected_Mutex_Type);

   procedure Leave (M : in Unprotected_Mutex_Type);

   ----------------------------------
   -- Watcher for PCS with Tasking --
   ----------------------------------

   type Unprotected_Watcher_Type is new Soft_Links.Watcher_Type with private;

   function Create return Soft_Links.Watcher_Access;

   procedure Destroy (W : in out Unprotected_Watcher_Type);

   procedure Differ
     (W : in Unprotected_Watcher_Type;
      V : in Soft_Links.Version_Id);

   procedure Lookup
     (W : in Unprotected_Watcher_Type;
      V : out Soft_Links.Version_Id);

   procedure Update (W : in Unprotected_Watcher_Type);

   -----------------------------------------
   -- Advanced Mutex for PCS with Tasking --
   -----------------------------------------

   type Unprotected_Adv_Mutex_Type is
     new Soft_Links.Adv_Mutex_Type with private;

   function Create return Soft_Links.Adv_Mutex_Access;

   procedure Enter (M : in Unprotected_Adv_Mutex_Type);

   procedure Destroy (M : in out Unprotected_Adv_Mutex_Type);

   procedure Leave (M : in Unprotected_Adv_Mutex_Type);

private
   type Unprotected_Mutex_Type is
     new Soft_Links.Mutex_Type with null record;

   type Unprotected_Adv_Mutex_Type is
     new Soft_Links.Adv_Mutex_Type with null record;

   type Unprotected_Barrier_Data is record
      Free : Natural := 0;
      Perm : Boolean := False;
   end record;

   type Unprotected_Barrier_Data_Access is
     access all Unprotected_Barrier_Data;

   type Unprotected_Barrier_Type is
     new Soft_Links.Barrier_Type with
      record
         X : Unprotected_Barrier_Data_Access;
      end record;

   type Unprotected_Watcher_Data is record
      Version : Soft_Links.Version_Id;
   end record;

   type Unprotected_Watcher_Data_Access is
     access all Unprotected_Watcher_Data;

   type Unprotected_Watcher_Type is
     new Soft_Links.Watcher_Type with
      record
         X : Unprotected_Watcher_Data_Access;
      end record;

end Droopi.No_Tasking;
