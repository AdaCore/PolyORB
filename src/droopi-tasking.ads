--  $Id$

with Droopi.Soft_Links;

package Droopi.Tasking is

   pragma Elaborate_Body;

   procedure Initialize;

   -------------------------------------------
   -- Critical Section for ORB with tasking --
   -------------------------------------------

   procedure Enter_Critical_Section;

   procedure Leave_Critical_Section;

   ----------------------------------
   -- Barrier for ORB with Tasking --
   ----------------------------------

   type Protected_Barrier_Type is new Soft_Links.Barrier_Type with private;

   function Create return Soft_Links.Barrier_Access;

   procedure Destroy (B : in out Protected_Barrier_Type);

   procedure Signal
     (B : in Protected_Barrier_Type;
      N : in Positive := 1);

   procedure Signal_All
     (B : in Protected_Barrier_Type;
      P : in Boolean := True);

   procedure Wait (B : in Protected_Barrier_Type);

   --------------------------------
   -- Mutex for PCS with Tasking --
   --------------------------------

   type Protected_Mutex_Type is new Soft_Links.Mutex_Type with private;

   function Create return Soft_Links.Mutex_Access;

   procedure Enter (M : in Protected_Mutex_Type);

   procedure Destroy (M : in out Protected_Mutex_Type);

   procedure Leave (M : in Protected_Mutex_Type);

   ----------------------------------
   -- Watcher for PCS with Tasking --
   ----------------------------------

   type Protected_Watcher_Type is new Soft_Links.Watcher_Type with private;

   function Create return Soft_Links.Watcher_Access;

   procedure Destroy (W : in out Protected_Watcher_Type);

   procedure Differ
     (W : in Protected_Watcher_Type;
      V : in Soft_Links.Version_Id);

   procedure Lookup
     (W : in Protected_Watcher_Type;
      V : out Soft_Links.Version_Id);

   procedure Update (W : in Protected_Watcher_Type);

   -----------------------------------------
   -- Advanced Mutex for PCS with Tasking --
   -----------------------------------------

   type Protected_Adv_Mutex_Type is
     new Soft_Links.Adv_Mutex_Type with private;

   function Create return Soft_Links.Adv_Mutex_Access;

   procedure Enter (M : in Protected_Adv_Mutex_Type);

   procedure Destroy (M : in out Protected_Adv_Mutex_Type);

   procedure Leave (M : in Protected_Adv_Mutex_Type);

   -------------------------
   -- Task identification --
   -------------------------

   type No_Task_Id is new Soft_Links.Task_Id with private;

   function Get_Current_Task return Soft_Links.Task_Id'Class;
   function Get_Null_Task return Soft_Links.Task_Id'Class;

   function Image (T : No_Task_Id) return String;
   pragma Inline (Image);

   function To_Integer (T : No_Task_Id) return Integer;
   pragma Inline (To_Integer);

private

   protected type Basic_Mutex is
      entry Enter;
      procedure Leave;
   private
      Taken : Boolean := False;
   end Basic_Mutex;

   type Basic_Mutex_Access is access Basic_Mutex;

   type Protected_Mutex_Type is
     new Soft_Links.Mutex_Type with record
        The_Mutex : Basic_Mutex_Access;
     end record;

   --  ??? All of the following still have to be implemented.

   type Protected_Adv_Mutex_Type is
     new Soft_Links.Adv_Mutex_Type with null record;

   type Protected_Barrier_Data is record
      Free : Natural := 0;
      Perm : Boolean := False;
   end record;

   type Protected_Barrier_Data_Access is
     access all Protected_Barrier_Data;

   type Protected_Barrier_Type is
     new Soft_Links.Barrier_Type with
      record
         X : Protected_Barrier_Data_Access;
      end record;

   type Protected_Watcher_Data is record
      Version : Soft_Links.Version_Id;
   end record;

   type Protected_Watcher_Data_Access is
     access all Protected_Watcher_Data;

   type Protected_Watcher_Type is
     new Soft_Links.Watcher_Type with
      record
         X : Protected_Watcher_Data_Access;
      end record;

   type No_Task_Id is new Soft_Links.Task_Id with record
      Is_Null : Boolean := True;
   end record;

end Droopi.Tasking;
