--  $Id$

with Ada.Task_Identification;

with Droopi.Soft_Links;

package Droopi.Protected_Objects is

   pragma Elaborate_Body;

   procedure Initialize;

   -------------------------------------------
   -- Critical Section for ORB with Tasking --
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

   type Protected_Adv_Mutex_Type is new Soft_Links.Adv_Mutex_Type with private;

   function Create return Soft_Links.Adv_Mutex_Access;

   procedure Enter (M : in Protected_Adv_Mutex_Type);

   procedure Destroy (M : in out Protected_Adv_Mutex_Type);

   procedure Leave (M : in Protected_Adv_Mutex_Type);

   -------------------------
   -- Task identification --
   -------------------------

   type PO_Task_Id is new Soft_Links.Task_Id with private;

   function Get_Current_Task return Soft_Links.Task_Id'Class;

   function Image (T : PO_Task_Id) return String;
   pragma Inline (Image);

private

   type Barrier_PO;

   type Barrier_PO_Access is access Barrier_PO;

   type Protected_Barrier_Type is new Soft_Links.Barrier_Type
     with record
        X : Barrier_PO_Access;
     end record;


   type Mutex_PO;

   type Mutex_PO_Access is access Mutex_PO;

   type Protected_Mutex_Type is new Soft_Links.Mutex_Type
     with record
        X : Mutex_PO_Access;
     end record;

   type Watcher_PO;

   type Watcher_PO_Access is access Watcher_PO;

   type Protected_Watcher_Type is new Soft_Links.Watcher_Type
     with record
        X : Watcher_PO_Access;
     end record;


   type Adv_Mutex_PO;

   type Adv_Mutex_PO_Access is access Adv_Mutex_PO;

   type Protected_Adv_Mutex_Type is new Soft_Links.Adv_Mutex_Type
     with record
        X : Adv_Mutex_PO_Access;
     end record;

   type PO_Task_Id is new Soft_Links.Task_Id
     with record
        X : Ada.Task_Identification.Task_Id;
     end record;

end Droopi.Protected_Objects;
