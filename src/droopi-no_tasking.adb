--  $Id$

with Ada.Unchecked_Deallocation;

with Droopi.Soft_Links;  use Droopi.Soft_Links;

package body Droopi.No_Tasking is

   ------------
   -- Create --
   ------------

   function Create return Watcher_Access is
      W : Unprotected_Watcher_Type;
   begin
      W.X := new Unprotected_Watcher_Data;
      pragma Assert (W.X /= null);

      return new Unprotected_Watcher_Type'(W);
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Adv_Mutex_Access is
   begin
      return new Unprotected_Adv_Mutex_Type;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Mutex_Access is
   begin
      return new Unprotected_Mutex_Type;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Barrier_Access is
      B : Unprotected_Barrier_Type;
   begin
      B.X := new Unprotected_Barrier_Data;
      pragma Assert (B.X /= null);

      return new Unprotected_Barrier_Type'(B);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Unprotected_Watcher_Data, Unprotected_Watcher_Data_Access);
   procedure Destroy (W : in out Unprotected_Watcher_Type) is
   begin
      Free (W.X);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Unprotected_Adv_Mutex_Type) is
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Unprotected_Mutex_Type) is
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Unprotected_Barrier_Data, Unprotected_Barrier_Data_Access);
   procedure Destroy (B : in out Unprotected_Barrier_Type) is
   begin
      Free (B.X);
   end Destroy;

   ----------------------------
   -- Enter_Critical_Section --
   ----------------------------

   procedure Enter_Critical_Section is
   begin
      null;
   end Enter_Critical_Section;

   ----------------------------
   -- Leave_Critical_Section --
   ----------------------------

   procedure Leave_Critical_Section is
   begin
      null;
   end Leave_Critical_Section;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (B : in Unprotected_Barrier_Type;
      N : in Positive := 1) is
   begin
      pragma Assert (B.X /= null);
      if not B.X.Perm then
         B.X.Free := B.X.Free + N;
      end if;
   end Signal;

   ----------------
   -- Signal_All --
   ----------------

   procedure Signal_All
     (B : in Unprotected_Barrier_Type;
      P : in Boolean := True) is
   begin
      pragma Assert (B.X /= null);

      if not B.X.Perm then
         if P then
            B.X.Perm := True;
         else
            null;
            --  B.Free := B.Free + B.Wait'Count;
            --  No tasking: B.Wait = 0
         end if;
      end if;
   end Signal_All;

   ----------
   -- Wait --
   ----------

   procedure Wait (B : in Unprotected_Barrier_Type) is
   begin
      pragma Assert (B.X /= null);

      if B.X.Perm or else B.X.Free > 0 then
         if not B.X.Perm then
            B.X.Free := B.X.Free - 1;
         end if;
      else
         raise Program_Error;

         --  Or hang forever...
         loop
            null;
         end loop;
      end if;
   end Wait;

   ------------
   -- Differ --
   ------------

   procedure Differ (W : in Unprotected_Watcher_Type; V : in Version_Id) is
   begin
      --  XXX this is wrong.
      null;
   end Differ;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Unprotected_Mutex_Type) is
   begin
      null;
   end Enter;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Unprotected_Adv_Mutex_Type) is
   begin
      null;
   end Enter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Enter_Critical_Section (Enter_Critical_Section'Access);
      Register_Leave_Critical_Section (Leave_Critical_Section'Access);
      Register_Watcher_Creation_Function (Create'Access);
      Register_Barrier_Creation_Function (Create'Access);
      Register_Mutex_Creation_Function (Create'Access);
      Register_Adv_Mutex_Creation_Function (Create'Access);
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Unprotected_Adv_Mutex_Type) is
   begin
      null;
   end Leave;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Unprotected_Mutex_Type) is
   begin
      null;
   end Leave;

   ------------
   -- Lookup --
   ------------

   procedure Lookup (W : in Unprotected_Watcher_Type; V : out Version_Id) is
   begin
      pragma Assert (W.X /= null);

      V := W.X.Version;
   end Lookup;

   ------------
   -- Update --
   ------------

   procedure Update (W : in Unprotected_Watcher_Type) is
   begin
      pragma Assert (W.X /= null);

      W.X.Version := W.X.Version + 1;
   end Update;

end Droopi.No_Tasking;
