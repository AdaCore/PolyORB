--  $Id$

with Ada.Task_Identification;    use Ada.Task_Identification;
with Ada.Unchecked_Deallocation;

with System.Soft_Links;  use System.Soft_Links;

with Droopi.Soft_Links;  use Droopi.Soft_Links;

package body Droopi.Protected_Objects is

   Critical_Section : Protected_Adv_Mutex_Type;

   protected type Barrier_PO is
      entry Wait;
      procedure Signal (How_Many : Positive := 1);
      procedure Signal_All (Permanent : Boolean);
   private
      Free : Natural := 0;
      Perm : Boolean := False;
   end Barrier_PO;
   --  Any number of task may be waiting on Wait. Signal unblocks How_Many
   --  tasks (the order depends on the queuing policy) and Signal_All
   --  unblocks all the tasks and Wait will no longer be blocking. If
   --  How_Many is more than the number of tasks waiting, new tasks will be
   --  awakened as well.

   protected type Mutex_PO is
      entry Enter;
      procedure Leave;
   private
      Locked : Boolean := False;
   end Mutex_PO;

   protected type Watcher_PO is
      function Lookup return Version_Id;
      procedure Update;
      entry Differ (V : in Version_Id);
   private
      entry Await (V : in Version_Id);
      Current : Version_Id := Version_Id'First;
      Passing : Boolean := True;
   end Watcher_PO;

   type Adv_Mutex_PO is limited record
      Mutex     : Mutex_PO_Access;
      Current   : Ada.Task_Identification.Task_Id;
      pragma Atomic (Current);
      Level     : Natural;
      pragma Atomic (Level);
   end record;
   --  This is a classical critical section except that when a task try to
   --  Enter a critical section several times without leaving it first it
   --  is not blocked and can continue. Leave keeps track of the number of
   --  times Enter has been successful.

   procedure Free is
     new Ada.Unchecked_Deallocation (Mutex_PO, Mutex_PO_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Adv_Mutex_PO, Adv_Mutex_PO_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Watcher_PO, Watcher_PO_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Barrier_PO, Barrier_PO_Access);

   ------------------
   -- Barrier_PO --
   ------------------

   protected body Barrier_PO is

      -----------------------
      -- Barrier_PO.Signal --
      -----------------------

      procedure Signal (How_Many : Positive := 1) is
      begin
         if not Perm then
            Free := Free + How_Many;
         end if;
      end Signal;

      ---------------------------
      -- Barrier_PO.Signal_All --
      ---------------------------

      procedure Signal_All (Permanent : Boolean) is
      begin
         if not Perm then
            if Permanent then
               Perm := True;
            else
               Free := Free + Wait'Count;
            end if;
         end if;
      end Signal_All;

      ---------------------
      -- Barrier_PO.Wait --
      ---------------------

      entry Wait when Perm or else Free > 0 is
      begin
         if not Perm then
            Free := Free - 1;
         end if;
      end Wait;

   end Barrier_PO;

   ------------
   -- Create --
   ------------

   function Create return Barrier_Access is
      B : Protected_Barrier_Type;
   begin
      B.X := new Barrier_PO;
      return new Protected_Barrier_Type'(B);
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Watcher_Access is
      W : Protected_Watcher_Type;
   begin
      W.X := new Watcher_PO;
      return new Protected_Watcher_Type'(W);
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Adv_Mutex_Access is
      M : Protected_Adv_Mutex_Type;
   begin
      M.X         := new Adv_Mutex_PO;
      M.X.Mutex   := new Mutex_PO;
      M.X.Current := Null_Task_Id;
      M.X.Level   := 0;
      return new Protected_Adv_Mutex_Type'(M);
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Mutex_Access is
      M : Protected_Mutex_Type;
   begin
      M.X := new Mutex_PO;
      return new Protected_Mutex_Type'(M);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (B : in out Protected_Barrier_Type) is
   begin
      Free (B.X);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (W : in out Protected_Watcher_Type) is
   begin
      Free (W.X);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Protected_Adv_Mutex_Type) is
   begin
      Free (M.X.Mutex);
      Free (M.X);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Protected_Mutex_Type) is
   begin
      Free (M.X);
   end Destroy;

   ------------
   -- Differ --
   ------------

   procedure Differ (W : in Protected_Watcher_Type; V : in Version_Id) is
   begin
      pragma Assert (W.X /= null);
      W.X.Differ (V);
   end Differ;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Protected_Mutex_Type) is
   begin
      pragma Assert (M.X /= null);
      Abort_Defer.all;
      M.X.Enter;
   end Enter;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Protected_Adv_Mutex_Type) is
      Self : constant Task_Id := Current_Task;
   begin
      pragma Assert (M.X /= null);
      if M.X.Current /= Self then
         pragma Assert (M.X.Mutex /= null);
         M.X.Mutex.Enter;
         pragma Assert (M.X.Current = Null_Task_Id);
         pragma Assert (M.X.Level   = 0);
         M.X.Current := Self;
      end if;
      M.X.Level := M.X.Level + 1;
   end Enter;

   ----------------------------
   -- Enter_Critical_Section --
   ----------------------------

   procedure Enter_Critical_Section is
   begin
      Enter (Critical_Section);
   end Enter_Critical_Section;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Critical_Section.X         := new Adv_Mutex_PO;
      Critical_Section.X.Mutex   := new Mutex_PO;
      Critical_Section.X.Current := Null_Task_Id;
      Critical_Section.X.Level   := 0;
      Register_Enter_Critical_Section (Enter_Critical_Section'Access);
      Register_Leave_Critical_Section (Leave_Critical_Section'Access);
      Register_Barrier_Creation_Function (Create'Access);
      Register_Watcher_Creation_Function (Create'Access);
      Register_Mutex_Creation_Function (Create'Access);
      Register_Adv_Mutex_Creation_Function (Create'Access);
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Protected_Adv_Mutex_Type) is
   begin
      pragma Assert (M.X /= null
                     and then M.X.Mutex /= null
                     and then M.X.Current = Current_Task
                     and then M.X.Level > 0);
      M.X.Level := M.X.Level - 1;
      if M.X.Level = 0 then
         M.X.Current := Null_Task_Id;
         M.X.Mutex.Leave;
      end if;
   end Leave;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Protected_Mutex_Type) is
   begin
      pragma Assert (M.X /= null);
      Abort_Undefer.all;
      M.X.Leave;
   end Leave;

   ----------------------------
   -- Leave_Critical_Section --
   ----------------------------

   procedure Leave_Critical_Section is
   begin
      Leave (Critical_Section);
   end Leave_Critical_Section;

   ------------
   -- Lookup --
   ------------

   procedure Lookup (W : in Protected_Watcher_Type; V : out Version_Id) is
   begin
      pragma Assert (W.X /= null);
      V := W.X.Lookup;
   end Lookup;

   --------------
   -- Mutex_PO --
   --------------

   protected body Mutex_PO is

      --------------------
      -- Mutex_PO.Enter --
      --------------------

      entry Enter when not Locked is
      begin
         Locked := True;
      end Enter;

      --------------------
      -- Mutex_PO.Leave --
      --------------------

      procedure Leave is
      begin
         Locked := False;
      end Leave;

   end Mutex_PO;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (B : in Protected_Barrier_Type;
      N : in Positive := 1) is
   begin
      pragma Assert (B.X /= null);
      B.X.Signal (N);
   end Signal;

   ----------------
   -- Signal_All --
   ----------------

   procedure Signal_All
     (B : in Protected_Barrier_Type;
      P : in Boolean := True) is
   begin
      pragma Assert (B.X /= null);
      B.X.Signal_All (P);
   end Signal_All;

   ------------
   -- Update --
   ------------

   procedure Update (W : in Protected_Watcher_Type) is
   begin
      pragma Assert (W.X /= null);
      W.X.Update;
   end Update;

   ----------
   -- Wait --
   ----------

   procedure Wait (B : in Protected_Barrier_Type) is
   begin
      pragma Assert (B.X /= null);
      B.X.Wait;
   end Wait;

   ----------------
   -- Watcher_PO --
   ----------------

   protected body Watcher_PO is

      ----------------------
      -- Watcher_PO.Await --
      ----------------------

      entry Await (V : in Version_Id) when not Passing is
      begin
         if Await'Count = 0 then
            Passing := True;
         end if;
         requeue Differ with abort;
      end Await;

      -----------------------
      -- Watcher_PO.Differ --
      -----------------------

      entry Differ (V : in Version_Id) when Passing is
      begin
         if Current = V then
            requeue Await with abort;
         end if;
      end Differ;

      -----------------------
      -- Watcher_PO.Lookup --
      -----------------------

      function Lookup return Version_Id is
      begin
         return Current;
      end Lookup;

      -----------------------
      -- Watcher_PO.Update --
      -----------------------

      procedure Update is
      begin
         Current := Current + 1;
         if Current = No_Version then
            Current := Current + 1;
         end if;
         if Await'Count /= 0 then
            Passing := False;
         end if;
      end Update;

   end Watcher_PO;

end Droopi.Protected_Objects;
