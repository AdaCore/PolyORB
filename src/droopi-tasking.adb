--  $Id$

with Ada.Unchecked_Deallocation;

package body Droopi.Tasking is

   use Droopi.Soft_Links;

   ------------------------
   -- Local declarations --
   ------------------------

   protected body Basic_Mutex is
      entry Enter when not Taken is
      begin
         Taken := True;
      end Enter;

      procedure Leave is
      begin
         Taken := False;
      end Leave;
   end Basic_Mutex;

   ------------
   -- Create --
   ------------

   function Create return Watcher_Access is
      W : Protected_Watcher_Type;
   begin
      W.X := new Protected_Watcher_Data;
      pragma Assert (W.X /= null);

      return new Protected_Watcher_Type'(W);
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Adv_Mutex_Access is
   begin
      return new Protected_Adv_Mutex_Type;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Mutex_Access is
      M : Protected_Mutex_Type;
   begin
      M.The_Mutex := new Basic_Mutex;
      return new Protected_Mutex_Type'(M);
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Barrier_Access is
      B : Protected_Barrier_Type;
   begin
      B.X := new Protected_Barrier_Data;
      pragma Assert (B.X /= null);

      return new Protected_Barrier_Type'(B);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Protected_Watcher_Data, Protected_Watcher_Data_Access);
   procedure Destroy (W : in out Protected_Watcher_Type) is
   begin
      Free (W.X);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Protected_Adv_Mutex_Type) is
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_Mutex, Basic_Mutex_Access);
   procedure Destroy (M : in out Protected_Mutex_Type) is
   begin
      Free (M.The_Mutex);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Protected_Barrier_Data, Protected_Barrier_Data_Access);
   procedure Destroy (B : in out Protected_Barrier_Type) is
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
     (B : in Protected_Barrier_Type;
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
     (B : in Protected_Barrier_Type;
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

   procedure Wait (B : in Protected_Barrier_Type) is
   begin
      pragma Assert (B.X /= null);

      if B.X.Perm or else B.X.Free > 0 then
         if not B.X.Perm then
            B.X.Free := B.X.Free - 1;
         end if;
      else
         raise Program_Error;

         --  Or hang forever...
         --  loop
         --     null;
         --  end loop;
      end if;
   end Wait;

   ------------
   -- Differ --
   ------------

   procedure Differ (W : in Protected_Watcher_Type; V : in Version_Id) is
   begin
      if W.X.Version = V then
         --  Dead lock!
         raise Program_Error;
      end if;
   end Differ;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Protected_Mutex_Type) is
   begin
      M.The_Mutex.Enter;
   end Enter;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Protected_Adv_Mutex_Type) is
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
      Register_Task_Identification
        (Task_Id_Function'(Get_Current_Task'Access),
         Task_Id_Function'(Get_Null_Task'Access));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Protected_Adv_Mutex_Type) is
   begin
      null;
   end Leave;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Protected_Mutex_Type) is
   begin
      M.The_Mutex.Leave;
   end Leave;

   ------------
   -- Lookup --
   ------------

   procedure Lookup (W : in Protected_Watcher_Type; V : out Version_Id) is
   begin
      pragma Assert (W.X /= null);

      V := W.X.Version;
   end Lookup;

   ------------
   -- Update --
   ------------

   procedure Update (W : in Protected_Watcher_Type) is
   begin
      pragma Assert (W.X /= null);

      W.X.Version := W.X.Version + 1;
   end Update;

   ------------------
   -- Current_Task --
   ------------------

   function Get_Current_Task return Soft_Links.Task_Id'Class is
   begin
      return No_Task_Id'(Is_Null => False);
   end Get_Current_Task;

   ---------------
   -- Null_Task --
   ---------------

   function Get_Null_Task return Soft_Links.Task_Id'Class is
   begin
      return No_Task_Id'(Is_Null => True);
   end Get_Null_Task;

   -----------
   -- Image --
   -----------

   function Image (T : No_Task_Id) return String is
   begin
      if T.Is_Null then
         return "null_task";
      else
         return "environment_task";
      end if;
   end Image;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (T : No_Task_Id) return Integer is
   begin
      return 0;
   end To_Integer;

end Droopi.Tasking;
