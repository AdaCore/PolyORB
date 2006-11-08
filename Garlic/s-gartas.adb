------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . T A S K I N G                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

--  with Ada.Calendar;
with Ada.Dynamic_Priorities;
--  with Ada.Task_Attributes;

with System;                     use System;
with System.Garlic.Debug;        use System.Garlic.Debug;
with System.Garlic.Soft_Links;   use System.Garlic.Soft_Links;
with System.Garlic.Types;        use System.Garlic.Types;
with System.Tasking;
pragma Elaborate_All (System.Tasking);
with System.Tasking.Debug;
pragma Elaborate_All (System.Tasking.Debug);
with System.Tasking.Utilities;
pragma Elaborate_All (System.Tasking.Utilities);

package body System.Garlic.Tasking is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARTAS", "(s-gartas): ");
   procedure D
     (Message : String;
      Key     : Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info_Nolock;

   use Ada.Task_Identification;
   use type System.Tasking.Task_Id;

--    package Stamp_Task_Attributes is
--       new Ada.Task_Attributes (Stamp_Type, No_Stamp);
--    use Stamp_Task_Attributes;

   Environment_Task : constant System.Tasking.Task_Id := System.Tasking.Self;
   --  The environment task. Self will be set to it at elaboration time.

   type Protected_Mutex_Access is access all Protected_Mutex_Type;
   type Protected_Watcher_Access is access all Protected_Watcher_Type;

   Critical_Section : Clever_Lock;

   -----------------
   -- Clever_Lock --
   -----------------

   protected body Clever_Lock is

      ----------------
      -- Contention --
      ----------------

      entry Contention when Count = 0 is
      begin
         Owner := Contention'Caller;
         Count := 1;
         pragma Debug (D (Image (Contention'Caller) &
                          " is out from contention"));
      end Contention;

      ----------
      -- Lock --
      ----------

      entry Lock when True is
      begin
         if Count = 0 or else Lock'Caller = Owner then
            Count := Count + 1;
            Owner := Lock'Caller;
            pragma Debug (D ("Locked by " & Image (Owner) &
                             ", count is now " & Count'Img));
         else
            pragma Debug (D ("Contention in lock" &
                             ", locker is " & Image (Owner) &
                             ", waiting is " & Image (Lock'Caller) &
                             ", count is" & Count'Img));
            pragma Assert (not Is_Terminated (Owner));
            requeue Contention with abort;
         end if;
      end Lock;

      ------------
      -- Unlock --
      ------------

      entry Unlock when True is
      begin
         pragma Assert (Unlock'Caller = Owner);
         pragma Assert (Count > 0);
         Count := Count - 1;
         pragma Debug (D ("Unlocked by " & Image (Owner) &
                          ", count is now " & Count'Img));
      end Unlock;

   end Clever_Lock;

   ------------
   -- Create --
   ------------

   function Create (V : Version_Id) return Watcher_Access is
      W : constant Protected_Watcher_Access := new Protected_Watcher_Type;
   begin
      W.P.Init (V);
      return Watcher_Access (W);
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
      M : constant Protected_Mutex_Access := new Protected_Mutex_Type;
   begin
      M.X := new Mutex_PO;
      return Mutex_Access (M);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (W : in out Protected_Watcher_Type)
   is
      pragma Unreferenced (W);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Protected_Adv_Mutex_Type)
   is
      pragma Unreferenced (M);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Protected_Mutex_Type) is
   begin
      if M.X /= null then
         Free (M.X);
      end if;
   end Destroy;

   ------------
   -- Differ --
   ------------

   procedure Differ (W : in out Protected_Watcher_Type; V : Version_Id)
   is
   begin
      W.P.Differ (V);
   end Differ;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : Protected_Mutex_Type) is
   begin
      pragma Assert (M.X /= null);
      M.X.Enter;
   end Enter;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in out Protected_Adv_Mutex_Type) is
   begin
      M.X.Lock;
   end Enter;

   ----------------------------
   -- Enter_Critical_Section --
   ----------------------------

   procedure Enter_Critical_Section is
   begin
      Critical_Section.Lock;
   end Enter_Critical_Section;

   --------------------------
   -- Env_Task_Awake_Count --
   --------------------------

   function Env_Task_Awake_Count return Natural is
   begin
      return Environment_Task.Awake_Count;
   end Env_Task_Awake_Count;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority return Natural is
   begin
      return Natural (Ada.Dynamic_Priorities.Get_Priority);
   end Get_Priority;

--    --------------------
--    -- Get_Task_Stamp --
--    --------------------

--    function Get_Task_Stamp return Stamp_Type is
--    begin
--       return Value;
--    end Get_Task_Stamp;

   ----------------------------
   -- Independent_Task_Count --
   ----------------------------

   function Independent_Task_Count return Natural is
   begin
      return System.Tasking.Utilities.Independent_Task_Count;
   end Independent_Task_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      pragma Debug (D ("Registering soft links"));
      Register_Enter_Critical_Section (Enter_Critical_Section'Access);
      Register_Leave_Critical_Section (Leave_Critical_Section'Access);
      Register_Watcher_Creation_Function (Create'Access);
      Register_Mutex_Creation_Function (Create'Access);
      Register_Adv_Mutex_Creation_Function (Create'Access);
      Register_Is_Environment_Task (Is_Environment_Task'Access);
      Register_Env_Task_Awake_Count (Env_Task_Awake_Count'Access);
      Register_Independent_Task_Count (Independent_Task_Count'Access);
      Register_List_Tasks (List_Tasks'Access);
      Register_Get_Priority (Get_Priority'Access);
      Register_Set_Priority (Set_Priority'Access);
--       Register_Get_Stamp (Get_Task_Stamp'Access);
--       Register_Set_Stamp (Set_Task_Stamp'Access);
   end Initialize;

   -------------------------
   -- Is_Environment_Task --
   -------------------------

   function Is_Environment_Task return Boolean
   is
      Self : constant System.Tasking.Task_Id := System.Tasking.Self;
   begin
      return Self = Environment_Task;
   end Is_Environment_Task;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in out Protected_Adv_Mutex_Type) is
   begin
      M.X.Unlock;
   end Leave;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : Protected_Mutex_Type) is
   begin
      M.X.Leave;
   end Leave;

   ----------------------------
   -- Leave_Critical_Section --
   ----------------------------

   procedure Leave_Critical_Section is
   begin
      Critical_Section.Unlock;
   end Leave_Critical_Section;

   ----------------
   -- List_Tasks --
   ----------------

   procedure List_Tasks is
   begin
      System.Tasking.Debug.List_Tasks;
   end List_Tasks;

   ------------
   -- Lookup --
   ------------

   procedure Lookup (W : Protected_Watcher_Type; V : out Version_Id) is
   begin
      V := W.P.Lookup;
   end Lookup;

   --------------
   -- Mutex_PO --
   --------------

   protected body Mutex_PO is

      -----------
      -- Enter --
      -----------

      entry Enter when not Held is
      begin
         Held := True;
      end Enter;

      -------------
      -- Is_Held --
      -------------

      function Is_Held return Boolean is
      begin
         return Held;
      end Is_Held;

      -----------
      -- Leave --
      -----------

      procedure Leave is
      begin
         pragma Assert (Held);
         Held := False;
      end Leave;

   end Mutex_PO;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (P : Natural) is
   begin
      Ada.Dynamic_Priorities.Set_Priority (Any_Priority (P));
   end Set_Priority;

--    --------------------
--    -- Set_Task_Stamp --
--    --------------------

--    procedure Set_Task_Stamp (S : Float) is
--       X : Stamp_Type := S;
--    begin
--       if S = No_Stamp and then Value = No_Stamp then
--          X := Stamp_Type (Ada.Calendar.Seconds (Ada.Calendar.Clock));
--       end if;
--       Set_Value (X);
--    end Set_Task_Stamp;

   ------------
   -- Update --
   ------------

   procedure Update (W : in out Protected_Watcher_Type) is
   begin
      W.P.Update;
   end Update;

   ----------------
   -- Watcher_PO --
   ----------------

   protected body Watcher_PO is

      ------------
      -- Differ --
      ------------

      entry Differ (From : Version_Id) when not Updated is
      begin
         if From = Value then
            requeue Wait_For_Update with abort;
         end if;
      end Differ;

      ----------
      -- Init --
      ----------

      procedure Init (Initial_Value : Version_Id) is
      begin
         Value := Initial_Value;
      end Init;

      ------------
      -- Lookup --
      ------------

      function Lookup return Version_Id is
      begin
         return Value;
      end Lookup;

      ------------
      -- Update --
      ------------

      procedure Update is
      begin
         Value  := Value + 1;
         if Wait_For_Update'Count > 0 then
            Updated := True;
         end if;
      end Update;

      ---------------------
      -- Wait_For_Update --
      ---------------------

      entry Wait_For_Update (From : Version_Id) when Updated is
      begin
         if Wait_For_Update'Count = 0 then
            Updated := False;
         end if;
         requeue Differ;
      end Wait_For_Update;

   end Watcher_PO;

end System.Garlic.Tasking;
