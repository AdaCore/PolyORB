------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . T A S K I N G                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with Ada.Task_Identification;    use Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with Ada.Dynamic_Priorities;

with System;                     use System;
with System.Soft_Links;          use System.Soft_Links;
with System.Garlic.Soft_Links;   use System.Garlic.Soft_Links;
with System.Garlic.Types;        use System.Garlic.Types;
with System.Tasking;
pragma Elaborate_All (System.Tasking);
with System.Tasking.Utilities;
pragma Elaborate_All (System.Tasking.Utilities);

package body System.Garlic.Tasking is

   use type System.Tasking.Task_ID;

   Environment_Task : constant System.Tasking.Task_ID := System.Tasking.Self;
   --  The environment task. Self will be set to it at elaboration time.

   Critical_Section : Protected_Adv_Mutex_Type;

   protected type Mutex_PO is
      entry Enter;
      procedure Leave;
   private
      Locked : Boolean := False;
   end Mutex_PO;

   protected type Watcher_PO is
      function Lookup return Version_Id;
      procedure Update;
      procedure Initialize (V : in Version_Id);
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

   ------------
   -- Create --
   ------------

   function Create (V : in Version_Id) return Watcher_Access is
      W : Protected_Watcher_Type;
   begin
      W.X := new Watcher_PO;
      W.X.Initialize (V);
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
      Register_Watcher_Creation_Function (Create'Access);
      Register_Mutex_Creation_Function (Create'Access);
      Register_Adv_Mutex_Creation_Function (Create'Access);
      Register_Is_Environment_Task (Is_Environment_Task'Access);
      Register_Env_Task_Awake_Count (Env_Task_Awake_Count'Access);
      Register_Independent_Task_Count (Independent_Task_Count'Access);
      Register_Get_Priority (Get_Priority'Access);
      Register_Set_Priority (Set_Priority'Access);
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
   -- Update --
   ------------

   procedure Update (W : in out Protected_Watcher_Type) is
   begin
      pragma Assert (W.X /= null);
      W.X.Update;
   end Update;

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

      ---------------------------
      -- Watcher_PO.Initialize --
      ---------------------------

      procedure Initialize
        (V : in Version_Id) is
      begin
         Current := V;
      end Initialize;

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

   function Is_Environment_Task return Boolean
   is
      Self : constant System.Tasking.Task_ID := System.Tasking.Self;
   begin
      return Self = Environment_Task;
   end Is_Environment_Task;

   function Env_Task_Awake_Count return Natural is
   begin
      return Environment_Task.Awake_Count;
   end Env_Task_Awake_Count;

   function Independent_Task_Count return Natural is
   begin
      return System.Tasking.Utilities.Independent_Task_Count;
   end Independent_Task_Count;

   function Get_Priority return Natural is
   begin
      return Natural (Ada.Dynamic_Priorities.Get_Priority);
   end Get_Priority;

   procedure Set_Priority (P : in Natural) is
   begin
      Ada.Dynamic_Priorities.Set_Priority (Any_Priority (P));
   end Set_Priority;

end System.Garlic.Tasking;
