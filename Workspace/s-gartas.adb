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
with Ada.Dynamic_Priorities;

with System;                     use System;
with System.Soft_Links;          use System.Soft_Links;
with System.Garlic.Soft_Links;   use System.Garlic.Soft_Links;
with System.Garlic.Types;        use System.Garlic.Types;
with System.Tasking;
pragma Elaborate_All (System.Tasking);
with System.Tasking.Debug;
pragma Elaborate_All (System.Tasking.Debug);
with System.Tasking.Utilities;
pragma Elaborate_All (System.Tasking.Utilities);

package body System.Garlic.Tasking is

   use type System.Tasking.Task_ID;

   Environment_Task : constant System.Tasking.Task_ID := System.Tasking.Self;
   --  The environment task. Self will be set to it at elaboration time.

   Critical_Section : Protected_Adv_Mutex_Type;

   type Watcher_PO is limited record
      Mutex : Mutex_PO_Access;
      Queue : Mutex_PO_Access;
      Count : Natural;
      Value : Version_Id;
   end record;

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
      W.X.Value := V;
      W.X.Mutex := new Mutex_PO;

      --  Queue is supposed to block the tasks waiting for a new
      --  version. These tasks wait for mutex Queue. At the beginning
      --  the mutex Queue is locked. When a new version occurs, the
      --  task updating the watcher will unblock each task one by one
      --  with several calls to Leave. This operation is performed
      --  inside a critical section using mutex Mutex. Leave is an
      --  entry to ensure that all the blocked tasks and only those
      --  tasks are unblocked. We also protect these section against
      --  abortion.

      W.X.Count := 0;
      W.X.Queue := new Mutex_PO;
      W.X.Queue.Enter;

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
      if W.X /= null then
         if W.X.Mutex /= null then
            Free (W.X.Mutex);
         end if;
         if W.X.Queue /= null then
            Free (W.X.Queue);
         end if;
         Free (W.X);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Protected_Adv_Mutex_Type) is
   begin
      if M.X /= null then
         if M.X.Mutex /= null then
            Free (M.X.Mutex);
         end if;
         Free (M.X);
      end if;
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

   procedure Differ (W : in Protected_Watcher_Type; V : in Version_Id)
   is
      Updated : Boolean;

   begin
      pragma Assert (W.X /= null);

      --  Queue is supposed to block the tasks waiting for a new
      --  version. These tasks wait for mutex Queue. At the beginning
      --  the mutex Queue is locked. When a new version occurs, the
      --  task updating the watcher will unblock each task one by one
      --  with several calls to Leave. This operation is performed
      --  inside a critical section using mutex Mutex. Leave is an
      --  entry to ensure that all the blocked tasks and only those
      --  tasks are unblocked. We also protect these section against
      --  abortion.

      Abort_Defer.all;
      loop
         W.X.Mutex.Enter;
         Updated := (W.X.Value /= V);
         if not Updated then
            W.X.Count := W.X.Count + 1;
         end if;
         W.X.Mutex.Leave;
         exit when Updated;
         W.X.Queue.Enter;
      end loop;
      Abort_Undefer.all;
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
      Register_List_Tasks (List_Tasks'Access);
      Register_Get_Priority (Get_Priority'Access);
      Register_Set_Priority (Set_Priority'Access);
   end Initialize;

   -------------------------
   -- Is_Environment_Task --
   -------------------------

   function Is_Environment_Task return Boolean
   is
      Self : constant System.Tasking.Task_ID := System.Tasking.Self;
   begin
      return Self = Environment_Task;
   end Is_Environment_Task;

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
      M.X.Leave;
      Abort_Undefer.all;
   end Leave;

   ----------------------------
   -- Leave_Critical_Section --
   ----------------------------

   procedure Leave_Critical_Section is
   begin
      Leave (Critical_Section);
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

   procedure Lookup (W : in Protected_Watcher_Type; V : out Version_Id) is
   begin
      pragma Assert (W.X /= null);
      W.X.Mutex.Enter;
      V := W.X.Value;
      W.X.Mutex.Leave;
   end Lookup;

   --------------
   -- Mutex_PO --
   --------------

   protected body Mutex_PO is

      --------------------
      -- Mutex_PO.Enter --
      --------------------

      entry Enter when not Busy is
      begin
         Busy := True;
      end Enter;

      ----------------------
      -- Mutex_PO.Is_Busy --
      ----------------------

      function Is_Busy return Boolean is
      begin
         return Busy;
      end Is_Busy;

      --------------------
      -- Mutex_PO.Leave --
      --------------------

      entry Leave when Busy is
      begin
         Busy := False;
      end Leave;

   end Mutex_PO;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (P : in Natural) is
   begin
      Ada.Dynamic_Priorities.Set_Priority (Any_Priority (P));
   end Set_Priority;

   ------------
   -- Update --
   ------------

   procedure Update (W : in out Protected_Watcher_Type) is
   begin
      pragma Assert (W.X /= null);
      Abort_Defer.all;
      W.X.Mutex.Enter;
      W.X.Value := W.X.Value + 1;

      --  Queue is supposed to block the tasks waiting for a new
      --  version. These tasks wait for mutex Queue. At the beginning
      --  the mutex Queue is locked. When a new version occurs, the
      --  task updating the watcher will unblock each task one by one
      --  with several calls to Leave. This operation is performed
      --  inside a critical section using mutex Mutex. Leave is an
      --  entry to ensure that all the blocked tasks and only those
      --  tasks are unblocked. We also protect these section against
      --  abortion.

      for I in 1 .. W.X.Count loop
         W.X.Queue.Leave;
      end loop;
      W.X.Count := 0;
      W.X.Mutex.Leave;
      Abort_Undefer.all;
   end Update;

end System.Garlic.Tasking;
