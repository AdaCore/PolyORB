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

with Ada.Task_Attributes;
with Ada.Dynamic_Priorities;
with Ada.Task_Identification;    use Ada.Task_Identification;
pragma Elaborate_All (Ada.Task_Identification);

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
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info_Nolock;

   use type System.Tasking.Task_ID;

   package LLTA is new Ada.Task_Attributes (Natural, 0);
   use LLTA;
   --  Lock Level Task Attribute

   Environment_Task : constant System.Tasking.Task_ID := System.Tasking.Self;
   --  The environment task. Self will be set to it at elaboration time.

   type Watcher_PO is limited record
      Queue : Mutex_PO_Access;
      Count : Natural;
      Value : Version_Id;
   end record;

   type Adv_Mutex_PO is limited null record;
   --  This is a classical critical section except that when a task try to
   --  Enter a critical section several times without leaving it first it
   --  is not blocked and can continue. Leave keeps track of the number of
   --  times Enter has been successful.

   procedure Free is
     new Ada.Unchecked_Deallocation (Adv_Mutex_PO, Adv_Mutex_PO_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Watcher_PO, Watcher_PO_Access);

   protected Global_Lock is
      entry Lock;
      entry Unlock;
      --  This entry could be a procedure, but we want the Task_Id of the
      --  caller for debugging purpose.
   private
      entry Contention;
      Count : Natural := 0;
      Owner : Task_Id;
   end Global_Lock;

   procedure Lock;
   pragma Inline (Lock);
   procedure Unlock;
   pragma Inline (Unlock);

   ------------
   -- Create --
   ------------

   function Create (V : in Version_Id) return Watcher_Access is
      W : Protected_Watcher_Type;
   begin
      W.X := new Watcher_PO;
      W.X.Value := V;
      W.X.Count := 0;
      W.X.Queue := new Mutex_PO;
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
   begin
      return new Protected_Mutex_Type;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (W : in out Protected_Watcher_Type) is
   begin
      if W.X /= null then
         Free (W.X);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Protected_Adv_Mutex_Type) is
   begin
      if M.X /= null then
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
      pragma Assert (Value = 0);

      loop
         Lock;
         Updated := (W.X.Value /= V);
         if not Updated then

            --  Check that the queue is correctly initialized as busy to
            --  block this task.

            if W.X.Count = 0
              and then W.X.Queue.Is_Busy
            then
               W.X.Queue.Enter;
            end if;
            W.X.Count := W.X.Count + 1;
         end if;
         Unlock;
         exit when Updated;
         W.X.Queue.Enter;
      end loop;
   end Differ;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Protected_Mutex_Type) is
   begin
      Lock;
   end Enter;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Protected_Adv_Mutex_Type) is
   begin
      Lock;
   end Enter;

   ----------------------------
   -- Enter_Critical_Section --
   ----------------------------

   procedure Enter_Critical_Section is
   begin
      Lock;
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

   -----------------
   -- Global_Lock --
   -----------------

   protected body Global_Lock is

      ----------------
      -- Contention --
      ----------------

      entry Contention when Count = 0 is
      begin
         Owner := Contention'Caller;
         Count := 1;
      end Contention;

      ----------
      -- Lock --
      ----------

      entry Lock when True is
      begin
         if Count = 0 or else Lock'Caller = Owner then
            Count := Count + 1;
            Owner := Lock'Caller;
         else
            pragma Debug (D ("Contention in lock, count is" & Count'Img));
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
      end Unlock;

   end Global_Lock;

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
      Unlock;
   end Leave;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Protected_Mutex_Type) is
   begin
      Unlock;
   end Leave;

   ----------------------------
   -- Leave_Critical_Section --
   ----------------------------

   procedure Leave_Critical_Section is
   begin
      Unlock;
   end Leave_Critical_Section;

   ----------------
   -- List_Tasks --
   ----------------

   procedure List_Tasks is
   begin
      System.Tasking.Debug.List_Tasks;
   end List_Tasks;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Global_Lock.Lock;
   end Lock;

   ------------
   -- Lookup --
   ------------

   procedure Lookup (W : in Protected_Watcher_Type; V : out Version_Id) is
   begin
      pragma Assert (W.X /= null);
      Lock;
      V := W.X.Value;
      Unlock;
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

      procedure Leave is
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
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Global_Lock.Unlock;
   end Unlock;

   ------------
   -- Update --
   ------------

   procedure Update (W : in out Protected_Watcher_Type) is
   begin
      pragma Assert (W.X /= null);
      Lock;
      W.X.Value := W.X.Value + 1;

      --  Resume at least W.X.Count but maybe less than W.X.Count if
      --  we take into account aborted Differ operations.

      for I in 1 .. W.X.Count loop
         W.X.Queue.Leave;
      end loop;
      W.X.Count := 0;
      Unlock;
   end Update;

end System.Garlic.Tasking;
