------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . S O F T _ L I N K S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Deallocation;
with System.Garlic.Types; use System.Garlic.Types;

package body System.Garlic.Soft_Links is

   Mutex_Create     : Mutex_Creation_Function;
   Watcher_Create   : Watcher_Creation_Function;
   Adv_Mutex_Create : Adv_Mutex_Creation_Function;

   procedure Free is
      new Ada.Unchecked_Deallocation (Mutex_Type'Class, Mutex_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Watcher_Type'Class, Watcher_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Adv_Mutex_Type'Class, Adv_Mutex_Access);

   generic
      Name : String;
   package Proc is
      procedure Register (P : in Parameterless_Procedure);
      procedure Call;
      pragma Inline (Call);
   end Proc;

   package body Proc is

      Var : Parameterless_Procedure;

      ----------
      -- Call --
      ----------

      procedure Call is
      begin
         if Var /= null then
            Var.all;
         end if;
      end Call;

      --------------
      -- Register --
      --------------

      procedure Register (P : in Parameterless_Procedure) is
      begin
         Var := P;
      end Register;

   end Proc;

   package P_Add_Non_Terminating_Task is new Proc ("Add_Non_Terminating_Task");
   procedure Register_Add_Non_Terminating_Task
     (P : in Parameterless_Procedure)
     renames P_Add_Non_Terminating_Task.Register;
   procedure Add_Non_Terminating_Task
     renames P_Add_Non_Terminating_Task.Call;

   package P_Sub_Non_Terminating_Task is new Proc ("Sub_Non_Terminating_Task");
   procedure Register_Sub_Non_Terminating_Task
     (P : in Parameterless_Procedure)
     renames P_Sub_Non_Terminating_Task.Register;
   procedure Sub_Non_Terminating_Task
     renames P_Sub_Non_Terminating_Task.Call;

   package P_Activity_Detected is new Proc ("Activity_Detected");
   procedure Register_Activity_Detected
     (P : in Parameterless_Procedure)
     renames P_Activity_Detected.Register;
   procedure Activity_Detected
     renames P_Activity_Detected.Call;

   package P_Local_Termination is new Proc ("Local_Termination");
   procedure Register_Local_Termination
     (P : in Parameterless_Procedure)
     renames P_Local_Termination.Register;
   procedure Local_Termination
     renames P_Local_Termination.Call;

   package P_Global_Termination is new Proc ("Global_Termination");
   procedure Register_Global_Termination
     (P : in Parameterless_Procedure)
     renames P_Global_Termination.Register;
   procedure Global_Termination
     renames P_Global_Termination.Call;

   package P_Enter_Critical_Section is new Proc ("Enter_Critical_Section");
   procedure Register_Enter_Critical_Section
     (P : in Parameterless_Procedure)
     renames P_Enter_Critical_Section.Register;
   procedure Enter_Critical_Section
     renames P_Enter_Critical_Section.Call;

   package P_Leave_Critical_Section is new Proc ("Leave_Critical_Section");
   procedure Register_Leave_Critical_Section
     (P : in Parameterless_Procedure)
     renames P_Leave_Critical_Section.Register;
   procedure Leave_Critical_Section
     renames P_Leave_Critical_Section.Call;

   package P_RPC_Shutdown is new Proc ("RPC_Shutdown");
   procedure Register_RPC_Shutdown
     (P : in Parameterless_Procedure)
     renames P_RPC_Shutdown.Register;
   procedure RPC_Shutdown
     renames P_RPC_Shutdown.Call;

   Is_Env_Task : Return_Boolean_Function;

   Awake_Count : Return_Natural_Function;

   Ind_Task_Count : Return_Natural_Function;

   package P_List_Tasks is new Proc ("List_Tasks");
   procedure Register_List_Tasks
     (P : in Parameterless_Procedure)
     renames P_List_Tasks.Register;
   procedure List_Tasks
     renames P_List_Tasks.Call;

   Get_Task_Priority : Return_Natural_Function;

   Set_Task_Priority : Natural_Parameter_Procedure;

   procedure Free is
      new Ada.Unchecked_Deallocation
     (Abort_Handler_Type'Class, Abort_Handler_Access);

   Var_Abort_Handler : Abort_Handler_Access;

   -------------------
   -- Abort_Handler --
   -------------------

   function Abort_Handler return Abort_Handler_Type'Class is
   begin
      return Var_Abort_Handler.all;
   end Abort_Handler;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Abort_Handler_Type) is
   begin
      null;
   end Adjust;

   ------------
   -- Create --
   ------------

   procedure Create (M : out Mutex_Access) is
   begin
      M := Mutex_Create.all;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (W : out Watcher_Access;
      V : in Version_Id := No_Version) is
   begin
      W := Watcher_Create (V);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (M :  out Adv_Mutex_Access) is
   begin
      M := Adv_Mutex_Create.all;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Mutex_Access) is
   begin
      if M /= null then
         Destroy (M.all);
         Free (M);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (W : in out Watcher_Access) is
   begin
      if W /= null then
         Destroy (W.all);
         Free (W);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Adv_Mutex_Access) is
   begin
      if M /= null then
         Destroy (M.all);
         Free (M);
      end if;
   end Destroy;

   ------------
   -- Differ --
   ------------

   procedure Differ (W : in Watcher_Access; V : in Types.Version_Id) is
   begin
      pragma Assert (W /= null);
      Differ (W.all, V);
   end Differ;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Enter (M.all);
   end Enter;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Adv_Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Enter (M.all);
   end Enter;

   --------------------------
   -- Env_Task_Awake_Count --
   --------------------------

   function Env_Task_Awake_Count return Natural is
   begin
      return Awake_Count.all;
   end Env_Task_Awake_Count;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority return Natural is
   begin
      return Get_Task_Priority.all;
   end Get_Priority;

   -----------------------------
   -- Independent_Task_Count --
   -----------------------------

   function Independent_Task_Count return Natural is
   begin
      return Ind_Task_Count.all;
   end Independent_Task_Count;

   -------------------------
   -- Is_Environment_Task --
   -------------------------

   function Is_Environment_Task return Boolean is
   begin
      return Is_Env_Task.all;
   end Is_Environment_Task;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Leave (M.all);
   end Leave;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Adv_Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Leave (M.all);
   end Leave;

   ------------
   -- Lookup --
   ------------

   procedure Lookup (W : in Watcher_Access; V : out Types.Version_Id) is
   begin
      pragma Assert (W /= null);
      Lookup (W.all, V);
   end Lookup;

   ----------------------------
   -- Register_Abort_Handler --
   ----------------------------

   procedure Register_Abort_Handler
     (Abort_Handler : in Abort_Handler_Access) is
   begin
      if Var_Abort_Handler /= null then
         Free (Var_Abort_Handler);
      end if;
      Var_Abort_Handler := Abort_Handler;
   end Register_Abort_Handler;

   ------------------------------------------
   -- Register_Adv_Mutex_Creation_Function --
   ------------------------------------------

   procedure Register_Adv_Mutex_Creation_Function
     (F : in Adv_Mutex_Creation_Function)
   is
   begin
      Adv_Mutex_Create := F;
   end Register_Adv_Mutex_Creation_Function;

   -----------------------------------
   -- Register_Env_Task_Awake_Count --
   -----------------------------------

   procedure Register_Env_Task_Awake_Count
     (F : in Return_Natural_Function) is
   begin
      Awake_Count := F;
   end Register_Env_Task_Awake_Count;

   ---------------------------
   -- Register_Get_Priority --
   ---------------------------

   procedure Register_Get_Priority
     (F : in Return_Natural_Function) is
   begin
      Get_Task_Priority := F;
   end Register_Get_Priority;

   -------------------------------------
   -- Register_Independent_Task_Count --
   -------------------------------------

   procedure Register_Independent_Task_Count
     (F : in Return_Natural_Function) is
   begin
      Ind_Task_Count := F;
   end Register_Independent_Task_Count;

   ----------------------------------
   -- Register_Is_Environment_Task --
   ----------------------------------

   procedure Register_Is_Environment_Task
     (F : in Return_Boolean_Function)
   is
   begin
      Is_Env_Task := F;
   end Register_Is_Environment_Task;

   --------------------------------------
   -- Register_Mutex_Creation_Function --
   --------------------------------------

   procedure Register_Mutex_Creation_Function
     (F : in Mutex_Creation_Function)
   is
   begin
      Mutex_Create := F;
   end Register_Mutex_Creation_Function;

   ---------------------------
   -- Register_Set_Priority --
   ---------------------------

   procedure Register_Set_Priority
     (P : in Natural_Parameter_Procedure) is
   begin
      Set_Task_Priority := P;
   end Register_Set_Priority;

   ----------------------------------------
   -- Register_Watcher_Creation_Function --
   ----------------------------------------

   procedure Register_Watcher_Creation_Function
     (F : in Watcher_Creation_Function)
   is
   begin
      Watcher_Create := F;
   end Register_Watcher_Creation_Function;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (P : in Natural) is
   begin
      Set_Task_Priority (P);
   end Set_Priority;

   ------------
   -- Update --
   ------------

   procedure Update (W : in Watcher_Access) is
   begin
      pragma Assert (W /= null);
      Update (W.all);
   end Update;

end System.Garlic.Soft_Links;
