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
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Utils; use System.Garlic.Utils;

package body System.Garlic.Soft_Links is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GASOLI", "(s-gasoli): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Barrier_Create   : Barrier_Creation_Function;
   Mutex_Create     : Mutex_Creation_Function;
   Watcher_Create   : Watcher_Creation_Function;
   Adv_Mutex_Create : Adv_Mutex_Creation_Function;

   procedure Free is
     new Ada.Unchecked_Deallocation (Barrier_Type'Class, Barrier_Access);
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

   ----------
   -- Proc --
   ----------

   package body Proc is

      Var : Parameterless_Procedure;

      ----------
      -- Call --
      ----------

      procedure Call is
      begin
         if Var /= null then
            pragma Debug (D (Name & ": exec call"));
            Var.all;
         else
            pragma Debug (D (Name & ": fake call"));
            null;
         end if;
      end Call;

      --------------
      -- Register --
      --------------

      procedure Register (P : in Parameterless_Procedure) is
      begin
         pragma Debug (D (Name & ": register"));
         Var := P;
      end Register;

   end Proc;

   --------------------------
   -- Termination services --
   --------------------------

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

   package P_Termination_Initialize is new Proc ("Termination_Initialize");
   procedure Register_Termination_Initialize
     (P : in Parameterless_Procedure)
     renames P_Termination_Initialize.Register;
   procedure Termination_Initialize
     renames P_Termination_Initialize.Call;

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

   -------------------------------
   -- Critical section handling --
   -------------------------------

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

   -------------
   -- Barrier --
   -------------

   procedure Register_Barrier_Creation_Function
     (F : in Barrier_Creation_Function)
   is
   begin
      pragma Debug (D ("Barrier'Create : register"));
      Barrier_Create := F;
   end Register_Barrier_Creation_Function;

   procedure Create (B : out Barrier_Access) is
   begin
      B := Barrier_Create.all;
   end Create;

   procedure Destroy (B : in out Barrier_Access) is
   begin
      if B /= null then
         Destroy (B.all);
         Free (B);
      end if;
   end Destroy;

   procedure Signal
     (B : in Barrier_Access;
      N : in Positive := 1) is
   begin
      pragma Assert (B /= null);
      Signal (B.all, N);
   end Signal;

   procedure Signal_All
     (B : in Barrier_Access;
      P : in Boolean := True) is
   begin
      pragma Assert (B /= null);
      Signal_All (B.all, P);
   end Signal_All;

   procedure Wait (B : in Barrier_Access) is
   begin
      pragma Assert (B /= null);
      Wait (B.all);
   end Wait;

   -------------
   -- Mutex --
   -------------

   procedure Register_Mutex_Creation_Function
     (F : in Mutex_Creation_Function)
   is
   begin
      pragma Debug (D ("Mutex'Create : register"));
      Mutex_Create := F;
   end Register_Mutex_Creation_Function;

   procedure Create (M : out Mutex_Access) is
   begin
      M := Mutex_Create.all;
   end Create;

   procedure Enter (M : in Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Enter (M.all);
   end Enter;

   procedure Destroy (M : in out Mutex_Access) is
   begin
      if M /= null then
         Destroy (M.all);
         Free (M);
      end if;
   end Destroy;

   procedure Leave (M : in Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Leave (M.all);
   end Leave;

   -------------
   -- Watcher --
   -------------

   procedure Register_Watcher_Creation_Function
     (F : in Watcher_Creation_Function)
   is
   begin
      pragma Debug (D ("Watcher'Create : register"));
      Watcher_Create := F;
   end Register_Watcher_Creation_Function;

   procedure Create (W : out Watcher_Access) is
   begin
      W := Watcher_Create.all;
   end Create;

   procedure Destroy (W : in out Watcher_Access) is
   begin
      if W /= null then
         Destroy (W.all);
         Free (W);
      end if;
   end Destroy;

   procedure Differ (W : in Watcher_Access; V : in Utils.Version_Id) is
   begin
      pragma Assert (W /= null);
      Differ (W.all, V);
   end Differ;

   procedure Lookup (W : in Watcher_Access; V : out Utils.Version_Id) is
   begin
      pragma Assert (W /= null);
      Lookup (W.all, V);
   end Lookup;

   procedure Update (W : in Watcher_Access) is
   begin
      pragma Assert (W /= null);
      Update (W.all);
   end Update;

   ---------------
   -- Adv_Mutex --
   ---------------

   procedure Register_Adv_Mutex_Creation_Function
     (F : in Adv_Mutex_Creation_Function)
   is
   begin
      pragma Debug (D ("Adv_Mutex'Create : register"));
      Adv_Mutex_Create := F;
   end Register_Adv_Mutex_Creation_Function;

   procedure Create (M :  out Adv_Mutex_Access) is
   begin
      M := Adv_Mutex_Create.all;
   end Create;

   procedure Enter (M : in Adv_Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Enter (M.all);
   end Enter;

   procedure Destroy (M : in out Adv_Mutex_Access) is
   begin
      if M /= null then
         Destroy (M.all);
         Free (M);
      end if;
   end Destroy;

   procedure Leave (M : in Adv_Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Leave (M.all);
   end Leave;

   -------------------------
   -- Shutdown mechanisms --
   -------------------------

   package P_RPC_Shutdown is new Proc ("RPC_Shutdown");
   procedure Register_RPC_Shutdown
     (P : in Parameterless_Procedure)
     renames P_RPC_Shutdown.Register;
   procedure RPC_Shutdown
     renames P_RPC_Shutdown.Call;

end System.Garlic.Soft_Links;
