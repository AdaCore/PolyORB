------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . N O _ T A S K I N G             --
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

with System.Garlic.Protocols;  use System.Garlic.Protocols;
with System.Garlic.Soft_Links; use System.Garlic.Soft_Links;
with System.Garlic.Types;      use System.Garlic.Types;

package body System.Garlic.No_Tasking is

   ------------
   -- Create --
   ------------

   function Create (V : in Version_Id) return Watcher_Access is
   begin
      return new Unprotected_Watcher_Type'(Version => V);
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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (W : in out Unprotected_Watcher_Type) is
   begin
      null;
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

   ------------
   -- Differ --
   ------------

   procedure Differ (W : in Unprotected_Watcher_Type; V : in Version_Id) is
   begin
      Receive_From_All_Protocols;
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

   ----------------------------
   -- Enter_Critical_Section --
   ----------------------------

   procedure Enter_Critical_Section is
   begin
      null;
   end Enter_Critical_Section;

   --------------------------
   -- Env_Task_Awake_Count --
   --------------------------

   function Env_Task_Awake_Count return Natural is
   begin
      return 1;
   end Env_Task_Awake_Count;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority return Natural is
   begin
      return Natural (System.Default_Priority);
   end Get_Priority;

   ----------------------------
   -- Independent_Task_Count --
   ----------------------------

   function Independent_Task_Count return Natural is
   begin
      return 0;
   end Independent_Task_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
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

   -------------------------
   -- Is_Environment_Task --
   -------------------------

   function Is_Environment_Task return Boolean is
   begin
      return True;
   end Is_Environment_Task;

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

   ----------------------------
   -- Leave_Critical_Section --
   ----------------------------

   procedure Leave_Critical_Section is
   begin
      null;
   end Leave_Critical_Section;

   ------------
   -- Lookup --
   ------------

   procedure Lookup (W : in Unprotected_Watcher_Type; V : out Version_Id) is
   begin
      V := W.Version;
   end Lookup;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (P : in Natural) is
   begin
      null;
   end Set_Priority;

   ------------
   -- Update --
   ------------

   procedure Update (W : in out Unprotected_Watcher_Type) is
   begin
      W.Version := W.Version + 1;
   end Update;

end System.Garlic.No_Tasking;
