------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . S O F T _ L I N K S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Unchecked_Deallocation;

package body PolyORB.Soft_Links is

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

   Version_Id_Window : constant Version_Id := Version_Id'Last / 2;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Version_Id) return Boolean is
   begin
      return Integer (R - L) < Integer (Version_Id_Window);
   end "<";

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

   procedure Differ (W : in Watcher_Access; V : in Version_Id) is
   begin
      pragma Assert (W /= null);
      Differ (W.all, V);
   end Differ;

   procedure Lookup (W : in Watcher_Access; V : out Version_Id) is
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

   ---------------------
   -- Task allocation --
   ---------------------

   Do_Create_Task : Task_Allocation_Procedure;

   procedure Register_Create_Task
     (P : Task_Allocation_Procedure) is
   begin
      Do_Create_Task := P;
   end Register_Create_Task;

   procedure Create_Task (Main : Parameterless_Procedure) is
   begin
      Do_Create_Task.all (Main);
   end Create_Task;

   -------------------------
   -- Task identification --
   -------------------------

   Get_Current_Task : Task_Id_Function;
   Get_Null_Task    : Task_Id_Function;

   procedure Register_Task_Identification
     (CT : in Task_Id_Function;
      NT : in Task_Id_Function) is
   begin
      Get_Current_Task := CT;
      Get_Null_Task    := NT;
   end Register_Task_Identification;

   function Current_Task return Task_Id'Class is
   begin
      return Get_Current_Task.all;
   end Current_Task;

   function Null_Task return Task_Id'Class is
   begin
      return Get_Null_Task.all;
   end Null_Task;

end PolyORB.Soft_Links;
