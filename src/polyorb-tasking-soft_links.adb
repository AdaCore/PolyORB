------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B - T A S K I N G - S O F T _ L I N K S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;


package body PolyORB.Tasking.Soft_Links is

   Critical_Section : Tasking_Adv_Mutex_Type;

   My_Monitor_Factory : Monitor_Factory_Access;

   My_Thread_Factory  : Thread_Factory_Access;

   type Generic_Run is new Runnable with record
      P  : PS.Parameterless_Procedure;
   end record;
   --  Simple generic Runnable, that use a access to procedure
   --  for its main procedure

   procedure Run (R : access Generic_Run);

   ------------
   -- Create --
   ------------

   function Create return PS.Mutex_Access is
      M : Tasking_Mutex_Type;
   begin
      M.M := Create (My_Monitor_Factory);
      return new Tasking_Mutex_Type'(M);
   end Create;

   function Create return PS.Watcher_Access is
      W : Tasking_Watcher_Type;
   begin
      W.W := new Watcher_Type;
      Create (W.W.all);
      return new Tasking_Watcher_Type'(W);
   end Create;

   function Create return PS.Adv_Mutex_Access is
      M : Tasking_Adv_Mutex_Type;
   begin
      M.X := new Adv_Mutex_Type;
      Create (M.X.all);
      return new Tasking_Adv_Mutex_Type'(M);
   end Create;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (Main : PS.Parameterless_Procedure) is
      T  : Thread_Access;
   begin
      T := Run_In_Task
        (TF => My_Thread_Factory,
         P  => Tasking.Threads.Parameterless_Procedure (Main));
   end Create_Task;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Tasking_Mutex_Type) is
   begin
      Destroy (My_Monitor_Factory.all, M.M);
   end Destroy;

   procedure Destroy (W : in out Tasking_Watcher_Type) is
   begin
      Destroy (W.W.all);
   end Destroy;

   procedure Destroy (M : in out Tasking_Adv_Mutex_Type) is
   begin
      Destroy (M.X.all);
   end Destroy;

   ------------
   -- Differ --
   ------------

   procedure Differ
     (W : in Tasking_Watcher_Type;
      V : in PS.Version_Id) is
   begin
      Differ (W.W.all, Tasking.Watchers.Version_Id (V));
   end Differ;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Tasking_Mutex_Type) is
   begin
      Enter (M.M.all);
   end Enter;

   procedure Enter (M : in Tasking_Adv_Mutex_Type) is
   begin
      Enter (M.X.all);
   end Enter;

   ----------------------------
   -- Enter_Critical_Section --
   ----------------------------

   procedure Enter_Critical_Section is
   begin
      Enter (Critical_Section);
   end Enter_Critical_Section;

   ----------------------
   -- Get_Current_Task --
   ----------------------

   function Get_Current_Task return PS.Task_Id'Class is
      X : Tasking_Task_Id;
   begin
      X.X := new Thread_Id'Class'(Get_Current_Thread_Id (My_Thread_Factory));
      return X;
   end Get_Current_Task;

   -------------------
   -- Get_Null_Task --
   -------------------

   function Get_Null_Task return PS.Task_Id'Class is
      X : Tasking_Task_Id;
   begin
      --  cannot be implemented with PolyORB.Tasking.
      raise Not_Implemented;
      return X;
   end Get_Null_Task;

   ----------------------------
   -- Leave_Critical_Section --
   ----------------------------

   procedure Leave_Critical_Section is
   begin
      Leave (Critical_Section);
   end Leave_Critical_Section;

   -----------
   -- Image --
   -----------

   function Image (T : Tasking_Task_Id) return String is
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      --  Not implementable with PolyORB.Tasking.
      return "";
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use PolyORB.Soft_Links;
   begin
      My_Monitor_Factory := Get_Monitor_Factory;
      My_Thread_Factory := Get_Thread_Factory;
      Critical_Section.X         := new Mutexes.Adv_Mutex_Type;
      Create (Critical_Section.X.all);
      Register_Enter_Critical_Section (Enter_Critical_Section'Access);
      Register_Leave_Critical_Section (Leave_Critical_Section'Access);
      Register_Watcher_Creation_Function (Create'Access);
      Register_Mutex_Creation_Function (Create'Access);
      Register_Adv_Mutex_Creation_Function (Create'Access);
      Register_Create_Task (Create_Task'Access);
      Register_Task_Identification
        (Task_Id_Function'(Get_Current_Task'Access),
         Task_Id_Function'(Get_Null_Task'Access));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Tasking_Mutex_Type) is
   begin
      Leave (M.M.all);
   end Leave;

   procedure Leave (M : in Tasking_Adv_Mutex_Type) is
   begin
      Leave (M.X.all);
   end Leave;

   ------------
   -- Lookup --
   ------------

   procedure Lookup
     (W : in Tasking_Watcher_Type;
      V : out PS.Version_Id) is
   begin
      Lookup (W.W.all, Tasking.Watchers.Version_Id (V));
   end Lookup;

   ---------
   -- Run --
   ---------

   procedure Run (R : access Generic_Run) is
   begin
      R.P.all;
   end Run;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (T : Tasking_Task_Id) return Integer is
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      --  Not implementable with PolyORB.Tasking.
      raise Not_Implemented;
      return -1;
   end To_Integer;

   ------------
   -- Update --
   ------------

   procedure Update (W : in Tasking_Watcher_Type) is
   begin
      Update (W.W.all);
   end Update;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
      Register_Module
     (Module_Info'
      (Name => +"tasking-soft_links",
       Conflicts => Empty,
       Depends => (+"tasking-monitors") & "tasking-threads",
       Provides => +"soft_links",
       Init => Initialize'Access));
end PolyORB.Tasking.Soft_Links;
