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

with Ada.Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Soft_Links is

   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Watchers;
   use PolyORB.Tasking.Advanced_Mutexes;

   procedure Free is
     new Ada.Unchecked_Deallocation (Mutex_Type'Class, Mutex_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Watcher_Type'Class, Watcher_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Adv_Mutex_Type'Class, Adv_Mutex_Access);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Version_Id) return Boolean is
      Version_Id_Window : constant Version_Id := Version_Id'Last / 2;
   begin
      return Integer (R - L) < Integer (Version_Id_Window);
   end "<";

   ---------------
   -- Adv_Mutex --
   ---------------

   procedure Create (M :  out Adv_Mutex_Access) is
   begin
      M := Create;
   end Create;

   function Create return Adv_Mutex_Access is
      M : Adv_Mutex_Type;
   begin
      Create (M.X);
      return new Adv_Mutex_Type'(M);
   end Create;

   procedure Destroy (M : in out Adv_Mutex_Type) is
   begin
      Destroy (M.X);
   end Destroy;

   procedure Destroy (M : in out Adv_Mutex_Access) is
   begin
      if M /= null then
         Destroy (M.all);
         Free (M);
      end if;
   end Destroy;

   procedure Enter (M : in Adv_Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Enter (M.all);
   end Enter;

   procedure Enter (M : in Adv_Mutex_Type) is
   begin
      Enter (M.X);
   end Enter;

   procedure Leave (M : in Adv_Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Leave (M.all);
   end Leave;

   procedure Leave (M : in Adv_Mutex_Type) is
   begin
      Leave (M.X);
   end Leave;

   -----------
   -- Mutex --
   -----------

   procedure Create (M : out Mutex_Access) is
   begin
      M := Create;
   end Create;

   function Create return Mutex_Access is
      M : Mutex_Type;
   begin
      Create (M.M);
      return new Mutex_Type'(M);
   end Create;

   procedure Destroy (M : in out Mutex_Access) is
   begin
      if M /= null then
         Destroy (M.all);
         Free (M);
      end if;
   end Destroy;

   procedure Destroy (M : in out Mutex_Type) is
   begin
      Destroy (M.M);
   end Destroy;

   procedure Enter (M : in Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Enter (M.all);
   end Enter;

   procedure Enter (M : in Mutex_Type) is
   begin
      Enter (M.M);
   end Enter;

   procedure Leave (M : in Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Leave (M.all);
   end Leave;

   procedure Leave (M : in Mutex_Type) is
   begin
      Leave (M.M);
   end Leave;

   -------------
   -- Watcher --
   -------------

   procedure Create (W : out Watcher_Access) is
   begin
      W := Create;
   end Create;

   function Create return Watcher_Access is
      W : Watcher_Type;
   begin
      W.W := new PolyORB.Tasking.Watchers.Watcher_Type;
      Create (W.W.all);
      return new Watcher_Type'(W);
   end Create;

   procedure Differ
     (W : in Watcher_Type;
      V : in Version_Id) is
   begin
      Differ (W.W.all, Tasking.Watchers.Version_Id (V));
   end Differ;

   procedure Destroy (W : in out Watcher_Access) is
   begin
      if W /= null then
         Destroy (W.all);
         Free (W);
      end if;
   end Destroy;

   procedure Destroy (W : in out Watcher_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (PolyORB.Tasking.Watchers.Watcher_Type,
         PolyORB.Tasking.Watchers.Watcher_Access);
   begin
      Destroy (W.W.all);
      Free (W.W);
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

   procedure Lookup
     (W : in Watcher_Type;
      V : out Version_Id) is
   begin
      Lookup (W.W.all, Tasking.Watchers.Version_Id (V));
   end Lookup;

   procedure Update (W : in Watcher_Access) is
   begin
      pragma Assert (W /= null);
      Update (W.all);
   end Update;

   procedure Update (W : in Watcher_Type) is
   begin
      Update (W.W.all);
   end Update;

   -------------------------------------------
   -- Critical Section for ORB with Tasking --
   -------------------------------------------

   Critical_Section : Adv_Mutex_Access;

   procedure Enter_Critical_Section is
   begin
      Enter (Critical_Section);
   end Enter_Critical_Section;

   procedure Leave_Critical_Section is
   begin
      Leave (Critical_Section);
   end Leave_Critical_Section;

   ---------------------
   -- Task management --
   ---------------------

   My_Thread_Factory  : Thread_Factory_Access;

   procedure Create_Task
     (Main : Parameterless_Procedure) is
      T  : Thread_Access;
   begin
      T := Run_In_Task
        (TF => My_Thread_Factory,
         P  => Tasking.Threads.Parameterless_Procedure (Main));
   end Create_Task;

   function Current_Task return Task_Id'Class is
      X : Task_Id;
   begin
      X.X := new Thread_Id'Class'(Get_Current_Thread_Id (My_Thread_Factory));
      return X;
   end Current_Task;

   function Null_Task return Task_Id'Class is
      X : Task_Id;
   begin
      --  cannot be implemented with PolyORB.Tasking.
      raise Not_Implemented;
      return X;
   end Null_Task;

   function Image (T : Task_Id) return String is
      pragma Warnings (Off);
      pragma Warnings (On);
   begin
      return Threads.Image (T.X.all);
   end Image;

   function To_Integer (T : Task_Id) return Integer is
      pragma Warnings (Off);
      pragma Unreferenced (T);
      pragma Warnings (On);
   begin
      --  Not implementable with PolyORB.Tasking.
      raise Not_Implemented;
      return -1;
   end To_Integer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      My_Thread_Factory  := Get_Thread_Factory;
      Create (Critical_Section);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
      Register_Module
     (Module_Info'
      (Name => +"tasking.soft_links",
       Conflicts => Empty,
       Depends => +"tasking.threads"
         & "tasking.condition_variables"
         & "tasking.mutexes",
       Provides => +"soft_links",
       Init => Initialize'Access));
end PolyORB.Tasking.Soft_Links;
