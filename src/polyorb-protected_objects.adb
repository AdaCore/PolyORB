------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . P R O T E C T E D _ O B J E C T S             --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

pragma Warnings (Off);
with System.Soft_Links;
pragma Warnings (On);
--  Internal GNAT unit.

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Soft_Links;  use PolyORB.Soft_Links;
with PolyORB.Utils.Strings;

package body PolyORB.Protected_Objects is

   use Ada.Task_Identification;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protected_objects");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Critical_Section : Protected_Adv_Mutex_Type;

   --  Any number of task may be waiting on Wait. Signal unblocks How_Many
   --  tasks (the order depends on the queuing policy) and Signal_All
   --  unblocks all the tasks and Wait will no longer be blocking. If
   --  How_Many is more than the number of tasks waiting, new tasks will be
   --  awakened as well.

   protected type Mutex_PO is
      entry Enter;
      procedure Leave;
   private
      Locked : Boolean := False;
   end Mutex_PO;

   protected type Watcher_PO is
      function Lookup return Version_Id;
      procedure Update;
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
   pragma Warnings (On);

   procedure Free is
     new Ada.Unchecked_Deallocation (Mutex_PO, Mutex_PO_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Adv_Mutex_PO, Adv_Mutex_PO_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Watcher_PO, Watcher_PO_Access);

   ------------
   -- Create --
   ------------

   function Create return Watcher_Access is
      W : Protected_Watcher_Type;
   begin
      W.X := new Watcher_PO;
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
      pragma Debug (O ("orb destroying lock !!"));

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
      System.Soft_Links.Abort_Defer.all;
      M.X.Enter;
   end Enter;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in Protected_Adv_Mutex_Type) is
      Self : constant Ada.Task_Identification.Task_Id
        := Current_Task;
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
      Register_Create_Task (Create_Task'Access);
      Register_Task_Identification
        (Task_Id_Function'(Get_Current_Task'Access),
         Task_Id_Function'(Get_Null_Task'Access));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Protected_Adv_Mutex_Type) is
   begin
      pragma Assert (M.X /= null);
      pragma Assert (M.X.Mutex /= null);
      pragma Assert (M.X.Current = Current_Task);
      pragma Assert (M.X.Level > 0);

      pragma Debug (O ("Prot_Adv_Mutex: leaving"));
      M.X.Level := M.X.Level - 1;

      pragma Debug (O ("Prot_Adv_Mutex: checking level"));
      if M.X.Level = 0 then
         pragma Debug (O ("Prot_Adv_Mutex: level = 0"));
         M.X.Current := Null_Task_Id;
         pragma Debug (O ("Prot_Adv_Mutex: setting current to null"));
         M.X.Mutex.Leave;
         pragma Debug (O ("Prot_Adv_Mutex: mutex left"));
      end if;
      pragma Debug (O ("Prot_Adv_Mutex: leave left"));
   end Leave;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Protected_Mutex_Type) is
   begin
      pragma Assert (M.X /= null);
      System.Soft_Links.Abort_Undefer.all;
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

   procedure Update (W : in Protected_Watcher_Type) is
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
         pragma Warnings (Off);
         pragma Unreferenced (V);
         pragma Warnings (On);
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

   task type Generic_Task is
      entry Start (Main : Parameterless_Procedure);
      pragma Storage_Size (131072);
   end Generic_Task;

   task body Generic_Task is
      My_Main : Parameterless_Procedure;
   begin
      accept Start (Main : Parameterless_Procedure) do
         My_Main := Main;
      end Start;
      My_Main.all;
   end Generic_Task;

   type Generic_Task_Access is access Generic_Task;

   procedure Create_Task (Main : Parameterless_Procedure) is
      T : constant Generic_Task_Access := new Generic_Task;
   begin
      T.Start (Main);
   end Create_Task;

   ------------------
   -- Current_Task --
   ------------------

   function Get_Current_Task return Soft_Links.Task_Id'Class is
   begin
      return PO_Task_Id'(X => Ada.Task_Identification.Current_Task);
   end Get_Current_Task;

   ---------------
   -- Null_Task --
   ---------------

   function Get_Null_Task return Soft_Links.Task_Id'Class is
   begin
      return PO_Task_Id'(X => Ada.Task_Identification.Null_Task_Id);
   end Get_Null_Task;

   -----------
   -- Image --
   -----------

   function Image (T : PO_Task_Id) return String is
   begin
      return Ada.Task_Identification.Image (T.X);
   end Image;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (T : PO_Task_Id) return Integer
   is
      function Task_Id_To_Integer is new Ada.Unchecked_Conversion
        (Ada.Task_Identification.Task_Id, Integer);
   begin
      return Task_Id_To_Integer (T.X);
   end To_Integer;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"protected_objects",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"soft_links",
       Init => Initialize'Access));
end PolyORB.Protected_Objects;
