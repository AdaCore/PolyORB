------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U T I L S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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
with System.Tasking_Soft_Links; use System.Tasking_Soft_Links;

package body System.Garlic.Utils is

   use Ada.Task_Identification;

   protected type Mutex_Type is
      entry Enter;
      procedure Leave;
   private
      Locked : Boolean := False;
   end Mutex_Type;

   protected type Watcher_Type is
      function Commit return Version_Id;
      procedure Update;
      entry Differ (V : in Version_Id);
   private
      entry Await (V : in Version_Id);
      Current : Version_Id := Version_Id'First;
      Passing : Boolean := True;
   end Watcher_Type;

   type Adv_Mutex_Type is limited record
      Mutex     : Mutex_Access;
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
     new Ada.Unchecked_Deallocation (Mutex_Type, Mutex_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Adv_Mutex_Type, Adv_Mutex_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Watcher_Type, Watcher_Access);

   ----------------------
   -- Access_To_String --
   ----------------------

   function Access_To_String (S : String_Access) return String is
   begin
      return S.all;
   end Access_To_String;

   ------------------
   -- Barrier_Type --
   ------------------

   protected body Barrier_Type is

      ------------
      -- Signal --
      ------------

      procedure Signal (How_Many : Positive := 1) is
      begin
         if not Perm then
            Free := Free + How_Many;
         end if;
      end Signal;

      ----------------
      -- Signal_All --
      ----------------

      procedure Signal_All (Permanent : Boolean) is
      begin
         if not Perm then
            if Permanent then
               Perm := True;
            else
               Free := Free + Wait'Count;
            end if;
         end if;
      end Signal_All;

      ----------
      -- Wait --
      ----------

      entry Wait when Perm or else Free > 0 is
      begin
         if not Perm then
            Free := Free - 1;
         end if;
      end Wait;

   end Barrier_Type;

   ------------
   -- Create --
   ------------

   function Create return Watcher_Access is
   begin
      return new Watcher_Type;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Adv_Mutex_Access is
      Item : Adv_Mutex_Access := new Adv_Mutex_Type;

   begin
      Item.Mutex   := new Mutex_Type;
      Item.Current := Null_Task_Id;
      return Item;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Mutex_Access is
   begin
      return new Mutex_Type;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (W : in out Watcher_Access) is
   begin
      Free (W);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Adv_Mutex_Access) is
   begin
      Free (M.Mutex);
      Free (M);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Mutex_Access) is
   begin
      Free (M);
   end Destroy;

   ------------
   -- Differ --
   ------------

   procedure Differ (W : in Watcher_Access; V : in Version_Id) is
   begin
      W.Differ (V);
   end Differ;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Abort_Defer.all;
      M.Enter;
   end Enter;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : Adv_Mutex_Access) is
      Self : constant Task_Id := Current_Task;
   begin
      pragma Assert (M /= null);
      if M.Current /= Self then
         M.Mutex.Enter;
         M.Current := Self;
      end if;
      M.Level := M.Level + 1;
   end Enter;

   -----------
   -- Commit --
   -----------

   procedure Commit (W : in Watcher_Access; V : out Version_Id) is
   begin
      V := W.Commit;
   end Commit;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Adv_Mutex_Access) is
   begin
      pragma Assert (M /= null
                     and then M.Current = Current_Task
                     and then M.Level > 0);
      M.Level := M.Level - 1;
      if M.Level = 0 then
         M.Current := Null_Task_Id;
         M.Mutex.Leave;
      end if;
   end Leave;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in Mutex_Access) is
   begin
      pragma Assert (M /= null);
      Abort_Undefer.all;
      M.Leave;
   end Leave;

   ----------------
   -- Mutex_Type --
   ----------------

   protected body Mutex_Type is

      -----------
      -- Enter --
      -----------

      entry Enter when not Locked is
      begin
         Locked := True;
      end Enter;

      ------------
      -- Leave --
      ------------

      procedure Leave is
      begin
         Locked := False;
      end Leave;

   end Mutex_Type;

   ----------------------
   -- String_To_Access --
   ----------------------

   function String_To_Access (S : String) return String_Access is
   begin
      return new String'(S);
   end String_To_Access;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (Item : in out String) is
   begin
      for I in Item'Range loop
         if Item (I) in 'A' .. 'Z' then
            Item (I) :=
               Character'Val (Character'Pos (Item (I)) -
                              Character'Pos ('A') +
                              Character'Pos ('a'));
         end if;
      end loop;
   end To_Lower;

   ------------
   -- Update --
   ------------

   procedure Update (W : in Watcher_Access) is
   begin
      W.Update;
   end Update;

   ------------------
   -- Watcher_Type --
   ------------------

   protected body Watcher_Type is

      -----------
      -- Await --
      -----------

      entry Await (V : in Version_Id) when not Passing is
      begin
         if Await'Count = 0 then
            Passing := True;
         end if;
         requeue Differ with abort;
      end Await;

      ------------
      -- Commit --
      ------------

      function Commit return Version_Id is
      begin
         return Current;
      end Commit;

      ------------
      -- Differ --
      ------------

      entry Differ (V : in Version_Id) when Passing is
      begin
         if Current = V then
            requeue Await with abort;
         end if;
      end Differ;

      ------------
      -- Update --
      ------------

      procedure Update is
      begin
         Current := Current + 1;
         if Await'Count /= 0 then
            Passing := False;
         end if;
      end Update;

   end Watcher_Type;

end System.Garlic.Utils;
