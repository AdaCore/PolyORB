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
      entry Leave (S : Status_Type := Unmodified);
   private
      entry Wait (S : Status_Type);
      Locked : Boolean := False;
      Status : Status_Type := Unmodified;
   end Mutex_Type;
   --  A task can enter in a mutex if and only if there is no other task in
   --  a mutex. A task can leave a mutex returning the status of its
   --  action.  Unmodified corresponds to no modification of the protected
   --  data. Postponed indicates that the task would like to be resumed
   --  when the protected data is modified (in order to enter the mutex
   --  another time). Modified corresponds to a modification and resume
   --  all the tasks pending for a modification of the protected data.

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

   ----------------------
   -- Access_To_String --
   ----------------------

   function Access_To_String (S : String_Access) return String is
   begin
      return S.all;
   end Access_To_String;

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

   procedure Leave (M : in Mutex_Access; S : in Status_Type := Unmodified) is
   begin
      pragma Assert (M /= null);
      Abort_Undefer.all;
      M.Leave (S);
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
         --  Abort_Defer.all;
         Locked := True;
      end Enter;

      ------------
      -- Leave --
      ------------

      entry Leave (S : Status_Type := Unmodified)
      when Status /= Modified is
      begin
         Locked := False;
         --  Abort_Undefer.all;
         case S is
            when Modified =>
               if Wait'Count > 0 then
                  Status := Modified;
               end if;
            when Postponed =>
               requeue Wait with abort;
            when Unmodified =>
               null;
         end case;
      end Leave;

      ----------
      -- Wait --
      ----------

      entry Wait (S : Status_Type)
      when Status = Modified is
      begin
         if Wait'Count = 0 then
            Status := Unmodified;
         end if;
      end Wait;

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

end System.Garlic.Utils;
