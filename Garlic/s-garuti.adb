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
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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
with Interfaces.C;               use Interfaces.C;
with System.Garlic.OS_Lib;       use System.Garlic.OS_Lib;
with System.RPC;                 use System.RPC;

package body System.Garlic.Utils is

   use Ada.Exceptions, Ada.Task_Identification;

   --------------
   -- Allocate --
   --------------

   function Allocate return Adv_Mutex_Access is
      Item : Adv_Mutex_Access := new Adv_Mutex_Type;

   begin
      Item.Mutex   := new Mutex_Type;
      Item.Current := Null_Task_Id;
      return Item;
   end Allocate;

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

   ---------------
   -- Different --
   ---------------

   function Different (V1, V2 : String) return Boolean is

      function Not_Null_Version (V : in String) return Boolean;
      --  Return true when V is not a string of blank characters

      ----------------------
      -- Not_Null_Version --
      ----------------------

      function Not_Null_Version (V : in String) return Boolean is
         Null_String : constant String (V'Range) := (others => ' ');
      begin
         return V /= Null_String;
      end Not_Null_Version;

   begin
      return     Not_Null_Version (V1)
        and then Not_Null_Version (V2)
        and then V1 /= V2;
   end Different;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : Adv_Mutex_Access) is
      Self : Task_Id := Current_Task;

   begin
      if M.Current /= Self then
         M.Mutex.Enter;
         M.Current := Self;
      end if;
      M.Level := M.Level + 1;
   end Enter;

   ----------
   -- Free --
   ----------

   procedure Free (M : in out Adv_Mutex_Access) is
      procedure Local_Free is
        new Ada.Unchecked_Deallocation (Adv_Mutex_Type, Adv_Mutex_Access);
   begin
      Free (M.Mutex);
      Local_Free (M);
   end Free;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : Adv_Mutex_Access; S : Status_Type := Unmodified) is
   begin
      M.Level := M.Level - 1;
      if M.Level = 0 then
         M.Current := Null_Task_Id;
         M.Mutex.Leave (S);
      end if;
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

      entry Leave (S : Status_Type := Unmodified)
      when Status /= Modified is
      begin
         Locked := False;
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

   -------------------------------
   -- Raise_Communication_Error --
   -------------------------------

   procedure Raise_Communication_Error (Msg : in String := "") is
   begin
      if Msg = "" then
         Raise_With_Errno (System.RPC.Communication_Error'Identity);
      else
         Raise_Exception (System.RPC.Communication_Error'Identity, Msg);
      end if;
   end Raise_Communication_Error;

   ----------------------
   -- Raise_With_Errno --
   ----------------------

   procedure Raise_With_Errno (Id : in Exception_Id) is
   begin
      Raise_Exception (Id, "Error" & int'Image (C_Errno));
   end Raise_With_Errno;

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

begin
   Global_Mutex := Allocate;
end System.Garlic.Utils;
