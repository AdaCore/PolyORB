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
with Ada.Task_Identification;    use Ada.Task_Identification;
with Interfaces.C;               use Interfaces.C;
with System.Garlic.OS_Lib;       use System.Garlic.OS_Lib;
with System.RPC;                 use System.RPC;

package body System.Garlic.Utils is

   Upper_To_Lower : constant := Character'Pos ('a') - Character'Pos ('A');

   use Ada.Exceptions;

   function Not_Null_Version (V : in String) return Boolean;
   --  Returns true when V is not a string of blank characters.

   protected Section is
      entry     Lock    (Self : Task_Id);
      procedure Unlock;
   private
      entry Wait (Self : Task_Id);
      Current   : Task_Id;
      Level     : Natural := 0;
      Requeuing : Boolean := False;
   end Section;

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
   begin
      return     Not_Null_Version (V1)
        and then Not_Null_Version (V2)
        and then V1 /= V2;
   end Different;

   -----------
   -- Enter --
   -----------

   procedure Enter is
      Self : Task_Id := Current_Task;

   begin
      Section.Lock (Self);
   end Enter;

   -----------
   -- Leave --
   -----------

   procedure Leave is
   begin
      Section.Unlock;
   end Leave;

   ----------------------
   -- Not_Null_Version --
   ----------------------

   function Not_Null_Version (V : in String) return Boolean is
   begin
      for I in V'Range loop
         if V (I) /= ' ' then
            return True;
         end if;
      end loop;
      return False;
   end Not_Null_Version;

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

   -------------
   -- Section --
   -------------

   protected body Section is

      -----------
      -- Lock --
      -----------

      entry Lock (Self : Task_Id) when not Requeuing is
      begin
         if Level /= 0 and then Current /= Self then
            requeue Wait with abort;
         end if;
         Current := Self;
         Level   := Level + 1;
      end Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         Level := Level - 1;
         if Level = 0 and then Wait'Count > 0 then
            Requeuing := True;
         end if;
      end Unlock;

      ----------
      -- Wait --
      ----------

      entry Wait (Self : Task_Id) when Requeuing is
      begin
         if Wait'Count = 0 then
            Requeuing := False;
         end if;
         requeue Lock with abort;
      end Wait;

   end Section;

   --------------------
   -- Semaphore_Type --
   --------------------

   protected body Semaphore_Type is

      ----------
      -- Lock --
      ----------

      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      ------------
      -- Unlock --
      ------------

      entry Unlock (Result : Status_Type := Unmodified)
      when Status /= Modified is
      begin
         Locked := False;
         case Result is
            when Modified =>
               if Wait'Count > 0 then
                  Status := Modified;
               end if;
            when Postponed =>
               requeue Wait with abort;
            when Unmodified =>
               null;
         end case;
      end Unlock;

      ----------
      -- Wait --
      ----------

      entry Wait (Result : Status_Type := Unmodified)
      when Status = Modified is
      begin
         if Wait'Count = 0 then
            Status := Unmodified;
         end if;
      end Wait;

   end Semaphore_Type;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (Item : in out String) is
   begin
      for I in Item'Range loop
         if Item (I) in 'A' .. 'Z' then
            Item (I) :=
               Character'Val (Character'Pos (Item (I)) + Upper_To_Lower);
         end if;
      end loop;
   end To_Lower;

end System.Garlic.Utils;
