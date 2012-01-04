------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Ada.Command_Line;
with Ada.Directories;
with GNAT.Directory_Operations; use GNAT;
with Namet;                     use Namet;
with Platform;

package body Utils is

   Up_To_Low : constant := Character'Pos ('A') - Character'Pos ('a');

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
      Up : Boolean := True;
   begin
      for I in S'Range loop
         if Up then
            Up := False;
            if S (I) in 'a' .. 'z' then
               S (I) := Character'Val (Character'Pos (S (I)) + Up_To_Low);
            end if;
         end if;
         if S (I) = '_' then
            Up := True;
         end if;
      end loop;
   end Capitalize;

   -----------
   -- Image --
   -----------

   function Image (N : Int) return String is
      S : constant String := Int'Image (N);
   begin
      case S (S'First) is
         when ' ' => return S (S'First + 1 .. S'Last);
         when '-' => return S;
         when others => raise Program_Error;
      end case;
   end Image;

   ----------------------
   -- Is_Dir_Separator --
   ----------------------

   function Is_Dir_Separator (C : Character) return Boolean is
   begin
      return C = Directory_Operations.Dir_Separator or else C = '/';
   end Is_Dir_Separator;

   ------------
   -- Quoted --
   ------------

   function Quoted (S : String; D : Character := '"') return String is -- "
   begin
      return (1 => D) & S & (1 => D);
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (S : String; D : Character := '"') return Name_Id is -- "
   begin
      Set_Char_To_Name_Buffer (D);
      Add_Str_To_Name_Buffer (S);
      Add_Char_To_Name_Buffer (D);
      return Name_Find;
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (N : Name_Id; D : Character := '"') return String is -- "
   begin
      return Quoted (Get_Name_String (N), D);
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (N : Name_Id; D : Character := '"') return Name_Id is -- "
   begin
      return Quoted (Get_Name_String (N), D);
   end Quoted;

   -------------------------
   -- Simple_Command_Name --
   -------------------------

   function Simple_Command_Name return String is
      use Ada, Ada.Directories;
      Name : constant String := Simple_Name (Command_Line.Command_Name);
      Exe : constant String := "exe";
   begin
      if Platform.Windows_On_Host then
         if To_Lower (Extension (Name)) = Exe then
            return Base_Name (Name);
         end if;
      end if;
      return Name;
   end Simple_Command_Name;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in out String) is
   begin
      S := Ada.Characters.Handling.To_Lower (S);
   end To_Lower;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (N : Name_Id) return Name_Id is
   begin
      if N = No_Name then
         return No_Name;
      end if;
      Get_Name_String (N);
      To_Lower (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end To_Lower;

end Utils;
