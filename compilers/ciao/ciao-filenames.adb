------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C I A O . F I L E N A M E S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
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

--  Mapping of file names
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body CIAO.Filenames is

   function IDL_File_Name (Ada_File_Name : String)
     return String is
      Result : String (Ada_File_Name'Range)
        := Ada_File_Name;
   begin
      Result (Result'Last - 2 .. Result'Last) := "idl";
      return Result;
   end IDL_File_Name;

   function Ada_File_Name (Full_Name : Asis.Program_Text;
                           Part      : Unit_Part := Unit_Declaration)
     return String is
      Extension : constant array (Unit_Part) of Character
        := (Unit_Declaration => 's',
            Unit_Body        => 'b');
      Result : String := To_String (Full_Name) & ".ad?";
   begin
      for I in Result'First .. Result'Last - 4 loop
         if Result (I) = '.' then
            Result (I) := '-';
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;

      Result (Result'Last) := Extension (Part);
      return Result;
   end Ada_File_Name;

end CIAO.Filenames;
