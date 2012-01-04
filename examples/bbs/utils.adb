------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;
use  Ada.Text_IO;

package body Utils is

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Prompt : String := "") return String is
      Line : String (1 .. 256);
      Last : Natural;
   begin
      Put (Prompt);
      Get_Line (Line, Last);
      return Line (1 .. Last);
   end Get_Line;

   -----------------------
   -- Integer_To_String --
   -----------------------

   function Integer_To_String (I : Integer) return String is
      Image : constant String := Integer'Image (I);
   begin
      if Image (1) = ' ' then
         return Image (2 .. Image'Last);
      else
         return Image;
      end if;
   end Integer_To_String;

   -----------------------
   -- String_To_Integer --
   -----------------------

   function String_To_Integer (S : String) return Integer is
   begin
      return Integer'Value (S);
   end String_To_Integer;

end Utils;
