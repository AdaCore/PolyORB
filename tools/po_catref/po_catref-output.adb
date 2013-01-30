------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O _ C A T R E F . O U T P U T                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2013, Free Software Foundation, Inc.          --
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
with Ada.Strings.Unbounded;

package body PO_Catref.Output is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Indent_Size : constant Natural := 3;
   Indent_Level : Natural := 0;

   Text : Unbounded_String;

   ----------------
   -- Inc_Indent --
   ----------------

   procedure Inc_Indent is
   begin
      Indent_Level := Indent_Level + 1;
   end Inc_Indent;

   ----------------
   -- Dec_Indent --
   ----------------

   procedure Dec_Indent is
   begin
      Indent_Level := Indent_Level - 1;
   end Dec_Indent;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Tag_C : String; Information : String) is
      Indent_String : constant String
        (1 .. Indent_Size * Indent_Level) := (others => ' ');

   begin
      Append (Text, Indent_String);
      Append (Text, Tag_C & ": " & Information & ASCII.LF);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Append (Text, ASCII.LF);
   end New_Line;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      Ada.Text_IO.Put (To_String (Text));
   end Flush;

end PO_Catref.Output;
