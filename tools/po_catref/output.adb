------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               O U T P U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 3-2004 Free Software Foundation, Inc.            --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Output is

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

end Output;
