------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

--  $Id$

with Ada.IO_Exceptions;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;

package body Utils is

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Prompt : String := "") return String is
      function readline (Prompt : chars_ptr := Null_Ptr) return chars_ptr;
      pragma Import (C, readline, "readline");
      pragma Linker_Options ("-lreadline");
      pragma Linker_Options ("-lncurses");
      C_Prompt : chars_ptr;
      Result   : chars_ptr;
   begin
      if Prompt = "" then
         Result := readline;
      else
         C_Prompt := New_String (Prompt);
         Result := readline (C_Prompt);
         Free (C_Prompt);
      end if;
      if Result = Null_Ptr then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      return Value (Result);
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
