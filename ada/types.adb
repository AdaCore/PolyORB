------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T Y P E S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

package body Types is

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Time_Stamp_Type) return Boolean is
      Thi : Time_Stamp_Type;
      Tlo : Time_Stamp_Type;

      Slo : Nat;
      Shi : Nat;

      function V (T : Time_Stamp_Type; X : Time_Stamp_Index) return Nat;
      --  Extract two decimal digit value from time stamp

      function V (T : Time_Stamp_Type; X : Time_Stamp_Index) return Nat is
      begin
         return 10 * (Character'Pos (T (X))     - Character'Pos ('0')) +
                      Character'Pos (T (X + 1)) - Character'Pos ('0');
      end V;

   --  Start of processing for "="

   begin
      if String (Left) = String (Right) then
         return True;

      elsif Left (1) = ' ' or else Right (1) = ' ' then
         return False;

      --  In the following code we check for a difference of 2 seconds or less

      elsif Left < Right then
         Tlo := Left;
         Thi := Right;

      else
         Tlo := Right;
         Thi := Left;
      end if;

      --  Now the smaller (older) of the two time stamps is in Tlo, the larger
      --  (more recent) is in Thi. Recall that the time stamp format is:

      --     Y  Y  M  M  D  D  H  H  M  M  S  S
      --    01 02 03 04 05 06 07 08 09 10 11 12

      --  Note that we do not bother to worry about shifts in the day.
      --  It seems unlikely that such shifts could ever occur in practice
      --  and even if they do we err on the safe side, ie we say that the time
      --  stamps are different.

      Slo := V (Tlo, 11) + 60 * (V (Tlo, 09) + 60 * V (Tlo, 07));
      Shi := V (Thi, 11) + 60 * (V (Thi, 09) + 60 * V (Thi, 07));

      --  So the check is: dates must be the same, times differ 2 sec at most

      return Shi <= Slo + 2
         and then String (Left (1 .. 6)) = String (Right (1 .. 6));
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      --  The following test deals with year 2000 problems. Consider any
      --  date before 700101 to be in the next century.

      if Left (1) in '7' .. '9' and then Right (1) in '0' .. '6' then
         return True;

      elsif Left (1) in '0' .. '6' and then Right (1) in '7' .. '9' then
         return False;

      --  If not year 2000 special case, then use standard string comparison.
      --  Note that this has the right semantics for Empty_Time_Stamp.

      else
         return String (Left) < String (Right);
      end if;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      --  The following test deals with year 2000 problems. Consider any
      --  date before 700101 to be in the next century.

      if Left (1) in '7' .. '9' and then Right (1) in '0' .. '6' then
         return True;

      elsif Left (1) in '0' .. '6' and then Right (1) in '7' .. '9' then
         return False;

      --  If not year 2000 special case, then use standard string comparison.
      --  Note that this has the right semantics for Empty_Time_Stamp.

      else
         return String (Left) <= String (Right);
      end if;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left <= Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   -------------------
   -- Get_Char_Code --
   -------------------

   function Get_Char_Code (C : Character) return Char_Code is
   begin
      return Char_Code'Val (Character'Pos (C));
   end Get_Char_Code;

   -------------------
   -- Get_Character --
   -------------------

   --  Note: raises Constraint_Error if checks on and C out of range

   function Get_Character (C : Char_Code) return Character is
   begin
      return Character'Val (C);
   end Get_Character;

   --------------------
   -- Get_Hex_String --
   --------------------

   subtype Wordh is Word range 0 .. 15;
   Hex : constant array (Wordh) of Character := "0123456789ABCDEF";

   function Get_Hex_String (W : Word) return Word_Hex_String is
      X  : Word := W;
      WS : Word_Hex_String;

   begin
      for J in reverse 1 .. 8 loop
         WS (J) := Hex (X mod 16);
         X := X / 16;
      end loop;

      return WS;
   end Get_Hex_String;

   ------------------------
   -- In_Character_Range --
   ------------------------

   function In_Character_Range (C : Char_Code) return Boolean is
   begin
      return (C <= 255);
   end In_Character_Range;

end Types;
