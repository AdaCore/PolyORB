------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O L Y O R B . U T I L S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If  --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body PolyORB.Utils is

   use Ada.Streams;

   ------------------------
   -- Local declarations --
   ------------------------

   Hex : constant array (16#0# .. 16#f#) of Character
     := "0123456789abcdef";

   Hex_Val : constant array (Character) of Integer
     := ('0' => 0,
         '1' => 1,
         '2' => 2,
         '3' => 3,
         '4' => 4,
         '5' => 5,
         '6' => 6,
         '7' => 7,
         '8' => 8,
         '9' => 9,
         'A' => 10,
         'a' => 10,
         'B' => 11,
         'b' => 11,
         'C' => 12,
         'c' => 12,
         'D' => 13,
         'd' => 13,
         'E' => 14,
         'e' => 14,
         'F' => 15,
         'f' => 15,
         others => -1);

   ---------------
   -- Hex_Value --
   ---------------

   function Hex_Value (C : Character) return Integer is
      V : constant Integer := Hex_Val (C);
   begin
      if V = -1 then
         raise Constraint_Error;
      else
         return V;
      end if;
   end Hex_Value;

   ---------------
   -- To_String --
   ---------------

   function To_String (A : Stream_Element_Array) return String
   is
      S : String (1 .. 2 * A'Length);
   begin
      for I in A'Range loop
         S (S'First + 2 * Integer (I - A'First))
           := Hex (Integer (A (I)) / 16);
         S (S'First + 2 * Integer (I - A'First) + 1)
           := Hex (Integer (A (I)) mod 16);
      end loop;
      return S;
   end To_String;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (S : String) return Stream_Element_Array
   is
      A : Stream_Element_Array (1 .. S'Length / 2);
   begin
      for I in A'Range loop
         A (I) :=
           Stream_Element
           (Hex_Value (S (S'First + 2 * Integer (I - A'First))) * 16
            + Hex_Value (S (S'First + 2 * Integer (I - A'First) + 1)));
      end loop;
      return A;
   end To_Stream_Element_Array;

   Need_Escape : constant array (Character) of Boolean
     := (';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' |
         '<' | '>' | '#' | '%' | '"' |
         '{' | '}' | '|' | '\' | '^' | '[' | ']' | '`' => True,
         others => False);

   function URI_Encode (S : String) return String is
      Result : String (1 .. 3 * S'Length);
      DI : Integer := Result'First;
   begin
      for SI in S'Range loop
         if Need_Escape (S (SI)) then
            Result (DI .. DI + 2)
              := '%' & Hex (Character'Pos (S (SI)) / 16)
              & Hex (Character'Pos (S (SI)) mod 16);
            DI := DI + 3;
         else
            if S (SI) = ' ' then
               Result (DI) := '+';
            else
               Result (DI) := S (SI);
            end if;
            DI := DI + 1;
         end if;
      end loop;
      return Result (Result'First .. DI - 1);
   end URI_Encode;

   function URI_Decode (S : String) return String is
      Result : String (S'Range);
      SI : Integer := S'First;
      DI : Integer := Result'First;
   begin
      while SI <= S'Last loop
         if S (SI) = '%' then
            if SI > S'Last - 2 then
               raise Constraint_Error;
            end if;
            Result (DI) := Character'Val
              (Hex_Value (S (SI + 1)) * 16 + Hex_Value (S (SI + 2)));
            SI := SI + 3;
         else
            if S (SI) = '+' then
               Result (DI) := ' ';
            else
               Result (DI) := S (SI);
            end if;
            SI := SI + 1;
         end if;
         DI := DI + 1;
      end loop;
      return Result (Result'First .. DI - 1);
   end URI_Decode;

   function Trimmed_Image (I : Integer) return String is
      R : constant String := Integer'Image (I);
   begin
      if I >= 0 then
         return R (R'First + 1 .. R'Last);
      else
         return R;
      end if;
   end Trimmed_Image;

   function Find_Skip
     (S     : String;
      Start : Integer;
      What  : Character;
      Skip  : Boolean)
     return Integer
   is
      I : Integer := Start;
   begin
      while I < S'Last loop
         I := I + 1;
         exit when I > S'Last or else (S (I) = What xor Skip);
      end loop;

      return I;
   end Find_Skip;

end PolyORB.Utils;
