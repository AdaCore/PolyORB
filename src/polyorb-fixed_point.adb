------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . F I X E D _ P O I N T                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Generic representation for fixed point types as an array
--  of BCD nibbles followed by a sign indication.

package body PolyORB.Fixed_Point is

   package body Fixed_Point_Conversion is

      Max_Nibbles : constant Integer := 2 * ((F'Digits + 2) / 2);
      --  F'Digits + sign indication, rounded up towards an even
      --  number.

      ----------------------
      -- Fixed_To_Nibbles --
      ----------------------

      function Fixed_To_Nibbles (Data : F) return Nibbles is
         Result : Nibbles (1 .. Max_Nibbles) := (others => 0);

         First_Digit : Integer := Result'Last;
         Val : F := Data;

      begin
         if Data >= 0.0 then
            Result (First_Digit) := Fixed_Positive_Zero;
         else
            Result (First_Digit) := Fixed_Negative;
            Val := -Val;
         end if;

         while Val /= 0.0 loop
            First_Digit := First_Digit - 1;
            Result (First_Digit)
              := Nibble ((Val - 10 * (Val / 10)) / F'Small);
            Val := Val / 10;
         end loop;

         --  Always return a full length array, including leading zeroes

         return Result;
      end Fixed_To_Nibbles;

      ----------------------
      -- Nibbles_To_Fixed --
      ----------------------

      function Nibbles_To_Fixed (Data : Nibbles) return F is
         Result : F := 0.0;

      begin
         for J in Data'First .. Data'Last - 1 loop
            if Data (J) not in Decimal_Nibble then
               raise Constraint_Error;
            end if;
            Result := (Result * 10.0)
              + (Integer (Data (J)) * F'Small);
         end loop;

         case Data (Data'Last) is

            when Fixed_Positive_Zero =>
               null;
            when Fixed_Negative =>
               Result := -Result;
            when others =>
               raise Constraint_Error;
         end case;

         return Result;
      end Nibbles_To_Fixed;
   end Fixed_Point_Conversion;

end PolyORB.Fixed_Point;
