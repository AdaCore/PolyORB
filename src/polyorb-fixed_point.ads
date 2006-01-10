------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . F I X E D _ P O I N T                   --
--                                                                          --
--                                 S p e c                                  --
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

package PolyORB.Fixed_Point is

   pragma Pure;

   type Nibble is range 16#0# .. 16#f#;

   subtype Decimal_Nibble is Nibble range 0 .. 9;

   Positive_Zero : constant Nibble;
   Negative      : constant Nibble;

   type Nibbles is array (Integer range <>) of Nibble;
   pragma Pack (Nibbles);

   generic
      type F is delta <> digits <>;
   package Fixed_Point_Conversion is
      function Fixed_To_Nibbles (Data : F) return Nibbles;
      function Nibbles_To_Fixed (Data : Nibbles) return F;
   end Fixed_Point_Conversion;

private

   Fixed_Positive_Zero : constant := 16#c#;
   Fixed_Negative      : constant := 16#d#;

   Positive_Zero : constant Nibble := Fixed_Positive_Zero;
   Negative      : constant Nibble := Fixed_Negative;

end PolyORB.Fixed_Point;
