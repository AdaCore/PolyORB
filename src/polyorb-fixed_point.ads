------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . F I X E D _ P O I N T                   --
--                                                                          --
--                                 S p e c                                  --
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
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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
