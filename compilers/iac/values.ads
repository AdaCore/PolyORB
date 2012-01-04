------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               V A L U E S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Types; use Types;

with Frontend.Nodes; use Frontend.Nodes;

package Values is

   type Value_Type (K : Node_Kind := K_Float) is
      record
         case K is
            when K_Short .. K_Unsigned_Long_Long
              | K_Octet
              | K_Boolean
              | K_Fixed_Point_Type =>
               IVal : Unsigned_Long_Long;
               Sign : Short_Short;
               case K is
                  when K_Fixed_Point_Type =>
                     Total : Unsigned_Short_Short;
                     Scale : Unsigned_Short_Short;
                  when others =>
                     Base : Unsigned_Short_Short;
               end case;

            when K_Float .. K_Long_Double =>
               FVal : Long_Double;

            when K_Char .. K_Wide_Char =>
               CVal : Unsigned_Short;

            when K_String .. K_Wide_String
              | K_Enumerator =>
               SVal : Name_Id;
               case K is
                  when K_Enumerator =>
                     Pos : Unsigned_Long_Long;
                  when others =>
                     null;
               end case;

            when K_Void =>
               null;

            when others =>
               null;
         end case;
      end record;

   Bad_Value : constant Value_Type;
   No_Value  : constant Value_Id;

   function New_Boolean_Value
     (Value : Boolean) return Value_Id;

   function New_Character_Value
     (Value : Unsigned_Short;
      Wide  : Boolean) return Value_Id;

   function New_Enumerator
     (Img : Name_Id;
      Pos : Unsigned_Long_Long) return Value_Id;

   function New_Fixed_Point_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Total : Unsigned_Short_Short;
      Scale : Unsigned_Short_Short) return Value_Id;

   function New_Floating_Point_Value
     (Value : Long_Double) return Value_Id;

   function New_Integer_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short) return Value_Id;

   function New_String_Value
     (Value : Name_Id;
      Wide  : Boolean) return Value_Id;

   function New_Value
     (Value : Value_Type) return Value_Id;

   function Convert (V : Value_Type; K : Node_Kind) return Value_Type;

   Max_Digits  : constant := 31;

   procedure Normalize_Fixed_Point_Value
     (Value : in out Value_Id;
      Total : Unsigned_Short_Short := Max_Digits;
      Scale : Unsigned_Short_Short := Max_Digits);

   procedure Normalize_Fixed_Point_Value
     (Value : in out Value_Type;
      Total : Unsigned_Short_Short := Max_Digits;
      Scale : Unsigned_Short_Short := Max_Digits);

   function Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   function Image (Value : Value_Id) return String;
   function Image_Ada (Value : Value_Id) return String;

   function "not" (R : Value_Type) return Value_Type;
   function "-"   (R : Value_Type) return Value_Type;
   function "-"   (L, R : Value_Type) return Value_Type;
   function "+"   (L, R : Value_Type) return Value_Type;
   function "mod" (L, R : Value_Type) return Value_Type;
   function "/"   (L, R : Value_Type) return Value_Type;
   function "*"   (L, R : Value_Type) return Value_Type;
   function "and" (L, R : Value_Type) return Value_Type;
   function "or"  (L, R : Value_Type) return Value_Type;
   function "xor" (L, R : Value_Type) return Value_Type;
   function Shift_Left  (L, R : Value_Type) return Value_Type;
   function Shift_Right (L, R : Value_Type) return Value_Type;

   function "<"   (L, R : Value_Type) return Boolean;
   --  Assume L and R have the same type.

   function Negative (V : Value_Type) return Boolean;
   function Negative (V : Value_Id) return Boolean;
   --  Return True when R is a strictly negative number. Raise an exception if
   --  if R is not a number.

private

   Bad_Value : constant Value_Type := Value_Type'((K => K_Void));
   No_Value  : constant Value_Id := 0;

end Values;
