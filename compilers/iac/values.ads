------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               V A L U E S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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
     (Value : Boolean)
     return Value_Id;

   function New_Character_Value
     (Value : Unsigned_Short;
      Wide  : Boolean)
     return Value_Id;

   function New_Enumerator
     (Img : Name_Id;
      Pos : Unsigned_Long_Long)
     return Value_Id;

   function New_Fixed_Point_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Total : Unsigned_Short_Short;
      Scale : Unsigned_Short_Short)
     return Value_Id;

   function New_Floating_Point_Value
     (Value : Long_Double)
     return Value_Id;

   function New_Integer_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short)
     return Value_Id;

   function New_String_Value
     (Value : Name_Id;
      Wide  : Boolean)
     return Value_Id;

   function New_Value
     (Value : Value_Type)
     return Value_Id;

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
   --  Return True when R is a strictly negative number. Raise an
   --  error if R is not a number.

private

   Bad_Value : constant Value_Type := Value_Type'((K => K_Void));
   No_Value  : constant Value_Id := 0;

end Values;
