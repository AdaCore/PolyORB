with Lexer; use Lexer;
with Types; use Types;

package Values is

   type Value_Type (T : Token_Type := T_Integer_Literal) is record
      case T is
         when T_Integer_Literal
           | T_Fixed_Point_Literal =>
            IVal : Unsigned_Long_Long;
            Sign : Short_Short;
            case T is
               when T_Integer_Literal =>
                  Base : Unsigned_Short_Short;
               when T_Fixed_Point_Literal =>
                  Total : Unsigned_Short_Short;
                  Scale : Unsigned_Short_Short;
               when others =>
                  null;
            end case;

         when T_Floating_Point_Literal =>
            FVal : Long_Long_Float;

         when T_Character_Literal
           | T_String_Literal =>
            Wide : Boolean;
            case T is
               when T_Character_Literal =>
                  CVal : Unsigned_Short;
               when T_String_Literal =>
                  SVal : Name_Id;
               when others =>
                  null;
            end case;

         when T_Boolean_Literal =>
            BVal : Boolean;

         when others =>
            null;
      end case;
   end record;

   Void     : constant Value_Type;
   No_Value : constant Value_Id;

   function New_Integer_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short)
     return Value_Id;

   function New_Boolean_Value
     (Value : Boolean)
     return Value_Id;

   function New_Fixed_Point_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Total : Unsigned_Short_Short;
      Scale : Unsigned_Short_Short)
     return Value_Id;

   function New_Floating_Point_Value
     (Value : Long_Long_Float)
     return Value_Id;

   function New_Character_Value
     (Value : Unsigned_Short;
      Wide  : Boolean)
     return Value_Id;

   function New_String_Value
     (Value : Name_Id;
      Wide  : Boolean)
     return Value_Id;

   function New_Value
     (Value : Value_Type)
     return Value_Id;

   function Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   function Image (Value : Value_Id) return String;

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

private

   Void : constant Value_Type := Value_Type'((T => T_Void));
   No_Value : constant Value_Id := 0;

end Values;

