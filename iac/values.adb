with Namet;  use Namet;
with Output; use Output;
with Types;  use Types;

with GNAT.Table;

package body Values is

   Hex      : constant String := "0123456789ABCDEF";

   package VT is
      new GNAT.Table (Value_Type, Value_Id, No_Value + 1, 10, 10);

   subtype ULL is Unsigned_Long_Long;

   procedure Add_ULL_To_Name_Buffer (U : ULL; B : ULL);

   Max : constant Unsigned_Long_Long := LULL / 10;

   ----------------------------
   -- Add_ULL_To_Name_Buffer --
   ----------------------------

   procedure Add_ULL_To_Name_Buffer (U : ULL; B : ULL)
   is
      Q : constant ULL := U / B;
      R : constant ULL := U mod B;
   begin
      if Q /= 0 then
         Add_ULL_To_Name_Buffer (Q, B);
      end if;
      Add_Char_To_Name_Buffer (Hex (Natural (R + 1)));
   end Add_ULL_To_Name_Buffer;

   -----------
   -- Image --
   -----------

   function Image (Value : Value_Id) return String
   is
      V : Value_Type;
   begin
      if Value = No_Value then
         return "<>";
      end if;
      V := VT.Table (Value);
      Name_Len := 0;
      case V.T is
         when T_Boolean_Literal =>
            if V.IVal = 1 then
               return "TRUE";
            else
               return "FALSE";
            end if;

         when T_Integer_Literal =>
            if V.Sign < 0 then
               Add_Char_To_Name_Buffer ('-');
            elsif V.Base = 16 then
               Add_Str_To_Name_Buffer ("0X");
            elsif V.Base = 8 then
               Add_Char_To_Name_Buffer ('0');
            end if;
            Add_ULL_To_Name_Buffer (V.IVal, ULL (V.Base));

         when T_Fixed_Point_Literal =>
            Add_Char_To_Name_Buffer ('[');
            Add_Dnat_To_Name_Buffer (Dint (V.IVal));
            Add_Char_To_Name_Buffer (',');
            Add_Dnat_To_Name_Buffer (Dint (V.Total));
            Add_Char_To_Name_Buffer (',');
            Add_Dnat_To_Name_Buffer (Dint (V.Scale));
            Add_Char_To_Name_Buffer (']');
            if V.Sign < 0 then
               Add_Char_To_Name_Buffer ('-');
            end if;
            Add_ULL_To_Name_Buffer  (V.IVal / 10 ** Natural (V.Scale), 10);
            if V.Scale > 0 then
               Add_Char_To_Name_Buffer ('.');
               Add_ULL_To_Name_Buffer (V.IVal mod 10 ** Natural (V.Scale), 10);
            end if;
            Add_Char_To_Name_Buffer ('D');

         when T_Floating_Point_Literal =>
            Add_Str_To_Name_Buffer (Long_Long_Float'Image (V.FVal));

         when T_Character_Literal =>
            if V.Wide then
               Add_Char_To_Name_Buffer ('L');
            end if;
            Add_Char_To_Name_Buffer (''');
            if V.CVal <= 127 then
               Add_Char_To_Name_Buffer (Character'Val (Natural (V.CVal)));
            else
               Add_Str_To_Name_Buffer ("\u");
               Add_ULL_To_Name_Buffer (ULL (V.CVal), 16);
            end if;
            Add_Char_To_Name_Buffer (''');

         when T_String_Literal =>
            if V.SVal = No_Name then
               return "<>";
            end if;
            if V.Wide then
               Add_Char_To_Name_Buffer ('L');
               Add_Char_To_Name_Buffer ('"'); -- "
               declare
                  S : constant String := Get_Name_String (V.SVal);
                  I : Natural := 0;
                  L : constant Natural := S'Last;
                  C : Natural;
               begin
                  while I < L loop
                     I := I + 1;
                     if S (I) = ASCII.NUL then
                        I := I + 1;
                        Add_Char_To_Name_Buffer (S (I));
                     else
                        Add_Str_To_Name_Buffer  ("""""\u");
                        C := Character'Pos (S (I));
                        Add_Char_To_Name_Buffer (Hex (C / 16 + 1));
                        Add_Char_To_Name_Buffer (Hex (C mod 16 + 1));
                        I := I + 1;
                        C := Character'Pos (S (I));
                        Add_Char_To_Name_Buffer (Hex (C / 16 + 1));
                        Add_Char_To_Name_Buffer (Hex (C mod 16 + 1));
                        Add_Str_To_Name_Buffer  ("""""");
                     end if;
                  end loop;
               end;
               Add_Char_To_Name_Buffer ('"'); -- "
            else
               Add_Char_To_Name_Buffer ('"'); -- "
               Get_Name_String_And_Append (V.SVal);
               Add_Char_To_Name_Buffer ('"'); -- "
            end if;

         when others =>
            raise Program_Error;
      end case;

      return Name_Buffer (1 .. Name_Len);
   end Image;

   -----------------------
   -- New_Boolean_Value --
   -----------------------

   function New_Boolean_Value
     (Value : Boolean)
     return Value_Id is
   begin
      return New_Value (Value_Type'(T_Boolean_Literal, Value));
   end New_Boolean_Value;

   -------------------------
   -- New_Character_Value --
   -------------------------

   function New_Character_Value
     (Value : Unsigned_Short;
      Wide  : Boolean)
     return Value_Id is
   begin
      return New_Value (Value_Type'(T_Character_Literal, Wide, Value));
   end New_Character_Value;

   ---------------------------
   -- New_Fixed_Point_Value --
   ---------------------------

   function New_Fixed_Point_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Total : Unsigned_Short_Short;
      Scale : Unsigned_Short_Short)
     return Value_Id
   is
      V : Value_Id;
   begin
      V := New_Value
        (Value_Type'(T_Fixed_Point_Literal, Value, Sign, Total, Scale));
      Normalize_Fixed_Point_Value (V);
      Write_Str (Image (V)); Write_Eol;
      return V;
   end New_Fixed_Point_Value;

   ------------------------------
   -- New_Floating_Point_Value --
   ------------------------------

   function New_Floating_Point_Value
     (Value : Long_Long_Float)
     return Value_Id is
   begin
      return New_Value (Value_Type'(T_Floating_Point_Literal, Value));
   end New_Floating_Point_Value;

   -----------------------
   -- New_Integer_Value --
   -----------------------

   function New_Integer_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short)
     return Value_Id is
   begin
      return New_Value (Value_Type'(T_Integer_Literal, Value, Sign, Base));
   end New_Integer_Value;

   ----------------------
   -- New_String_Value --
   ----------------------

   function New_String_Value
     (Value : Name_Id;
      Wide  : Boolean)
     return Value_Id is
   begin
      return New_Value (Value_Type'(T_String_Literal, Wide, Value));
   end New_String_Value;

   ---------------
   -- New_Value --
   ---------------

   function New_Value (Value : Value_Type) return Value_Id
   is
      V : Value_Id;
   begin
      VT.Increment_Last;
      V := VT.Last;
      VT.Table (V) := Value;
      return V;
   end New_Value;

   ---------------------------------
   -- Normalize_Fixed_Point_Value --
   ---------------------------------

   procedure Normalize_Fixed_Point_Value (V : Value_Id) is
      Val : Value_Type := Value (V);
   begin
      Normalize_Fixed_Point_Value (Val);
      Set_Value (V, Val);
   end Normalize_Fixed_Point_Value;

   ---------------------------------
   -- Normalize_Fixed_Point_Value --
   ---------------------------------

   procedure Normalize_Fixed_Point_Value (V : in out Value_Type) is
   begin
      while V.Scale > 0 and then V.IVal mod 10 = 0 loop
         V.IVal  := V.IVal / 10;
         V.Scale := V.Scale - 1;
         V.Total := V.Total - 1;
      end loop;
   end Normalize_Fixed_Point_Value;

   -----------
   -- "not" --
   -----------

   function "not" (R : Value_Type) return Value_Type
   is
      V : Value_Type := R;
   begin
      case V.T is
         when T_Integer_Literal =>
            V.IVal := not V.IVal;

         when T_Boolean_Literal =>
            V.BVal := not V.BVal;

         when others =>
            return Void;
      end case;
      return V;
   end "not";

   ---------
   -- "-" --
   ---------

   function "-" (R : Value_Type) return Value_Type
   is
      V : Value_Type := R;
   begin
      case R.T is
         when T_Integer_Literal
           | T_Fixed_Point_Literal =>
            V.Sign := -V.Sign;

         when T_Floating_Point_Literal =>
            V.FVal := -V.FVal;

         when others =>
            return Void;

      end case;
      return V;
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Value_Type) return Value_Type
   is
      V : Value_Type := R;
   begin
      case R.T is
         when T_Integer_Literal
           | T_Fixed_Point_Literal =>
            V.Sign := -V.Sign;

         when T_Floating_Point_Literal =>
            V.FVal := -V.FVal;

         when others =>
            return Void;

      end case;
      return L + V;
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Value_Type) return Value_Type
   is
      V  : Value_Type := R;
      NL : Value_Type := L;
      NR : Value_Type := R;
   begin
      case R.T is
         when T_Integer_Literal =>
            if L.Base /= R.Base then
               V.Base := 10;
            end if;
            if L.Sign = R.Sign then
               V.IVal := L.IVal + R.IVal;
            elsif R.IVal <= L.IVal then
               V.Sign := L.Sign;
               V.IVal := L.IVal - R.IVal;
            else
               V.Sign := -L.Sign;
               V.IVal := R.IVal - L.IVal;
            end if;

         when T_Fixed_Point_Literal =>
            if NL.Scale > NR.Scale then
               NR.IVal := NR.IVal * 10 ** Integer (L.Scale - R.Scale);
               V.Scale := NL.Scale;
            else
               NL.IVal := L.IVal * 10 ** Integer (R.Scale - L.Scale);
               V.Scale := NR.Scale;
            end if;
            if NL.Sign = NR.Sign then
               V.IVal := NL.IVal + NR.IVal;
            elsif NR.IVal <= NL.IVal then
               V.Sign := NL.Sign;
               V.IVal := NL.IVal - NR.IVal;
            else
               V.Sign := -NL.Sign;
               V.IVal := NR.IVal - NL.IVal;
            end if;
            Normalize_Fixed_Point_Value (V);

         when T_Floating_Point_Literal =>
            V.FVal := L.FVal + R.FVal;

         when others =>
            return Void;

      end case;
      return V;
   end "+";

   -----------
   -- "mod" --
   -----------

   function "mod" (L, R : Value_Type) return Value_Type
   is
      V : Value_Type := L;
   begin
      case L.T is
         when T_Integer_Literal =>
            if L.Base /= R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal mod R.IVal;

         when others =>
            return Void;
      end case;
      return V;
   end "mod";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Value_Type) return Value_Type
   is
      V  : Value_Type := L;
      NL : Value_Type := L;
   begin
      case V.T is
         when T_Integer_Literal =>
            if L.Base = R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal / R.IVal;

         when T_Floating_Point_Literal =>
            V.FVal := L.FVal / R.FVal;

         when T_Fixed_Point_Literal =>
            while NL.IVal < Max loop
               NL.IVal  := NL.IVal * 10;
               NL.Total := NL.Total + 1;
               NL.Scale := NL.Scale + 1;
            end loop;
            V.Sign   := L.Sign * R.Sign;
            V.IVal   := NL.IVal / R.IVal;
            declare
               Q : Unsigned_Long_Long := V.IVal / 10;
            begin
               V.Total := 1;
               while Q /= 0 loop
                  Q := Q / 10;
                  V.Total := V.Total + 1;
               end loop;
            end;
            V.Scale := NL.Scale - R.Scale;
            Normalize_Fixed_Point_Value (V);

         when others =>
            return Void;
      end case;
      return V;
   end "/";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Value_Type) return Value_Type
   is
      V : Value_Type := L;
   begin
      case V.T is
         when T_Integer_Literal =>
            if L.Base = R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal * R.IVal;

         when T_Fixed_Point_Literal =>
            V.Sign  := L.Sign * R.Sign;
            V.IVal  := L.IVal * R.IVal;
            V.Total := L.Total + R.Total;
            V.Scale := L.Scale + R.Scale;
            Normalize_Fixed_Point_Value (V);

         when T_Floating_Point_Literal =>
            V.FVal := L.FVal * R.FVal;

         when others =>
            return Void;
      end case;
      return V;
   end "*";

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Value_Type) return Value_Type
   is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case L.T is
         when T_Integer_Literal =>
            if LV.Base /= RV.Base then
               LV.Base := 10;
            end if;
            if LV.Sign < 0 then
               LV.IVal := LULL - LV.IVal;
            end if;
            if RV.Sign < 0 then
               RV.IVal := LULL - RV.IVal;
            end if;
            LV.IVal := LV.IVal and RV.IVal;
            LV.Sign := 1;

         when T_Boolean_Literal =>
            LV.BVal := LV.BVal and RV.BVal;

         when others =>
            return Void;
      end case;
      return LV;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Value_Type) return Value_Type
   is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case L.T is
         when T_Integer_Literal =>
            if LV.Base /= RV.Base then
               LV.Base := 10;
            end if;
            if LV.Sign < 0 then
               LV.IVal := LULL - LV.IVal;
            end if;
            if RV.Sign < 0 then
               RV.IVal := LULL - RV.IVal;
            end if;
            LV.IVal := LV.IVal or RV.IVal;
            LV.Sign := 1;

         when T_Boolean_Literal =>
            LV.BVal := LV.BVal or RV.BVal;

         when others =>
            return Void;
      end case;
      return LV;
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (L, R : Value_Type) return Value_Type
   is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case LV.T is
         when T_Integer_Literal =>
            if LV.Base /= RV.Base then
               LV.Base := 10;
            end if;
            if LV.Sign < 0 then
               LV.IVal := LULL - LV.IVal;
            end if;
            if RV.Sign < 0 then
               RV.IVal := LULL - RV.IVal;
            end if;
            LV.IVal := LV.IVal xor RV.IVal;
            LV.Sign := 1;

         when T_Boolean_Literal =>
            LV.BVal := LV.BVal xor RV.BVal;

         when others =>
            return Void;
      end case;
      return LV;
   end "xor";

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (V : Value_Id; X : Value_Type) is
   begin
      VT.Table (V) := X;
   end Set_Value;

   ----------------
   -- Shift_Left --
   ----------------

   function Shift_Left (L, R : Value_Type) return Value_Type
   is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case RV.T is
         when T_Integer_Literal =>
            if RV.Sign < 0 then
               RV.Sign := 1;
               return Shift_Right (LV, RV);
            end if;
            if LV.Base /= RV.Base then
               LV.Base := 10;
            end if;
            LV.IVal := Shift_Left (LV.IVal, Natural (RV.IVal));
            return LV;

         when others =>
            return Void;
      end case;
   end Shift_Left;

   -----------------
   -- Shift_Right --
   -----------------

   function Shift_Right (L, R : Value_Type) return Value_Type
   is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case RV.T is
         when T_Integer_Literal =>
            if RV.Sign < 0 then
               RV.Sign := 1;
               return Shift_Left (LV, RV);
            end if;
            if LV.Base /= RV.Base then
               LV.Base := 10;
            end if;
            LV.IVal := Shift_Right (LV.IVal, Natural (RV.IVal));
            return LV;

         when others =>
            return Void;
      end case;
   end Shift_Right;

   -----------
   -- Value --
   -----------

   function Value (V : Value_Id) return Value_Type is
   begin
      return VT.Table (V);
   end Value;

end Values;
