------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               V A L U E S                                --
--                                                                          --
--                                 B o d y                                  --
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

with Namet;  use Namet;

with GNAT.Table;

package body Values is

   Hex      : constant String := "0123456789ABCDEF";

   package VT is
      new GNAT.Table (Value_Type, Value_Id, No_Value + 1, 10, 10);

   subtype ULL is Unsigned_Long_Long;

   procedure Add_ULL_To_Name_Buffer (U : ULL; B : ULL; S : Integer := 1);

   LULL_Div_10 : constant Unsigned_Long_Long := LULL / 10;

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Value_Type) return Value_Type
   is
      V : Value_Type := L;
   begin
      case V.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
            if L.Base = R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal * R.IVal;

         when K_Fixed_Point_Type =>
            V.Sign  := L.Sign * R.Sign;
            V.IVal  := L.IVal * R.IVal;
            V.Total := L.Total + R.Total;
            V.Scale := L.Scale + R.Scale;
            Normalize_Fixed_Point_Value (V);

         when K_Float .. K_Long_Double =>
            V.FVal := L.FVal * R.FVal;

         when others =>
            return Bad_Value;
      end case;
      return V;
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Value_Type) return Value_Type
   is
      V  : Value_Type := R;
      NL : Value_Type := L;
      NR : Value_Type := R;
   begin
      case R.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
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

         when K_Fixed_Point_Type =>
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

         when K_Float .. K_Long_Double =>
            V.FVal := L.FVal + R.FVal;

         when others =>
            return Bad_Value;

      end case;
      return V;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (R : Value_Type) return Value_Type
   is
      V : Value_Type := R;
   begin
      case R.K is
         when K_Short .. K_Unsigned_Long_Long
           | K_Octet
           | K_Fixed_Point_Type =>
            V.Sign := -V.Sign;

         when K_Float .. K_Long_Double =>
            V.FVal := -V.FVal;

         when others =>
            return Bad_Value;

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
      case R.K is
         when K_Short .. K_Unsigned_Long_Long
           | K_Octet
           | K_Fixed_Point_Type =>
            V.Sign := -V.Sign;

         when K_Float .. K_Long_Double =>
            V.FVal := -V.FVal;

         when others =>
            return Bad_Value;

      end case;
      return L + V;
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Value_Type) return Value_Type
   is
      V  : Value_Type := L;
      NL : Value_Type := L;
   begin
      case V.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
            if L.Base = R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal / R.IVal;

         when K_Float .. K_Long_Double =>
            V.FVal := L.FVal / R.FVal;

         when K_Fixed_Point_Type =>
            while NL.IVal < LULL_Div_10 loop
               NL.IVal  := NL.IVal * 10;
               NL.Total := NL.Total + 1;
               NL.Scale := NL.Scale + 1;
            end loop;
            V.Sign  := L.Sign * R.Sign;
            V.IVal  := NL.IVal / R.IVal;
            V.Scale := NL.Scale - R.Scale;
            Normalize_Fixed_Point_Value (V);

         when others =>
            return Bad_Value;
      end case;
      return V;
   end "/";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Value_Type) return Boolean is
   begin
      case R.K is
         when K_Short .. K_Unsigned_Long_Long
           |  K_Octet
           |  K_Boolean =>
            if L.Sign > 0 then
               if R.Sign > 0 then
                  return L.IVal < R.IVal;
               else
                  return False;
               end if;
            elsif R.Sign > 0 then
               return True;
            else
               return L.IVal > R.IVal;
            end if;

         when K_Enumerator =>
            return L.Pos < R.Pos;

         when K_Fixed_Point_Type =>
            raise Program_Error;

         when K_Float .. K_Long_Double =>
            raise Program_Error;

         when K_Char .. K_Wide_Char =>
            return L.CVal < R.CVal;

         when others =>
            return False;

      end case;
   end "<";

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Value_Type) return Value_Type
   is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case L.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
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

         when K_Boolean =>
            LV.IVal := LV.IVal and RV.IVal;

         when others =>
            return Bad_Value;
      end case;
      return LV;
   end "and";

   -----------
   -- "mod" --
   -----------

   function "mod" (L, R : Value_Type) return Value_Type
   is
      V : Value_Type := L;
   begin
      case L.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
            if L.Base /= R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal mod R.IVal;

         when others =>
            return Bad_Value;
      end case;
      return V;
   end "mod";

   -----------
   -- "not" --
   -----------

   function "not" (R : Value_Type) return Value_Type
   is
      V : Value_Type := R;
   begin
      case V.K is
         when K_Octet
           | K_Short
           | K_Unsigned_Short
           | K_Long
           | K_Unsigned_Long =>
            V.IVal := Unsigned_Long_Long (not Unsigned_Long (V.IVal));

         when K_Long_Long
           | K_Unsigned_Long_Long =>
            V.IVal := not V.IVal;

         when K_Boolean =>
            V.IVal := 1 - V.IVal;

         when others =>
            return Bad_Value;
      end case;
      return V;
   end "not";

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Value_Type) return Value_Type
   is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case L.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
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

         when K_Boolean =>
            LV.IVal := LV.IVal or RV.IVal;

         when others =>
            return Bad_Value;
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
      case LV.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
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

         when K_Boolean =>
            LV.IVal := LV.IVal xor RV.IVal;

         when others =>
            return Bad_Value;
      end case;
      return LV;
   end "xor";

   ----------------------------
   -- Add_ULL_To_Name_Buffer --
   ----------------------------

   procedure Add_ULL_To_Name_Buffer (U : ULL; B : ULL; S : Integer := 1)
   is
      Q : constant ULL := U / B;
      R : constant ULL := U mod B;
   begin
      if Q /= 0 or else S > 1 then
         Add_ULL_To_Name_Buffer (Q, B, S - 1);
      end if;
      Add_Char_To_Name_Buffer (Hex (Hex'First + Natural (R)));
   end Add_ULL_To_Name_Buffer;

   -------------
   -- Convert --
   -------------

   function Convert (V : Value_Type; K : Node_Kind) return Value_Type
   is
      R : Value_Type (K);
   begin
      case K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
            R.IVal := V.IVal;
            R.Sign := V.Sign;
            R.Base := V.Base;

         when K_Fixed_Point_Type =>
            R.IVal  := V.IVal;
            R.Sign  := V.Sign;
            R.Total := V.Total;
            R.Scale := V.Scale;

         when K_Float .. K_Long_Double =>
            R.FVal := V.FVal;

         when K_Char .. K_Wide_Char =>
            R.CVal := V.CVal;

         when K_String .. K_Wide_String =>
            R.SVal := V.SVal;

         when K_Boolean =>
            R.IVal := V.IVal;

         when others =>
            return V;
      end case;
      return R;
   end Convert;

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
      case V.K is
         when K_Boolean =>
            if V.IVal = 1 then
               return "TRUE";
            else
               return "FALSE";
            end if;

         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
            if V.Sign < 0 then
               Add_Char_To_Name_Buffer ('-');
            elsif V.Base = 16 then
               Add_Str_To_Name_Buffer ("0X");
            elsif V.Base = 8 then
               Add_Char_To_Name_Buffer ('0');
            end if;
            Add_ULL_To_Name_Buffer (V.IVal, ULL (V.Base));

         when K_Fixed_Point_Type =>
            if V.Sign < 0 then
               Add_Char_To_Name_Buffer ('-');
            end if;
            Add_ULL_To_Name_Buffer  (V.IVal / 10 ** Natural (V.Scale), 10);
            if V.Scale > 0 then
               Add_Char_To_Name_Buffer ('.');
               Add_ULL_To_Name_Buffer (V.IVal mod 10 ** Natural (V.Scale), 10);
            end if;
            Add_Char_To_Name_Buffer ('D');

         when K_Float .. K_Long_Double =>
            Add_Str_To_Name_Buffer (Long_Double'Image (V.FVal));
            declare
               Index : Natural := Name_Len;
            begin
               --  Find exponent if any

               while Index > 0 and then Name_Buffer (Index) /= 'E' loop
                  Index := Index - 1;
               end loop;

               --  Remove leading zero in exponent part.

               if Index > 0 then
                  Index := Index + 2;
                  while Index <= Name_Len
                    and then Name_Buffer (Index) = '0'
                  loop
                     Name_Buffer (Index .. Name_Len - 1) :=
                       Name_Buffer (Index + 1 .. Name_Len);
                     Name_Len := Name_Len - 1;
                  end loop;

                  --  Remove exponent

                  if Index > Name_Len then
                     Name_Len := Name_Len - 2;
                     Index := Name_Len;

                  else
                     Index := Name_Len;
                     while Name_Buffer (Index) /= 'E' loop
                        Index := Index - 1;
                     end loop;
                     Index := Index - 1;
                  end if;

               end if;

               --  Remove trailing zero in fraction part.

               while Name_Buffer (Index) = '0' loop
                  exit when Name_Buffer (Index - 1) = '.';
                  Name_Buffer (Index .. Name_Len - 1) :=
                    Name_Buffer (Index + 1 .. Name_Len);
                  Name_Len := Name_Len - 1;
                  Index    := Index - 1;
               end loop;
            end;

         when K_Char | K_Wide_Char =>
            if V.K = K_Wide_Char then
               Add_Char_To_Name_Buffer ('L');
            end if;
            Add_Char_To_Name_Buffer (''');
            if V.CVal <= 127 then
               declare
                  C : constant Character := Character'Val (Natural (V.CVal));
               begin
                  if C in '!' .. '~' then
                     Add_Char_To_Name_Buffer (C);
                  else
                     Add_Char_To_Name_Buffer ('\');
                     Add_ULL_To_Name_Buffer (ULL (V.CVal), 8, 3);
                  end if;
               end;
            else
               Add_Str_To_Name_Buffer ("\u");
               Add_ULL_To_Name_Buffer (ULL (V.CVal), 16);
            end if;
            Add_Char_To_Name_Buffer (''');

         when K_String | K_Wide_String | K_String_Type | K_Wide_String_Type =>
            if V.SVal = No_Name then
               return '"' & '"';
            end if;
            if V.K = K_Wide_String or else V.K = K_Wide_String_Type then
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

         when K_Enumerator =>
            Get_Name_String (V.SVal);

         when others =>
            raise Program_Error;
      end case;

      return Name_Buffer (1 .. Name_Len);
   end Image;

   ---------------
   -- Image_Ada --
   ---------------

   function Image_Ada (Value : Value_Id) return String
   is
      V : Value_Type;
   begin
      if Value = No_Value then
         return "<>";
      end if;
      V := VT.Table (Value);
      Name_Len := 0;
      case V.K is
         when K_Boolean =>
            if V.IVal = 1 then
               return "True";
            else
               return "False";
            end if;

         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
            if V.Sign < 0 then
               Add_Char_To_Name_Buffer ('-');
            elsif V.Base = 16 then
               Add_Str_To_Name_Buffer ("16#");
            elsif V.Base = 8 then
               Add_Str_To_Name_Buffer ("8#");
            end if;

            Add_ULL_To_Name_Buffer (V.IVal, ULL (V.Base));

            if V.Base = 16 or else V.Base = 8 then
               Add_Char_To_Name_Buffer ('#');
            end if;

         when K_Fixed_Point_Type =>
            if V.Sign < 0 then
               Add_Char_To_Name_Buffer ('-');
            end if;
            Add_ULL_To_Name_Buffer  (V.IVal / 10 ** Natural (V.Scale), 10);
            if V.Scale > 0 then
               Add_Char_To_Name_Buffer ('.');
               Add_ULL_To_Name_Buffer (V.IVal mod 10 ** Natural (V.Scale), 10);
            end if;

         when K_Float .. K_Long_Double =>
            Add_Str_To_Name_Buffer (Long_Double'Image (V.FVal));

            if Name_Buffer (Name_Buffer'First) = ' ' then
               Set_Str_To_Name_Buffer
                 (Name_Buffer (Name_Buffer'First .. Name_Len));
            end if;

            declare
               Index : Natural := Name_Len;

            begin

               --  Find exponent if any

               while Index > 0 and then Name_Buffer (Index) /= 'E' loop
                  Index := Index - 1;
               end loop;

               --  Remove leading zero in exponent part.

               if Index > 0 then
                  Index := Index + 2;
                  while Index <= Name_Len
                    and then Name_Buffer (Index) = '0'
                  loop
                     Name_Buffer (Index .. Name_Len - 1) :=
                       Name_Buffer (Index + 1 .. Name_Len);
                     Name_Len := Name_Len - 1;
                  end loop;

                  --  Remove exponent

                  if Index > Name_Len then
                     Name_Len := Name_Len - 2;
                     Index := Name_Len;

                  else
                     Index := Name_Len;
                     while Name_Buffer (Index) /= 'E' loop
                        Index := Index - 1;
                     end loop;
                     Index := Index - 1;
                  end if;

               end if;

               --  Remove trailing zero in fraction part.

               while Name_Buffer (Index) = '0' loop
                  exit when Name_Buffer (Index - 1) = '.';
                  Name_Buffer (Index .. Name_Len - 1) :=
                    Name_Buffer (Index + 1 .. Name_Len);
                  Name_Len := Name_Len - 1;
                  Index    := Index - 1;
               end loop;
            end;

         when K_Char | K_Wide_Char =>
            if V.CVal <= 127 then
               declare
                  C : constant Character := Character'Val (Natural (V.CVal));
               begin
                  if C in '!' .. '~' then
                     Add_Char_To_Name_Buffer (''');
                     Add_Char_To_Name_Buffer (C);
                     Add_Char_To_Name_Buffer (''');
                  else
                     if V.K = K_Wide_Char then
                        Add_Str_To_Name_Buffer ("Wide_");
                     end if;
                     Add_Str_To_Name_Buffer ("Character'Val (");
                     Add_ULL_To_Name_Buffer (ULL (V.CVal), 10);
                     Add_Char_To_Name_Buffer (')');
                  end if;
               end;
            else
               Add_Str_To_Name_Buffer ("Wide_Character'Val (");
               Add_ULL_To_Name_Buffer (ULL (V.CVal), 10);
               Add_Char_To_Name_Buffer (')');
            end if;

         when K_String | K_Wide_String | K_String_Type | K_Wide_String_Type =>
            if V.SVal = No_Name then
               return '"' & '"';
            end if;
            Add_Char_To_Name_Buffer ('"'); -- "
            Get_Name_String_And_Append (V.SVal);
            Add_Char_To_Name_Buffer ('"'); -- "

         when K_Enumerator =>
            Get_Name_String (V.SVal);

         when others =>
            raise Program_Error;
      end case;

      return Name_Buffer (1 .. Name_Len);
   end Image_Ada;

   --------------
   -- Negative --
   --------------

   function Negative (V : Value_Type) return Boolean is
   begin
      case V.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet | K_Fixed_Point_Type =>
            return V.Sign < 0;

         when K_Float .. K_Long_Double =>
            return V.FVal < 0.0;

         when others =>
            raise Program_Error;
      end case;
   end Negative;

   --------------
   -- Negative --
   --------------

   function Negative (V : Value_Id) return Boolean is
   begin
      if V = No_Value then
         raise Program_Error;
      end if;

      return Negative (Value (V));
   end Negative;

   -----------------------
   -- New_Boolean_Value --
   -----------------------

   function New_Boolean_Value
     (Value : Boolean)
     return Value_Id is
   begin
      return New_Value (Value_Type'(K_Boolean, Boolean'Pos (Value), 1, 10));
   end New_Boolean_Value;

   -------------------------
   -- New_Character_Value --
   -------------------------

   function New_Character_Value
     (Value : Unsigned_Short;
      Wide  : Boolean)
     return Value_Id is
   begin
      if Wide then
         return New_Value (Value_Type'(K_Wide_Char, Value));
      else
         return New_Value (Value_Type'(K_Char, Value));
      end if;
   end New_Character_Value;

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator
     (Img : Name_Id;
      Pos : Unsigned_Long_Long)
      return Value_Id is
   begin
      return New_Value (Value_Type'(K_Enumerator, Img, Pos));
   end New_Enumerator;

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
        (Value_Type'(K_Fixed_Point_Type, Value, Sign, Total, Scale));
      Normalize_Fixed_Point_Value (V, Total, Scale);
      return V;
   end New_Fixed_Point_Value;

   ------------------------------
   -- New_Floating_Point_Value --
   ------------------------------

   function New_Floating_Point_Value
     (Value : Long_Double)
     return Value_Id is
   begin
      return New_Value (Value_Type'(K_Long_Double, Value));
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
      return New_Value (Value_Type'(K_Unsigned_Long_Long, Value, Sign, Base));
   end New_Integer_Value;

   ----------------------
   -- New_String_Value --
   ----------------------

   function New_String_Value
     (Value : Name_Id;
      Wide  : Boolean)
     return Value_Id is
   begin
      if Wide then
         return New_Value (Value_Type'(K_Wide_String, Value));
      else
         return New_Value (Value_Type'(K_String, Value));
      end if;
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

   procedure Normalize_Fixed_Point_Value
     (Value : in out Value_Id;
      Total : Unsigned_Short_Short := Max_Digits;
      Scale : Unsigned_Short_Short := Max_Digits)
   is
      V : Value_Type := Values.Value (Value);
   begin
      Normalize_Fixed_Point_Value (V, Total, Scale);
      if V = Bad_Value then
         Value := No_Value;
      else
         Set_Value (Value, V);
      end if;
   end Normalize_Fixed_Point_Value;

   ---------------------------------
   -- Normalize_Fixed_Point_Value --
   ---------------------------------

   procedure Normalize_Fixed_Point_Value
     (Value : in out Value_Type;
      Total : Unsigned_Short_Short := Max_Digits;
      Scale : Unsigned_Short_Short := Max_Digits)
   is
      Quotient : Unsigned_Long_Long;
   begin
      --  Reduce the precision when it exceeds what is required

      while Value.Scale > Scale loop
         Value.IVal  := Value.IVal / 10;
         Value.Scale := Value.Scale - 1;
         Value.Total := Value.Total - 1;
      end loop;

      --  Remove any trailing zero

      while Value.Scale > 0 and then Value.IVal mod 10 = 0 loop
         Value.IVal  := Value.IVal / 10;
         Value.Scale := Value.Scale - 1;
         Value.Total := Value.Total - 1;
      end loop;

      --  Remove any leading zero and recompute total digits

      Quotient    := Value.IVal / 10;
      Value.Total := 1;
      while Quotient /= 0 loop
         Quotient := Quotient / 10;
         Value.Total := Value.Total + 1;
      end loop;

      --  Value overflows maximum value supported by fixed point type

      if Value.Total > Total then
         Value := Bad_Value;
      end if;
   end Normalize_Fixed_Point_Value;

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
      case RV.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
            if RV.Sign < 0 then
               RV.Sign := 1;
               return Shift_Right (LV, RV);
            end if;

            --  Keep working with left operand base

            LV.IVal := Shift_Left (LV.IVal, Natural (RV.IVal));
            return LV;

         when others =>
            return Bad_Value;
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
      case RV.K is
         when K_Short .. K_Unsigned_Long_Long | K_Octet =>
            if RV.Sign < 0 then
               RV.Sign := 1;
               return Shift_Left (LV, RV);
            end if;

            --  Keep working with left operand base

            LV.IVal := Shift_Right (LV.IVal, Natural (RV.IVal));
            return LV;

         when others =>
            return Bad_Value;
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
