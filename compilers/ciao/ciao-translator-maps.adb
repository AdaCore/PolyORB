----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  Various mapping functions for CIAO.Translator.
--  $Id: //droopi/main/compilers/ciao/ciao-translator-maps.adb#2 $

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Wide_Text_Io;       use Ada.Wide_Text_Io;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Asis.Compilation_Units; use Asis.Compilation_Units;
with Asis.Declarations;      use Asis.Declarations;
with Asis.Elements;          use Asis.Elements;
with Asis.Expressions;       use Asis.Expressions;
with Asis.Text;              use Asis.Text;

with CIAO.ASIS_Queries;      use CIAO.ASIS_Queries;
--  with CIAO.IDL_Tree;          use CIAO.IDL_Tree;
--  with CIAO.IDL_Syntax.Scoped_Names;

with Errors;

package body CIAO.Translator.Maps is

   function Internal_IDL_Module_Name
     (Library_Unit_Name : String)
     return String;

   function IDL_Module_Name (Library_Unit : Compilation_Unit)
     return String is
      Full_Name : constant String :=
        To_String
        (Asis.Compilation_Units.Unit_Full_Name (Library_Unit));
   begin
      return Internal_IDL_Module_Name (Full_Name);
   end IDL_Module_Name;

   function Internal_IDL_Module_Name
     (Library_Unit_Name : String)
     return String
   is
      Total_Dot_Count : Natural := 0;
   begin
      for I in Library_Unit_Name'Range loop
         if Library_Unit_Name (I) = '.' then
            Total_Dot_Count := Total_Dot_Count + 1;
         end if;
      end loop;

      declare
         IDL_Name  : String (1 .. Library_Unit_Name'Length + Total_Dot_Count);
         Dot_Count : Natural := 0;
      begin
         for I in Library_Unit_Name'Range loop
            if Library_Unit_Name (I) /= '.' then
               IDL_Name
                 (IDL_Name'First + Dot_Count + (I - Library_Unit_Name'First))
                 := Library_Unit_Name (I);
            else
               IDL_Name
                 (IDL_Name'First + Dot_Count + (I - Library_Unit_Name'First) ..
                  IDL_Name'First + Dot_Count + (I - Library_Unit_Name'First) + 1)
                 := "__";
               Dot_Count := Dot_Count + 1;
            end if;
         end loop;

         return "DSA_" & IDL_Name;
      end;
   end Internal_IDL_Module_Name;

--    function Relative_Scoped_Name (Denoted_Definition : Definition;
--                                   Referer            : Declaration)
--      return Node_Id is

--       use CIAO.IDL_Syntax.Scoped_Names;

--       Context : constant Asis.Element := Enclosing_Element (Enclosing_Basic_Declaration (Referer));
--    begin
--       if False
--         and then Is_Identical
--         (Enclosing_Element
--          (Enclosing_Basic_Declaration (Denoted_Definition)),
--          Context)
--       then
--          pragma Assert (False);
--          --  XXX DISABLED FOR NOW
--          --  because With_Convert_For_Type expects that full scoped names
--          --  are generated for all param_type_specs.
--          --  The real fix for this problem would be to store node ids
--          --  in param type specs, and to make With_Convert_For_Type
--          --  work from the node actually defining the type, *not* from
--          --  the scoped name that denotes it.
--          --  2000 07 11 Thomas.

--          --  The basic_declarations containing the Referer and the
--          --  Denoted_Definition occur within the same scope:
--          --  use short name.
--          return CIAO.IDL_Syntax.New_Scoped_Name
--            (Isolated_Element_Image (Denoted_Definition));
--       else
--          declare
--             Enclosing_Declaration : Declaration
--               := Enclosing_Element (Enclosing_Element (Denoted_Definition));
--             Origin           : constant Compilation_Unit
--               := Enclosing_Compilation_Unit (Enclosing_Declaration);
--             Scoped_Name_Node : Node_Id;
--             Origin_Scoped_Name_Node : Node_Id;
--          begin
--             Scoped_Name_Node := New_Node (N_Scoped_Name);
--             Set_Name (Scoped_Name_Node,
--                       New_Name (Isolated_Element_Image (Denoted_Definition)));

--             --  Unwind all enclosing subunit declarations.
--             while not
--               (Is_Identical (Enclosing_Declaration, Unit_Declaration (Origin))
--                or else Is_Identical (Enclosing_Declaration, Context))
--             loop
--                declare
--                   N : constant Node_Id
--                     := New_Node (N_Scoped_Name);
--                   Defining_Names : constant Asis.Defining_Name_List
--                     := Names (Enclosing_Declaration);
--                begin
--                   pragma Assert (Defining_Names'Length = 1);

--                   Set_Name (N, New_Name (Isolated_Element_Image
--                                          (Defining_Names (Defining_Names'First))));
--                   Add_Prefix (Scoped_Name_Node, N);
--                end;
--                Enclosing_Declaration := Enclosing_Element (Enclosing_Declaration);
--             end loop;

--             if Is_Identical (Enclosing_Declaration, Unit_Declaration (Origin))
--               --  and then not Is_Identical (Enclosing_Declaration, Context)
--               --  XXX Condition disabled, see above.
--             then
--                Origin_Scoped_Name_Node := New_Node (N_Scoped_Name);
--                Set_Name (Origin_Scoped_Name_Node, New_Name (IDL_Module_Name (Origin)));

--                Add_Prefix (Scoped_Name_Node, Origin_Scoped_Name_Node);
--                Add_Absolute (Scoped_Name_Node);
--             end if;

--             Chain_Prefixes (Scoped_Name_Node);

--             return Scoped_Name_Node;
--          end;
--       end if;
--    end Relative_Scoped_Name;

   function Operator_Symbol_Identifier
     (Op : Asis.Defining_Name)
     return String is
   begin
      pragma Assert
        (Defining_Name_Kind (Op) = A_Defining_Operator_Symbol);

      case Operator_Kind (Op) is
         when  An_And_Operator =>
            return "Op_And";
         when  An_Or_Operator =>
            return "Op_Or";
         when  An_Xor_Operator =>
            return "Op_Xor";
         when  An_Equal_Operator =>
            return "Op_Equal";
         when  A_Not_Equal_Operator =>
            return "Op_Not_Equal";
         when  A_Less_Than_Operator =>
            return "Op_Less_Than";
         when  A_Less_Than_Or_Equal_Operator =>
            return "Op_Less_Than_Or_Equal";
         when  A_Greater_Than_Operator =>
            return "Op_Greater_Than";
         when  A_Greater_Than_Or_Equal_Operator =>
            return "Op_Greater_Than_Or_Equal";
         when A_Plus_Operator =>
            return "Op_Plus";
         when A_Minus_Operator =>
            return "Op_Minus";
         when A_Concatenate_Operator =>
            return "Op_Concatenate";
         when A_Unary_Plus_Operator =>
            return "Op_Unary_Plus";
         when A_Unary_Minus_Operator =>
            return "Op_Unary_Minus";
         when A_Multiply_Operator =>
            return "Op_Multiply";
         when A_Divide_Operator =>
            return "Op_Divide";
         when A_Mod_Operator =>
            return "Op_Mod";
         when A_Rem_Operator =>
            return "Op_Rem";
         when An_Exponentiate_Operator =>
            return "Op_Exponentiate";
         when An_Abs_Operator =>
            return "Op_Abs";
         when A_Not_Operator =>
            return "Op_Not";
         when others =>
            -- XXX Error
            raise Program_Error;
            return "";
      end case;
   end Operator_Symbol_Identifier;

   function Character_Literal_Identifier
     (Ch : Program_Text)
     return String
   is
      Wide_Ch : Wide_Character;
   begin
      pragma Assert (True
                     and then Ch'Length = 3
        and then Ch (Ch'First) = '''
        and then Ch (Ch'Last)  = ''');

      Wide_Ch := Ch (Ch'First + 1);

      case To_Character (Wide_Ch) is
         when NUL =>
            return "Ch_NUL";
         when SOH =>
            return "Ch_SOH";
         when STX =>
            return "Ch_STX";
         when ETX =>
            return "Ch_ETX";
         when EOT =>
            return "Ch_EOT";
         when ENQ =>
            return "Ch_ENQ";
         when ACK =>
            return "Ch_ACK";
         when BEL =>
            return "Ch_BEL";
         when BS =>
            return "Ch_BS";
         when HT =>
            return "Ch_HT";
         when LF =>
            return "Ch_LF";
         when VT =>
            return "Ch_VT";
         when FF =>
            return "Ch_FF";
         when CR =>
            return "Ch_CR";
         when SO =>
            return "Ch_SO";
         when SI =>
            return "Ch_SI";

         when DLE =>
            return "Ch_DLE";
         when DC1 =>
            return "Ch_DC1";
         when DC2 =>
            return "Ch_DC2";
         when DC3 =>
            return "Ch_DC3";
         when DC4 =>
            return "Ch_DC4";
         when NAK =>
            return "Ch_NAK";
         when SYN =>
            return "Ch_SYN";
         when ETB =>
            return "Ch_ETB";
         when CAN =>
            return "Ch_CAN";
         when EM =>
            return "Ch_EM";
         when SUB =>
            return "Ch_SUB";
         when ESC =>
            return "Ch_ESC";
         when FS =>
            return "Ch_FS";
         when GS =>
            return "Ch_GS";
         when RS =>
            return "Ch_RS";
         when US =>
            return "Ch_US";

            --------------------------------
            -- ISO 646 Graphic Characters --
            --------------------------------

         when Space =>
            return "Ch_Space";
         when Exclamation =>
            return "Ch_Exclamation";
         when Quotation =>
            return "Ch_Quotation";
         when Number_Sign =>
            return "Ch_Number_Sign";
         when Dollar_Sign =>
            return "Ch_Dollar_Sign";
         when Percent_Sign =>
            return "Ch_Percent_Sign";
         when Ampersand =>
            return "Ch_Ampersand";
         when Apostrophe =>
            return "Ch_Apostrophe";
         when Left_Parenthesis =>
            return "Ch_Left_Parenthesis";
         when Right_Parenthesis =>
            return "Ch_Right_Parenthesis";
         when Asterisk =>
            return "Ch_Asterisk";
         when Plus_Sign =>
            return "Ch_Plus_Sign";
         when Comma =>
            return "Ch_Comma";
         when Hyphen =>
            return "Ch_Hyphen";
         when Full_Stop =>
            return "Ch_Full_Stop";
         when Solidus =>
            return "Ch_Solidus";

            --  Decimal digits '0' though '9' are at positions 48 through 57

         when '0' =>
            return "Ch_0";
         when '1' =>
            return "Ch_1";
         when '2' =>
            return "Ch_2";
         when '3' =>
            return "Ch_3";
         when '4' =>
            return "Ch_4";
         when '5' =>
            return "Ch_5";
         when '6' =>
            return "Ch_6";
         when '7' =>
            return "Ch_7";
         when '8' =>
            return "Ch_8";
         when '9' =>
            return "Ch_9";

         when Colon =>
            return "Ch_Colon";
         when Semicolon =>
            return "Ch_Semicolon";
         when Less_Than_Sign =>
            return "Ch_Less_Than_Sign";
         when Equals_Sign =>
            return "Ch_Equals_Sign";
         when Greater_Than_Sign =>
            return "Ch_Greater_Than_Sign";
         when Question =>
            return "Ch_Question";

         when Commercial_At =>
            return "Ch_Commercial_At";

            --  Letters 'A' through 'Z' are at positions 65 through 90

         when 'A' =>
            return "Ch_UC_A";
         when 'B' =>
            return "Ch_UC_B";
         when 'C' =>
            return "Ch_UC_C";
         when 'D' =>
            return "Ch_UC_D";
         when 'E' =>
            return "Ch_UC_E";
         when 'F' =>
            return "Ch_UC_F";
         when 'G' =>
            return "Ch_UC_G";
         when 'H' =>
            return "Ch_UC_H";
         when 'I' =>
            return "Ch_UC_I";
         when 'J' =>
            return "Ch_UC_J";
         when 'K' =>
            return "Ch_UC_K";
         when 'L' =>
            return "Ch_UC_L";
         when 'M' =>
            return "Ch_UC_M";
         when 'N' =>
            return "Ch_UC_N";
         when 'O' =>
            return "Ch_UC_O";
         when 'P' =>
            return "Ch_UC_P";
         when 'Q' =>
            return "Ch_UC_Q";
         when 'R' =>
            return "Ch_UC_R";
         when 'S' =>
            return "Ch_UC_S";
         when 'T' =>
            return "Ch_UC_T";
         when 'U' =>
            return "Ch_UC_U";
         when 'V' =>
            return "Ch_UC_V";
         when 'W' =>
            return "Ch_UC_W";
         when 'X' =>
            return "Ch_UC_X";
         when 'Y' =>
            return "Ch_UC_Y";
         when 'Z' =>
            return "Ch_UC_Z";

         when Left_Square_Bracket =>
            return "Ch_Left_Square_Bracket";
         when Reverse_Solidus =>
            return "Ch_Reverse_Solidus";
         when Right_Square_Bracket =>
            return "Ch_Right_Square_Bracket";
         when Circumflex =>
            return "Ch_Circumflex";
         when Low_Line =>
            return "Ch_Low_Line";

         when Grave =>
            return "Ch_Grave";
         when LC_A =>
            return "Ch_LC_A";
         when LC_B =>
            return "Ch_LC_B";
         when LC_C =>
            return "Ch_LC_C";
         when LC_D =>
            return "Ch_LC_D";
         when LC_E =>
            return "Ch_LC_E";
         when LC_F =>
            return "Ch_LC_F";
         when LC_G =>
            return "Ch_LC_G";
         when LC_H =>
            return "Ch_LC_H";
         when LC_I =>
            return "Ch_LC_I";
         when LC_J =>
            return "Ch_LC_J";
         when LC_K =>
            return "Ch_LC_K";
         when LC_L =>
            return "Ch_LC_L";
         when LC_M =>
            return "Ch_LC_M";
         when LC_N =>
            return "Ch_LC_N";
         when LC_O =>
            return "Ch_LC_O";
         when LC_P =>
            return "Ch_LC_P";
         when LC_Q =>
            return "Ch_LC_Q";
         when LC_R =>
            return "Ch_LC_R";
         when LC_S =>
            return "Ch_LC_S";
         when LC_T =>
            return "Ch_LC_T";
         when LC_U =>
            return "Ch_LC_U";
         when LC_V =>
            return "Ch_LC_V";
         when LC_W =>
            return "Ch_LC_W";
         when LC_X =>
            return "Ch_LC_X";
         when LC_Y =>
            return "Ch_LC_Y";
         when LC_Z =>
            return "Ch_LC_Z";
         when Left_Curly_Bracket =>
            return "Ch_Left_Curly_Bracket";
         when Vertical_Line =>
            return "Ch_Vertical_Line";
         when Right_Curly_Bracket =>
            return "Ch_Right_Curly_Bracket";
         when Tilde =>
            return "Ch_Tilde";
         when DEL =>
            return "Ch_DEL";

            ---------------------------------
            -- ISO 6429 Control Characters --
            ---------------------------------

         when Reserved_128 =>
            return "Ch_Reserved_128";
         when Reserved_129 =>
            return "Ch_Reserved_129";
         when BPH =>
            return "Ch_BPH";
         when NBH =>
            return "Ch_NBH";
         when Reserved_132 =>
            return "Ch_Reserved_132";
         when NEL =>
            return "Ch_NEL";
         when SSA =>
            return "Ch_SSA";
         when ESA =>
            return "Ch_ESA";
         when HTS =>
            return "Ch_HTS";
         when HTJ =>
            return "Ch_HTJ";
         when VTS =>
            return "Ch_VTS";
         when PLD =>
            return "Ch_PLD";
         when PLU =>
            return "Ch_PLU";
         when RI =>
            return "Ch_RI";
         when SS2 =>
            return "Ch_SS2";
         when SS3 =>
            return "Ch_SS3";

         when DCS =>
            return "Ch_DCS";
         when PU1 =>
            return "Ch_PU1";
         when PU2 =>
            return "Ch_PU2";
         when STS =>
            return "Ch_STS";
         when CCH =>
            return "Ch_CCH";
         when MW =>
            return "Ch_MW";
         when SPA =>
            return "Ch_SPA";
         when EPA =>
            return "Ch_EPA";

         when SOS =>
            return "Ch_SOS";
         when Reserved_153 =>
            return "Ch_Reserved_153";
         when SCI =>
            return "Ch_SCI";
         when CSI =>
            return "Ch_CSI";
         when ST =>
            return "Ch_ST";
         when OSC =>
            return "Ch_OSC";
         when PM =>
            return "Ch_PM";
         when APC =>
            return "Ch_APC";

            ------------------------------
            -- Other Graphic Characters --
            ------------------------------

            --  Character positions 160 (16#A0#) .. 175 (16#AF#)

         when No_Break_Space =>
            return "Ch_No_Break_Space";
         when Inverted_Exclamation =>
            return "Ch_Inverted_Exclamation";
         when Cent_Sign =>
            return "Ch_Cent_Sign";
         when Pound_Sign =>
            return "Ch_Pound_Sign";
         when Currency_Sign =>
            return "Ch_Currency_Sign";
         when Yen_Sign =>
            return "Ch_Yen_Sign";
         when Broken_Bar =>
            return "Ch_Broken_Bar";
         when Section_Sign =>
            return "Ch_Section_Sign";
         when Diaeresis =>
            return "Ch_Diaeresis";
         when Copyright_Sign =>
            return "Ch_Copyright_Sign";
         when Feminine_Ordinal_Indicator =>
            return "Ch_Feminine_Ordinal_Indicator";
         when Left_Angle_Quotation =>
            return "Ch_Left_Angle_Quotation";
         when Not_Sign =>
            return "Ch_Not_Sign";
         when Soft_Hyphen =>
            return "Ch_Soft_Hyphen";
         when Registered_Trade_Mark_Sign =>
            return "Ch_Registered_Trade_Mark_Sign";
         when Macron =>
            return "Ch_Macron";

            --  Character positions 176 (16#B0#) .. 191 (16#BF#)

         when Degree_Sign =>
            return "Ch_Degree_Sign";
         when Plus_Minus_Sign =>
            return "Ch_Plus_Minus_Sign";
         when Superscript_Two =>
            return "Ch_Superscript_Two";
         when Superscript_Three =>
            return "Ch_Superscript_Three";
         when Acute =>
            return "Ch_Acute";
         when Micro_Sign =>
            return "Ch_Micro_Sign";
         when Pilcrow_Sign =>
            return "Ch_Pilcrow_Sign";
         when Middle_Dot =>
            return "Ch_Middle_Dot";
         when Cedilla =>
            return "Ch_Cedilla";
         when Superscript_One =>
            return "Ch_Superscript_One";
         when Masculine_Ordinal_Indicator =>
            return "Ch_Masculine_Ordinal_Indicator";
         when Right_Angle_Quotation =>
            return "Ch_Right_Angle_Quotation";
         when Fraction_One_Quarter =>
            return "Ch_Fraction_One_Quarter";
         when Fraction_One_Half =>
            return "Ch_Fraction_One_Half";
         when Fraction_Three_Quarters =>
            return "Ch_Fraction_Three_Quarters";
         when Inverted_Question =>
            return "Ch_Inverted_Question";

            --  Character positions 192 (16#C0#) .. 207 (16#CF#)

         when UC_A_Grave =>
            return "Ch_UC_A_Grave";
         when UC_A_Acute =>
            return "Ch_UC_A_Acute";
         when UC_A_Circumflex =>
            return "Ch_UC_A_Circumflex";
         when UC_A_Tilde =>
            return "Ch_UC_A_Tilde";
         when UC_A_Diaeresis =>
            return "Ch_UC_A_Diaeresis";
         when UC_A_Ring =>
            return "Ch_UC_A_Ring";
         when UC_AE_Diphthong =>
            return "Ch_UC_AE_Diphthong";
         when UC_C_Cedilla =>
            return "Ch_UC_C_Cedilla";
         when UC_E_Grave =>
            return "Ch_UC_E_Grave";
         when UC_E_Acute =>
            return "Ch_UC_E_Acute";
         when UC_E_Circumflex =>
            return "Ch_UC_E_Circumflex";
         when UC_E_Diaeresis =>
            return "Ch_UC_E_Diaeresis";
         when UC_I_Grave =>
            return "Ch_UC_I_Grave";
         when UC_I_Acute =>
            return "Ch_UC_I_Acute";
         when UC_I_Circumflex =>
            return "Ch_UC_I_Circumflex";
         when UC_I_Diaeresis =>
            return "Ch_UC_I_Diaeresis";

            --  Character positions 208 (16#D0#) .. 223 (16#DF#)

         when UC_Icelandic_Eth =>
            return "Ch_UC_Icelandic_Eth";
         when UC_N_Tilde =>
            return "Ch_UC_N_Tilde";
         when UC_O_Grave =>
            return "Ch_UC_O_Grave";
         when UC_O_Acute =>
            return "Ch_UC_O_Acute";
         when UC_O_Circumflex =>
            return "Ch_UC_O_Circumflex";
         when UC_O_Tilde =>
            return "Ch_UC_O_Tilde";
         when UC_O_Diaeresis =>
            return "Ch_UC_O_Diaeresis";
         when Multiplication_Sign =>
            return "Ch_Multiplication_Sign";
         when UC_O_Oblique_Stroke =>
            return "Ch_UC_O_Oblique_Stroke";
         when UC_U_Grave =>
            return "Ch_UC_U_Grave";
         when UC_U_Acute =>
            return "Ch_UC_U_Acute";
         when UC_U_Circumflex =>
            return "Ch_UC_U_Circumflex";
         when UC_U_Diaeresis =>
            return "Ch_UC_U_Diaeresis";
         when UC_Y_Acute =>
            return "Ch_UC_Y_Acute";
         when UC_Icelandic_Thorn =>
            return "Ch_UC_Icelandic_Thorn";
         when LC_German_Sharp_S =>
            return "Ch_LC_German_Sharp_S";

            --  Character positions 224 (16#E0#) .. 239 (16#EF#)

         when LC_A_Grave =>
            return "Ch_LC_A_Grave";
         when LC_A_Acute =>
            return "Ch_LC_A_Acute";
         when LC_A_Circumflex =>
            return "Ch_LC_A_Circumflex";
         when LC_A_Tilde =>
            return "Ch_LC_A_Tilde";
         when LC_A_Diaeresis =>
            return "Ch_LC_A_Diaeresis";
         when LC_A_Ring =>
            return "Ch_LC_A_Ring";
         when LC_AE_Diphthong =>
            return "Ch_LC_AE_Diphthong";
         when LC_C_Cedilla =>
            return "Ch_LC_C_Cedilla";
         when LC_E_Grave =>
            return "Ch_LC_E_Grave";
         when LC_E_Acute =>
            return "Ch_LC_E_Acute";
         when LC_E_Circumflex =>
            return "Ch_LC_E_Circumflex";
         when LC_E_Diaeresis =>
            return "Ch_LC_E_Diaeresis";
         when LC_I_Grave =>
            return "Ch_LC_I_Grave";
         when LC_I_Acute =>
            return "Ch_LC_I_Acute";
         when LC_I_Circumflex =>
            return "Ch_LC_I_Circumflex";
         when LC_I_Diaeresis =>
            return "Ch_LC_I_Diaeresis";

            --  Character positions 240 (16#F0#) .. 255 (16#FF)

         when LC_Icelandic_Eth =>
            return "Ch_LC_Icelandic_Eth";
         when LC_N_Tilde =>
            return "Ch_LC_N_Tilde";
         when LC_O_Grave =>
            return "Ch_LC_O_Grave";
         when LC_O_Acute =>
            return "Ch_LC_O_Acute";
         when LC_O_Circumflex =>
            return "Ch_LC_O_Circumflex";
         when LC_O_Tilde =>
            return "Ch_LC_O_Tilde";
         when LC_O_Diaeresis =>
            return "Ch_LC_O_Diaeresis";
         when Division_Sign =>
            return "Ch_Division_Sign";
         when LC_O_Oblique_Stroke =>
            return "Ch_LC_O_Oblique_Stroke";
         when LC_U_Grave =>
            return "Ch_LC_U_Grave";
         when LC_U_Acute =>
            return "Ch_LC_U_Acute";
         when LC_U_Circumflex =>
            return "Ch_LC_U_Circumflex";
         when LC_U_Diaeresis =>
            return "Ch_LC_U_Diaeresis";
         when LC_Y_Acute =>
            return "Ch_LC_Y_Acute";
         when LC_Icelandic_Thorn =>
            return "Ch_LC_Icelandic_Thorn";
         when LC_Y_Diaeresis =>
            return "Ch_LC_Y_Diaeresis";
      end case;
   end Character_Literal_Identifier;

   function Base_Type (T : Root_Type) return Node_Id is
      use Errors;
   begin
      case T is
         when Root_Integer =>
            return Make_Long (No_Location);
         when Root_Modular =>
            return Make_Unsigned_Long (No_Location);
         when Root_Real =>
            return Make_Double (No_Location);
         --  Change these to long long, unsigned long long and long double
         --  if the CORBA software supports it.

         when Root_Boolean =>
            return Make_Boolean (No_Location);
         when Root_Char =>
            return Make_Char (No_Location);
         when Root_String =>
            return Make_String (No_Location);
      end case;
   end Base_Type;

end CIAO.Translator.Maps;
