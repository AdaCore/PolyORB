package Tokens is
pragma Pure;

   T_Error                  : constant := 0;
   T_Identifier             : constant := 1;
   T_Pragma                 : constant := 2;

   --  About basic keywords

   T_Abstract               : constant := 4;
   T_Const                  : constant := 5;
   T_Exception              : constant := 6;
   T_Interface              : constant := 7;
   T_Module                 : constant := 8;
   T_Native                 : constant := 9;
   T_Typedef                : constant := 10;
   T_ValueType              : constant := 11;

   --  About attributes

   T_Readonly               : constant := 13;
   T_Attribute              : constant := 14;

   --  About operations

   T_Oneway                 : constant := 16;
   T_Void                   : constant := 17;
   T_In                     : constant := 18;
   T_Inout                  : constant := 19;
   T_Out                    : constant := 20;
   T_Context                : constant := 21;
   T_Raises                 : constant := 22;

   --  Basic types

   T_Float                  : constant := 24;
   T_Double                 : constant := 25;
   T_Unsigned               : constant := 26;
   T_Short                  : constant := 27;
   T_Long                   : constant := 28;
   T_Char                   : constant := 29;
   T_Wchar                  : constant := 30;
   T_Boolean                : constant := 31;
   T_Octet                  : constant := 32;
   T_Any                    : constant := 33;
   T_Object                 : constant := 34;
   T_Value_Base              : constant := 35;

   --  Boolean values

   T_False                  : constant := 37;
   T_True                   : constant := 38;

   --  About constructed types

   T_Enum                   : constant := 40;
   T_Struct                 : constant := 41;
   T_Union                  : constant := 42;
   T_Case                   : constant := 43;
   T_Default                : constant := 44;
   T_Switch                 : constant := 45;

   --  About template types

   T_Sequence               : constant := 47;
   T_String                 : constant := 48;
   T_Wstring                : constant := 49;
   T_Fixed                  : constant := 50;

   --  About value types

   T_Custom                 : constant := 52;
   T_Factory                : constant := 53;
   T_Private                : constant := 54;
   T_Public                 : constant := 55;
   T_Supports               : constant := 56;
   T_Truncatable            : constant := 57;

   --  Graphic characters

   T_Ampersand              : constant := 59;
   T_Bar                    : constant := 60;
   T_Circumflex             : constant := 61;
   T_Colon                  : constant := 62;
   T_Colon_Colon            : constant := 63;
   T_Comma                  : constant := 64;
   T_Equal                  : constant := 65;
   T_Greater                : constant := 66;
   T_Left_Brace             : constant := 67;
   T_Left_Bracket           : constant := 68;
   T_Left_Paren             : constant := 69;
   T_Less                   : constant := 70;
   T_Minus                  : constant := 71;
   T_Percent                : constant := 72;
   T_Plus                   : constant := 73;
   T_Right_Brace            : constant := 74;
   T_Right_Bracket          : constant := 75;
   T_Right_Paren            : constant := 76;
   T_Semi_Colon             : constant := 77;
   T_Slash                  : constant := 78;
   T_Star                   : constant := 79;
   T_Tilde                  : constant := 80;

   --  Double graphic characters

   T_Greater_Greater        : constant := 82;
   T_Less_Less              : constant := 83;

   --  Literals

   T_Integer_Literal        : constant := 85;
   T_Fixed_Point_Literal    : constant := 86;
   T_Floating_Point_Literal : constant := 87;
   T_Character_Literal      : constant := 88;
   T_Wide_Character_Literal : constant := 89;
   T_String_Literal         : constant := 90;
   T_Wide_String_Literal    : constant := 91;
   T_EOF                    : constant := 92;

   type Token_Type is range T_Error .. T_EOF;
   type Token_List_Type is array (Positive range <>) of Token_Type;

end Tokens;
