with GNAT.OS_Lib;

with Locations; use Locations;
with Types;     use Types;

package Lexer is
pragma Elaborate_Body (Lexer);

   type Token_Type is
     (T_Error,
      T_Identifier,
      T_Pragma,

      --  About basic keywords

      T_Abstract,               --  First keyword
      T_Const,
      T_Exception,
      T_Interface,
      T_Module,
      T_Native,
      T_Typedef,
      T_Value_Type,

      --  About attributes

      T_Readonly,
      T_Attribute,

      --  About operations

      T_Oneway,
      T_Void,
      T_In,
      T_Inout,
      T_Out,
      T_Context,
      T_Raises,

      --  Basic types

      T_Float,
      T_Double,
      T_Unsigned,
      T_Short,
      T_Long,
      T_Char,
      T_Wchar,
      T_Boolean,
      T_Octet,
      T_Any,
      T_Object,
      T_Value_Base,

      --  Boolean values

      T_False,
      T_True,

      --  About constructed types

      T_Enum,
      T_Struct,
      T_Union,
      T_Case,
      T_Default,
      T_Switch,

      --  About template types

      T_Sequence,
      T_String,
      T_Wstring,
      T_Fixed,

      --  About value types

      T_Custom,
      T_Factory,
      T_Private,
      T_Public,
      T_Supports,
      T_Truncatable,            --  Last keyword

      --  Graphic characters

      T_Colon,
      T_Comma,
      T_Semi_Colon,
      T_Left_Brace,
      T_Left_Bracket,
      T_Left_Paren,
      T_Right_Brace,
      T_Right_Bracket,
      T_Right_Paren,
      T_Equal,
      T_Greater,
      T_Less,

      T_Tilde,                  --  First unary operator

      T_Minus,                  --  First binary operator
      T_Plus,                   --  Last unary operator
      T_Percent,
      T_Slash,
      T_Star,
      T_Ampersand,
      T_Bar,
      T_Circumflex,

      --  Double graphic characters

      T_Greater_Greater,
      T_Less_Less,              --  Last binary operator

      T_Colon_Colon,

      --  Literals

      T_Integer_Literal,
      T_Fixed_Point_Literal,
      T_Floating_Point_Literal,
      T_Boolean_Literal,
      T_Character_Literal,
      T_Wide_Character_Literal,
      T_String_Literal,
      T_Wide_String_Literal,

      T_EOF);

   First_Token_Pos : constant := Token_Type'Pos (Token_Type'First);
   Last_Token_Pos  : constant := Token_Type'Pos (Token_Type'Last);

   type Token_List_Type is array (Positive range <>) of Token_Type;

   subtype Keyword_Type is Token_Type
     range T_Abstract .. T_Truncatable;

   subtype Literal_Type is Token_Type
     range T_Integer_Literal .. T_Wide_String_Literal;

   subtype Operator_Type is Token_Type
     range T_Tilde .. T_Less_Less;

   subtype Unary_Operator_Type is Token_Type
     range T_Tilde .. T_Plus;

   subtype Binary_Operator_Type is Token_Type
     range T_Minus .. T_Less_Less;

   subtype Mode_Type is Token_Type
     range T_In .. T_Out;

   Token          : Token_Type;
   Token_Name     : Name_Id;
   Token_Location : Location;

   function Is_Literal (T : Token_Type) return Boolean;
   function Is_Operator (T : Token_Type) return Boolean;
   function Is_Scoped_Name (T : Token_Type) return Boolean;

   function Image (T : Token_Type) return String;
   --  Return an image of token T.

   procedure Scan_Token;
   --  Scan token and update global variables Token, Token_Name
   --  (for identifiers and literals) and Token_Location.

   procedure Scan_Token (T : Token_Type);
   --  Same as above. When the current token is not the expected token
   --  T, an error message is output and Token is set to T_Error. As a
   --  special case, when T_Semi_Colon is expected, we output an error
   --  location at the end of the line instead of the current token
   --  location.

   procedure Scan_Token (L : Token_List_Type);
   --  Same as above. When the current token is not in the list of the
   --  expected tokens L, an error message is output.

   function Next_Token return Token_Type;
   --  Return next token but do not update the lexer state that is
   --  Token, Token_Name and Token_Location.

   procedure Unexpected_Token (T : Token_Type; C : String := "");
   --  Output an error message to indicate that T is unexpected. If C
   --  is not a null string, the message also indicates in which
   --  construct it is not expected.

   procedure Restore_Lexer (State : Location);
   procedure Save_Lexer (State : out Location);

   procedure Skip_Declaration (Delimiter : Token_Type);
   --  Skip until we find a potential end of declaration. Delimiter
   --  indicates the kind of delimiters we are looking for (';', ',',
   --  ':') or ('{', '(', '[') or ('}', ')', ']'). We ensure that the
   --  declaration is well embraced.

   --  Various literal values updated when the corresponding token is read

   Boolean_Literal_Value : Boolean;
   Integer_Literal_Value : Long_Long_Integer;
   Integer_Literal_Base  : Integer;
   Float_Literal_Value   : Long_Long_Float;
   String_Literal_Value  : Name_Id;

   procedure Preprocess
     (Source : Types.Name_Id;
      Result : out GNAT.OS_Lib.File_Descriptor);
   --  Return a file descriptor of the preprocessed Source file

   procedure Output
     (Source : GNAT.OS_Lib.File_Descriptor);
   --  Output the preprocessed file Source

   procedure Process
     (Source_File : GNAT.OS_Lib.File_Descriptor;
      Source_Name : Types.Name_Id);
   --  Load file Source in the lexer

end Lexer;
