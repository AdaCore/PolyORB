--  This package contains the lexical analyzer routines. This is used by the
--  parser for scanning AADL source files.

with GNAT.OS_Lib;

with Types;         use Types;
with Locations;     use Locations;

package Lexer is
   pragma Elaborate_Body (Lexer);

   type Token_Type is
     (T_Error,
      T_Identifier,

      --  Special_Characters

      T_Quotation_Mark,        --  "    (NOT USED, only in T_String_Literal)
      T_Number_Sign,           --  #
      T_Ampersand,             --  &
      T_Vertical_Line,         --  |
      T_Underline,             --  _

      T_Apostrophe,            --  '         First Delimiter
      T_Left_Parenthesis,      --  (
      T_Right_Parenthesis,     --  )
      T_Comma,                 --  ,

      T_Plus,                  --  +         First Binary Operator
      T_Minus,                 --  -
      T_Multiply,              --  *
      T_Divide,                --  /         Last Binary Operator

      T_Dot,                   --  .
      T_Colon,                 --  :
      T_Semicolon,             --  ;
      T_Less_Than_Sign,        --  <
      T_Equals_Sign,           --  =
      T_Greater_Than_Sign,     --  >
      T_Left_Square_Bracket,   --  [
      T_Right_Square_Bracket,  --  ]
      T_Left_Curly_Bracket,    --  {
      T_Right_Curly_Bracket,   --  }

      --  Compound Delimiters

      T_Association,           --  =>
      T_Additive_Association,  --  +=>
      T_Connection,            --  ->
      T_Immediate_Connection,  --  ->>
      T_Interval,              --  ..
      T_Colon_Colon,           --  ::
      T_Left_Step_Bracket,     --  -[
      T_Right_Step_Bracket,    --  ]->
      T_Begin_Annex,           --  {**
      T_End_Annex,             --  **}       Last Delimiter

      --  Reserved Words

      T_Access,                --  'access'
      T_Annex,                 --  'annex'
      T_Applies,               --  'applies'
      T_Binding,               --  'binding'
      T_Boolean,               --  'boolean'
      T_Bus,                   --  'bus'
      T_Case,                  --  'case'
      T_Classifier,            --  'classifier'
      T_Client,                --  'client'
      T_Component,             --  'component'
      T_Connections,           --  'connections'
      T_Constant,              --  'constant'
      T_Data,                  --  'data'
      T_Delta,                 --  'delta'
      T_Device,                --  'device'
      T_End,                   --  'end'
      T_Enumeration,           --  'enumeration'
      T_Event,                 --  'event'
      T_Extends,               --  'extends'

      T_False,                 --  'false'         Boolean Value
      T_True,                  --  'true'          Boolean Value

      T_Group,                 --  'group'
      T_Implementation,        --  'implementaion'
      T_In,                    --  'in'
      T_Infinity,              --  'infinity'
      T_Inherit,               --  'inherit'
      T_Initial,               --  'initial'
      T_Integer,               --  'integer'
      T_Inverse,               --  'inverse'
      T_Is,                    --  'is'
      T_List,                  --  'list'
      T_Memory,                --  'memory'
      T_Mode,                  --  'mode'
      T_Modes,                 --  'modes'
      T_None,                  --  'none'
      T_Not,                   --  'not'
      T_Of,                    --  'of'
      T_Or,                    --  'or'
      T_Orless,                --  'orless'
      T_Ormore,                --  'ormore'
      T_Others,                --  'others'
      T_Out,                   --  'out'
      T_Package,               --  'package'
      T_Port,                  --  'port'
      T_Private,               --  'private'
      T_Process,               --  'process'
      T_Processor,             --  'processor'
      T_Properties,            --  'properties'
      T_Property,              --  'property'
      T_Provides,              --  'provides'
      T_Public,                --  'public'
      T_Range,                 --  'range'
      T_Real,                  --  'real'
      T_Refined,               --  'refined'
      T_Refines,               --  'refines'
      T_Requires,              --  'requires'
      T_Server,                --  'server'
      T_Set,                   --  'set'
      T_String,                --  'string'
      T_Subcomponents,         --  'subcomponents'
      T_Subprogram,            --  'subprogram'
      T_System,                --  'system'
      T_Thread,                --  'thread'
      T_To,                    --  'to'
      T_Type,                  --  'type'
      T_Units,                 --  'units'

      --  Numeric Literals

      T_Real_Literal,          --  real number
      T_Integer_Literal,       --  integer number

      --  Others

      T_String_Literal,        --  string
      T_Comment,               --  comment      (NOT USED, ignored)

      T_EOF                    --  end of file
     );

   First_Token_Pos   : constant := Token_Type'Pos (Token_Type'First);
   Last_Token_Pos    : constant := Token_Type'Pos (Token_Type'Last);

   --  Synonyms

   T_Asterisk  : Token_Type renames T_Multiply;
   T_Full_Stop : Token_Type renames T_Dot;
   T_Hyphen    : Token_Type renames T_Minus;
   T_Point     : Token_Type renames T_Dot;
   T_Tick      : Token_Type renames T_Apostrophe;

   --  Sybtype definitions

   subtype Binary_Operator_Type is Token_Type
     range T_Plus .. T_Divide;

   subtype Boolean_Type is Token_Type
     range T_False .. T_True;

   subtype Compound_Delimiter_Type is Token_Type
     range T_Association .. T_End_Annex;

   subtype Delimiter_Type is Token_Type
     range T_Apostrophe .. T_End_Annex;

   subtype Numeric_Type is Token_Type
     range T_Real_Literal .. T_Integer_Literal;

   subtype Reserved_Word_Type is Token_Type
     range T_Access .. T_Units;

   First_Reserved_Word_Pos :
     constant := Reserved_Word_Type'Pos (Reserved_Word_Type'First);
   Last_Reserved_Word_Pos  :
     constant := Reserved_Word_Type'Pos (Reserved_Word_Type'Last);

   subtype Special_Character_Type is Token_Type
     range T_Quotation_Mark .. T_Right_Curly_Bracket;

   Max_Number_Of_Digits : constant Integer := 18;
   --  Number of digits of the biggest allowed integer, 2**64 have 20 digits

   Token          : Token_Type;
   Token_Location : Location;

   --  Various literal values updated when the corresponding token is read

   Token_Name            : Name_Id;   --  for Tokens : T_identifier
   String_Literal_Value  : Name_Id;   --  for Tokens : T_String

   Integer_Literal_Value : Unsigned_Long_Long;
   --  for Tokens : T_Integer_Literal

   Float_Literal_Value   : Long_Long_Float;
   --  for Tokens : T_Real_Literal

   Numeric_Literal_Base  : Unsigned_Short_Short;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   function Image (T : Token_Type) return String;
   --  Return an image of token T

   function Image_Current_Token return String;
   --  Return an image of the current token

   procedure Preprocess
     (Source : Name_Id;
      Result : out GNAT.OS_Lib.File_Descriptor);
   --  Return a file descriptor of the preprocessed Source file

   procedure Process
     (Source_File : GNAT.OS_Lib.File_Descriptor;
      Source_Name : Name_Id);
   --  Load file Source in the lexer

   procedure Restore_Lexer (State : Location);
   pragma Inline (Restore_Lexer);
   procedure Save_Lexer (State : out Location);
   pragma Inline (Save_Lexer);

   procedure Scan_Token;
   --  Scan token and update global variables Token, Token_Name
   --  (for identifiers and literals) and Token_Location.

   procedure Scan_Token (T : Token_Type);
   --  Same as above. When the current token is not the expected token
   --  T, an error message is output and Token is set to T_Error. As a
   --  special case, when T_Semicolon is expected, we output an error
   --  location at the end of the line instead of the current token
   --  location.

   function End_Of_File return Boolean;
   --  Return TRUE if there is no more useful data in file or EOF is reached

end Lexer;
