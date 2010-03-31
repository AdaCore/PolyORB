------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                L E X E R                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Locations;   use Locations;
with Types;       use Types;

package Lexer is
pragma Elaborate_Body (Lexer);

   type Token_Type is
     (T_Error,
      T_Identifier,

      --  About pragmas

      T_Pragma,
      T_Pragma_Id,
      T_Pragma_Prefix,
      T_Pragma_Version,
      T_Pragma_Unrecognized,

      --  About basic keywords

      T_Abstract,               --  First keyword
      T_Component,
      T_Const,
      T_Consumes,
      T_Emits,
      T_Eventtype,
      T_Exception,
      T_Finder,
      T_Home,
      T_Import,
      T_Interface,
      T_Local,
      T_Module,
      T_Multiple,
      T_Native,
      T_Primary_Key,
      T_Provides,
      T_Publishes,
      T_Typedef,
      T_Type_Id,
      T_Type_Prefix,
      T_Uses,
      T_Value_Type,

      --  About attributes

      T_Readonly,
      T_Attribute,
      T_Get_Raises,
      T_Set_Raises,

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
      T_String_Literal,
      T_Wide_String_Literal,
      T_Character_Literal,
      T_Wide_Character_Literal,
      T_Fixed_Point_Literal,
      T_Floating_Point_Literal,
      T_Boolean_Literal,

      T_EOF);

   First_Token_Pos : constant := Token_Type'Pos (Token_Type'First);
   Last_Token_Pos  : constant := Token_Type'Pos (Token_Type'Last);

   type Token_List_Type is array (Positive range <>) of Token_Type;

   subtype Keyword_Type is Token_Type
     range T_Abstract .. T_Truncatable;

   subtype Literal_Type is Token_Type
     range T_Integer_Literal .. T_Boolean_Literal;

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

   procedure Finalize_Imported;
   --  Pops the lexer state (when the state stack is not empty)

   function Handled (File_Name_Id : Name_Id) return Boolean;
   --  Indicate whether the file was parsed or not in order to avoid cyclic
   --  imports

   procedure Set_Handled (File_Name_Id : Name_Id);
   --  Marks the file as handled

   procedure Make_Cleanup;
   --  Cleanup temporary files when needed

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

   procedure Unexpected_Token (T : Token_Type; C : String);
   --  Output an error message to indicate that T is unexpected. If C
   --  is not a null string, the message also indicates in which
   --  construct it is not expected.

   procedure Save_Lexer (State : out Location);
   --  Saves the current location in the State variable

   procedure Restore_Lexer (State : Location);
   --  Modifies the current location in the IDL specification

   procedure Push_Lexer_State;
   --  Pushes the current location in a state stack and deallocates the data
   --  concerning the handled IDL specification

   procedure Pop_Lexer_State;
   --  Pops a location from the state stack and loads the corresponding file

   procedure Skip_Declaration (Delimiter : Token_Type);
   --  Skip until we find a potential end of declaration. Delimiter
   --  indicates the kind of delimiters we are looking for (';', ',',
   --  ':') or ('{', '(', '[') or ('}', ')', ']'). We ensure that the
   --  declaration is well embraced.

   procedure Skip_Line;
   --  Skip current line

   --  Various literal values updated when the corresponding token is read

   Integer_Literal_Value   : Unsigned_Long_Long;
   Integer_Literal_Sign    : Short;
   Integer_Literal_Base    : Unsigned_Short_Short;
   Decimal_Point_Position  : Unsigned_Short_Short;
   Float_Literal_Value     : Long_Double;
   String_Literal_Value    : Name_Id;
   Character_Literal_Value : Unsigned_Short;
   Is_Wide_Literal_Value   : Boolean;

   --  Since the name id used to designate empty string is No_Name, the
   --  incorrect string should not be No_Name.
   Incorrect_String    : constant Name_Id        := Name_Id'Last;
   Incorrect_Character : constant Unsigned_Short := LUS;

   --  Preprocessor and processor related entities

   CPP_Arg_Values : Argument_List (1 .. 64);
   CPP_Arg_Count  : Natural := 0;
   --  Preprocessor arguments (including -I...)

   IAC_Search_Paths : Argument_List (1 .. 64);
   IAC_Search_Count : Natural := 0;
   --  IAC search path (for imports and for preprocessor)

   Keep_TMP_Files  : Boolean := False;
   --  True when we want to keep temporary files generated during the
   --  compilation process

   procedure Add_CPP_Flag (S : String);
   --  Add argument S to the preprocessor flags

   procedure Add_IAC_Search_Path (S : String);
   --  Add argument S to the search path

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

   procedure Write (T : Token_Type);

end Lexer;
