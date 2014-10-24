------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                L E X E R                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2014, Free Software Foundation, Inc.          --
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

with Ada.Containers.Vectors;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Locations;    use Locations;
with Source_Input; use Source_Input;
with Types;        use Types;

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
      T_Pragma_Range,
      T_Pragma_Range_Idl,
      T_Pragma_Subtype,
      T_Pragma_Derived,
      T_Pragma_Switchname,
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
      T_Dot_Dot,                --  currently only used in #pragma range

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

   -----------------
   -- Pragma_Type --
   -----------------

   --  Note: the constants below must be kept in sync with the list of pragma
   --  tokens in Token_Type.

   --  Can't we do away with the annoying duplication???

   Pragma_Id           : constant := 0;
   Pragma_Prefix       : constant := 1;
   Pragma_Version      : constant := 2;
   Pragma_Range        : constant := 3;
   Pragma_Range_Idl    : constant := 4;
   Pragma_Subtype      : constant := 5;
   Pragma_Derived      : constant := 6;
   Pragma_Switchname   : constant := 7;
   Pragma_Unrecognized : constant := 8;

   type Pragma_Type is new Byte range Pragma_Id .. Pragma_Unrecognized;

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

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => String_Access);

   IAC_Search_Paths : String_Vectors.Vector;
   --  IAC search path (for imports and for preprocessor)

   Keep_TMP_Files  : Boolean := False;
   --  True when we want to keep temporary files generated during the
   --  compilation process

   procedure Add_CPP_Flag (S : String);
   --  Add argument S to the preprocessor flags

   procedure Add_IAC_Search_Path (S : String);
   --  Add argument S to the search path

   function Preprocess (Source_Name : Types.Name_Id) return Source_File_Ptr;
   --  Return a Source_File representing the preprocessed source file, unless
   --  preprocessing is disabled, in which case we return the original source
   --  file.

   procedure Process (Source : Source_File_Ptr);
   --  Load file Source in the lexer

   procedure Write (T : Token_Type);

end Lexer;
