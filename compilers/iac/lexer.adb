------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                L E X E R                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2017, Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Command_Line; use GNAT.Command_Line;

with Errors;    use Errors;
with Flags;     use Flags;
with Namet;     use Namet;
with Output;    use Output;
with Utils;     use Utils;

with Platform;

with GNAT.Table;

package body Lexer is

   use ASCII;

   CPP_Arg_Values : String_Vectors.Vector;
   --  Preprocessor arguments (including -I...)

   Current_Source_File : Source_File_Ptr;
   --  Current source file being scanned

   Buffer : Text_Buffer_Ptr;
   --  Always equal to Current_Source_File.Buffer. Contains the EOF-terminated
   --  text of the (usually) preprocessed file. Token_Location.Scan is used to
   --  scan the text.

   function Is_Identifier_Character (C : Character) return Boolean;
   --  Alphabetic character or digit or underscore character

   function To_String_List (V : String_Vectors.Vector) return String_List;
   --  Return an array of V's elements

   -------------------
   -- Handled Files --
   -------------------

   package Handled_Files_Table is new GNAT.Table
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);

   -----------------
   -- Lexer State --
   -----------------

   type Lexer_State is
      record
         Loc               : Location;
         Preprocessed_File : Source_File_Ptr;
      end record;

   package Lexer_State_Stack is new GNAT.Table
     (Table_Component_Type => Lexer_State,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  During the parsing of a file, we may encounter instructions that
   --  require the parsing of another file to be handled. The goal of this
   --  stack is to save the information concerning the current file to be able
   --  to continue its parsing later.

   Initialized : Boolean := False;

   procedure Skip_Identifier;
   --  Skip a sequence of identifier characters as the current
   --  identifier or literal is not well-formed.

   function Quoted_Image (T : Token_Type) return String;
   --  Return an image of token T. Keywords are output between double
   --  quotes and characters between single quotes.

   procedure Scan_Preprocessor_Directive;
   --  Once a '#' character has been detected, scan past that character and
   --  process the remaining of the directive.
   --  It can be either a #pragma directive (in which case a T_Pragma token
   --  is generated) or a line directive.
   --  The latter case is handled internally to update Token_Location.

   procedure Scan_Chars_Literal_Value
     (Literal  : Token_Type;
      Fatal    : Boolean;
      Adjacent : Boolean   := True);
   --
   --  Char Literals : (3.2.5.2)
   --  A character literal is one or more characters enclosed in
   --  single quotes, as in 'x'. Non graphic characters must be
   --  represented using escape sequences as defined in Table
   --  3-9. (escape sequences are \n, \t, \v, \b, \r, \f, \a, \\, \?,
   --  \', \", \ooo, \xhh and \uhhhh)
   --
   --  The escape \ooo consists of the backslash followed by one, two
   --  or three octal digits that are taken to specify the value of
   --  the desired character. The escape \xhh consists of the
   --  backslash followed by x followed by one or two hexadecimal
   --  digits that are taken to specify the value of the desired
   --  character.
   --
   --  The escape \uhhhh consist of a backslash followed by the
   --  character 'u', followed by one, two, three or four hexadecimal
   --  digits.
   --
   --  String Literals : (3.2.5.1)
   --  A string literal is a sequence of characters (...) surrounded
   --  by double quotes, as in "...".
   --
   --  Adjacent string literals are concatenated. (...) Within a
   --  string, the double quote character " must be preceded by a \.
   --  A string literal may not contain the character '\0'.
   --
   --  Wide is used to say if the scanner should scan a wide string or
   --  not. If not and a character looks like '/u...' then an error is
   --  raised

   procedure Scan_Numeric_Literal_Value (Fatal : Boolean);
   --
   --  Integers Literals : (3.2.5.1)
   --  An integer literal consisting of a sequence of digits is taken
   --  to be decimal (base ten), unless it begins with 0 (digit zero).
   --  A sequence of digits starting with 0 is taken to be an octal
   --  integer (base eight).  The digits 8 and 9 are not octal digits.
   --  A sequence of digits preceded by 0x or 0X is taken to be a
   --  hexadecimal integer (base sixteen).  The hexadecimal digits
   --  include a or A through f or F with decimal values ten to
   --  through fifteen, respectively. For example, the number twelve
   --  can be written 12, 014 or 0XC
   --
   --  Floating-point literals : (3.2.5.3)
   --  A floating-point literal consists of an integer part, a decimal
   --  point, a fraction part, an e or E, and an optionally signed
   --  integer exponent.  The integer and fraction parts both consists
   --  of a sequence of decimal (base ten) digits. Either the integer
   --  part or the fraction part (but not both may be missing; either
   --  the decimal point or the letter e (or E) and the exponent (but
   --  not both) may be missing.
   --
   --  Fixed-point literals : (3.2.5.5)
   --  A fixed-point decimal literal consists of an integer part, a
   --  decimal point, a fraction part and a d or D. The integer and
   --  fraction part both consist of a sequence of decimal (base ten)
   --  digits. Either the integer part or the fraction part (but not
   --  both) may be missing; the decimal point (but not the letter d
   --  (or D)) may be missing

   procedure Scan_Integer_Literal_Value
     (Base  : Unsigned_Short_Short;
      Fatal : Boolean;
      Size  : Natural := Natural'Last);
   --  Scan an integer literal in Base with less than Size digits. The
   --  result is stored in Integer_Literal_Value and Token is set to
   --  T_Integer_Literal. When the procedure cannot read any digit or
   --  when a digit is greater than Base, Token is set to T_Error.
   --  The procedure skips the literal and an error message is output
   --  when Fatal is true.

   procedure Scan_Integer_To_Name_Buffer
     (Base  : Unsigned_Short_Short;
      Fatal : Boolean;
      Size  : Natural := Natural'Last);
   --  Scan an integer literal in Base with a max of Size digits. The
   --  result is appended to Name_Buffer. Token is set to
   --  T_Error on failure and to T_Integer_Literal on success.

   procedure Eval_Integer_From_Name_Buffer
     (Base  : Unsigned_Short_Short;
      Fatal : Boolean);
   --  Evaluate integer literal stored in Name_Buffer. Token is set to
   --  T_Error on failure and to T_Integer_Literal on success. The
   --  result is stored in Integer_Literal_Value on success. The
   --  literal is not always well-formed since a character may not be
   --  incorrect. When Fatal is true, the primitive outputs an error
   --  message.

   procedure Scan_Identifier
     (Fatal        : Boolean;
      Is_Directive : Boolean := False);
   --
   --  Names : 3.2.3
   --  An identifier is an arbitrarily long sequence of ASCII
   --  alphabetic, digit and underscore characters.  The first
   --  character must be an ASCII alphabetic character. All characters
   --  are significant.
   --
   --  Keywords : 3.2.4
   --  keywords must be written exactly as in the above
   --  list. Names that collide with keywords (...) are
   --  illegal. For example, "boolean" is a valid keyword, "Boolean"
   --  and "BOOLEAN" are illegal identifiers.
   --
   --  Directives : 3.3
   --  This procedure is also used to scan directives that remain in source
   --  code after preprocessing, in which case the current character location
   --  is the # character that starts the directive.

   procedure Scan_Token (Fatal : Boolean);
   --  Scan token but do not report any error and do not fail on minor
   --  errors like detecting a string which appears to be a wide string.

   procedure New_Token
     (Token : Token_Type;
      Image : String);
   --  Evaluate token image and store it in Token_Image table. When
   --  Token is a graphical character, embrace its image between
   --  single quotes ('<<' and '>>' are considered as graphical
   --  characters). When Token is a keyword, embrace its image between
   --  double quotes. Enter the lower-case form of a keyword image
   --  into the name table and set name table byte to its token
   --  position in order to resolve it easily.

   procedure New_Line;
   --  Increment the line number and save the current position in the
   --  buffer in order to compute later on the column number.
   --  Also, if we have CRLF (carriage return followed by line feed), skip
   --  ahead one character, in order to treat the two characters as a single
   --  end-of-line. We do the same for LFCR, even though no supported operating
   --  systems use that sequence, because that's what the GNAT compiler does.

   procedure Skip_Spaces (Except_Newline : Boolean := False);
   --  Skip all spaces, except end-of-line markers if Except_Newline is True

   function To_Token (Name : Name_Id) return Token_Type;
   --  Return the token matching Name. Otherwise, return T_Error.

   Token_Image : array (Token_Type) of Name_Id;

   ------------------
   -- Add_CPP_Flag --
   ------------------

   procedure Add_CPP_Flag (S : String) is
   begin
      CPP_Arg_Values.Append (new String'(S));
   end Add_CPP_Flag;

   -------------------------
   -- Add_IAC_Search_Path --
   -------------------------

   procedure Add_IAC_Search_Path (S : String) is
   begin
      IAC_Search_Paths.Append (new String'(S));
   end Add_IAC_Search_Path;

   -----------------------------------
   -- Eval_Integer_From_Name_Buffer --
   -----------------------------------

   procedure Eval_Integer_From_Name_Buffer
     (Base  : Unsigned_Short_Short;
      Fatal : Boolean)
   is
      C : Character;
      D : Natural;
   begin
      Integer_Literal_Value := 0;
      for I in 1 .. Name_Len loop
         C := Name_Buffer (I);
         if Integer_Literal_Base = 8 and then C in '8' .. '9' then
            if Fatal then
               Error_Loc (1)      := Token_Location;
               Error_Loc (1).Scan := Error_Loc (1).Scan + Text_Ptr (I - 1);
               DE ("digit >= base");
            end if;
            Skip_Identifier;
            Token := T_Error;
            return;
         end if;

         if C in '0' .. '9' then
            D := Character'Pos (C) - Character'Pos ('0');
         else
            D := Character'Pos (C) - Character'Pos ('a') + 10;
         end if;

         Integer_Literal_Value :=
           Integer_Literal_Value * Unsigned_Long_Long (Base)
           + Unsigned_Long_Long (D);
      end loop;
   end Eval_Integer_From_Name_Buffer;

   --------------
   -- Finalize --
   --------------

   procedure Finalize_Imported is
   begin
      if Lexer_State_Stack.Last /= 0 then
         Pop_Lexer_State;
      end if;
   end Finalize_Imported;

   -------------
   -- Handled --
   -------------

   function Handled (File_Name_Id : Name_Id) return Boolean is
   begin
      for Index in Handled_Files_Table.First .. Handled_Files_Table.Last loop
         if Handled_Files_Table.Table (Index) = File_Name_Id then
            return True;
         end if;
      end loop;
      return False;
   end Handled;

   -----------------
   -- Set_Handled --
   -----------------

   procedure Set_Handled (File_Name_Id : Name_Id) is
   begin
      if not Handled (File_Name_Id) then
         Handled_Files_Table.Append (File_Name_Id);
      end if;
   end Set_Handled;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
   begin
      return Get_Name_String (Token_Image (T));
   end Image;

   -----------------------------
   -- Is_Identifier_Character --
   -----------------------------

   function Is_Identifier_Character (C : Character) return Boolean is
   begin
      return C = '_' or else Is_Alphanumeric (C);
   end Is_Identifier_Character;

   ----------------
   -- Is_Literal --
   ----------------

   function Is_Literal (T : Token_Type) return Boolean is
   begin
      return T in Literal_Type;
   end Is_Literal;

   -----------------
   -- Is_Operator --
   -----------------

   function Is_Operator (T : Token_Type) return Boolean is
   begin
      return T in Operator_Type;
   end Is_Operator;

   --------------------
   -- Is_Scoped_Name --
   --------------------

   function Is_Scoped_Name (T : Token_Type) return Boolean is
   begin
      return T = T_Identifier or else T = T_Colon_Colon;
   end Is_Scoped_Name;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      --  If we have CRLF or LFCR, skip ahead one character, so we treat that
      --  as a single end-of-line

      case Buffer (Token_Location.Scan) is
         when LF =>
            if Buffer (Token_Location.Scan + 1) = CR then
               Token_Location.Scan := Token_Location.Scan + 1;
            end if;
         when CR =>
            if Buffer (Token_Location.Scan + 1) = LF then
               Token_Location.Scan := Token_Location.Scan + 1;
            end if;
         when FF | VT =>
            null;
         when others =>
            raise Program_Error;  --  Caller makes sure we're at end-of-line
      end case;

      --  Increment line number and save current position

      Token_Location.Scan  := Token_Location.Scan + 1;
      Token_Location.First := Token_Location.Scan;
      Token_Location.Last  := Token_Location.Scan;
      Token_Location.Line  := Token_Location.Line + 1;
   end New_Line;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token
     (Token : Token_Type;
      Image : String) is
   begin
      Set_Str_To_Name_Buffer (Image);
      Token_Image (Token) := Name_Find;

      if Token in Keyword_Type or else Token = T_Pragma then
         To_Lower (Name_Buffer (1 .. Name_Len));
         Set_Name_Table_Byte (Name_Find, Byte (Token_Type'Pos (Token)));
      end if;
   end New_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token return Token_Type is
      Current_Token_Name     : Name_Id;
      Current_Token          : Token_Type;
      Current_Token_Location : Location;
      Next_Token_Value       : Token_Type;
   begin
      Current_Token_Name     := Token_Name;
      Current_Token          := Token;
      Current_Token_Location := Token_Location;
      Scan_Token (Fatal => False);
      Next_Token_Value       := Token;
      Token_Name             := Current_Token_Name;
      Token                  := Current_Token;
      Token_Location         := Current_Token_Location;
      return Next_Token_Value;
   end Next_Token;

   ----------------
   -- Preprocess --
   ----------------

   function Preprocess (Source_Name : Types.Name_Id) return Source_File_Ptr is
      Success                     : Boolean;
      Tmp_FDesc                   : File_Descriptor := Invalid_FD;
      Tmp_FName                   : aliased Temp_File_Name; -- NUL terminated
      CPP_Tmp_File_Name           : Name_Id;
      Preprocessor                : String_Access;
      Prep_And_Flags_List : constant Argument_List_Access :=
                              Argument_String_To_List
                                (Platform.IDL_Preprocessor);

      Result : Source_File_Ptr;
      Ignore : Boolean;

   begin
      if Initialized then
         Push_Lexer_State;
      end if;

      Result := Open_Source (Source_Name, Kind => True_Source);
      --  Result will be overwritten with the output of the preprocessor, if it
      --  runs successfully below.

      if No_Preprocess then
         return Result;
      end if;

      --  Reinitialize the CPP arguments

      CPP_Arg_Values.Clear;

      --  Append the preprocessor flags

      for Index in Prep_And_Flags_List'First + 1 .. Prep_And_Flags_List'Last
      loop
         Add_CPP_Flag (Prep_And_Flags_List (Index).all);
      end loop;

      --  Pass user options to the preprocessor.

      Goto_Section ("cppargs");
      while Getopt ("*") /= ASCII.NUL loop
         Add_CPP_Flag (Full_Switch);
      end loop;

      --  Add the paths in the IAC search path to the preprocessor search path

      for P of IAC_Search_Paths loop
         Add_CPP_Flag ("-I");
         Add_CPP_Flag (P.all);
      end loop;

      --  The temporary file containing the preprocessing result

      Add_CPP_Flag ("-o");
      Create_Temp_File (Tmp_FDesc, Tmp_FName);
      if Tmp_FDesc = Invalid_FD then
         DE ("cannot create tmp file");
         raise Fatal_Error;
      end if;
      Close (Tmp_FDesc);

      --  Get temporary file name, drop trailing NUL, add required suffix

      Set_Str_To_Name_Buffer (Tmp_FName);
      Name_Len := Name_Len - 1;
      Add_Str_To_Name_Buffer (Platform.IDL_Preprocessor_Suffix);
      Add_CPP_Flag (Name_Buffer (1 .. Name_Len));

      CPP_Tmp_File_Name := Name_Find;

      --  The source file to be preprocessed

      Add_CPP_Flag (Get_Name_String (Source_Name));

      --  Locate preprocessor

      Preprocessor := Locate_Exec_On_Path
        (Prep_And_Flags_List (Prep_And_Flags_List'First).all);

      if Preprocessor = null then
         DE ("?cannot locate %",
             Prep_And_Flags_List (Prep_And_Flags_List'First).all);
         pragma Assert (Result.Name = Source_Name);
         return Result;
      end if;

      Spawn (Preprocessor.all, To_String_List (CPP_Arg_Values), Success);

      if not Success then
         Error_Name (1) := Source_Name;
         DE ("fail to preprocess%");
         raise Fatal_Error;
      end if;

      Result := Open_Source (CPP_Tmp_File_Name, Kind => Preprocessed_Source);

      --  Now we can delete the preprocessor output file

      if not Keep_TMP_Files then
         Delete_File (Tmp_FName'Address, Ignore);
      end if;

      return Result;

   exception
      when others =>
         --  Delete the preprocessor output file if it exists

         if not Keep_TMP_Files and then Tmp_FDesc /= Invalid_FD then
            Delete_File (Tmp_FName'Address, Ignore);
         end if;
         raise;
   end Preprocess;

   -------------
   -- Process --
   -------------

   procedure Process (Source : Source_File_Ptr) is
   begin

      if not Initialized then
         Initialized := True;

         --  Enter all the alphabetic keywords in the name table

         New_Token (T_Error, "<error>");
         New_Token (T_Abstract, "abstract");
         New_Token (T_Any, "any");
         New_Token (T_Attribute, "attribute");
         New_Token (T_Boolean, "boolean");
         New_Token (T_Case, "case");
         New_Token (T_Char, "char");
         New_Token (T_Component, "component");
         New_Token (T_Const, "const");
         New_Token (T_Consumes, "consumes");
         New_Token (T_Context, "context");
         New_Token (T_Custom, "custom");
         New_Token (T_Default, "default");
         New_Token (T_Double, "double");
         New_Token (T_Emits, "emits");
         New_Token (T_Enum, "enum");
         New_Token (T_Eventtype, "eventtype");
         New_Token (T_Exception, "exception");
         New_Token (T_Factory, "factory");
         New_Token (T_False, "FALSE");
         New_Token (T_Finder, "finder");
         New_Token (T_Fixed, "fixed");
         New_Token (T_Float, "float");
         New_Token (T_Get_Raises, "getraises");
         New_Token (T_Home, "home");
         New_Token (T_Import, "import");
         New_Token (T_In, "in");
         New_Token (T_Inout, "inout");
         New_Token (T_Interface, "interface");
         New_Token (T_Local, "local");
         New_Token (T_Long, "long");
         New_Token (T_Module, "module");
         New_Token (T_Multiple, "multiple");
         New_Token (T_Native, "native");
         New_Token (T_Object, "Object");
         New_Token (T_Octet, "octet");
         New_Token (T_Oneway, "oneway");
         New_Token (T_Out, "out");
         New_Token (T_Primary_Key, "primarykey");
         New_Token (T_Private, "private");
         New_Token (T_Provides, "provides");
         New_Token (T_Public, "public");
         New_Token (T_Publishes, "publishes");
         New_Token (T_Raises, "raises");
         New_Token (T_Readonly, "readonly");
         New_Token (T_Sequence, "sequence");
         New_Token (T_Set_Raises, "setraises");
         New_Token (T_Short, "short");
         New_Token (T_String, "string");
         New_Token (T_Struct, "struct");
         New_Token (T_Supports, "supports");
         New_Token (T_Switch, "switch");
         New_Token (T_True, "TRUE");
         New_Token (T_Truncatable, "truncatable");
         New_Token (T_Typedef, "typedef");
         New_Token (T_Type_Id, "typeid");
         New_Token (T_Type_Prefix, "typeprefix");
         New_Token (T_Unsigned, "unsigned");
         New_Token (T_Union, "union");
         New_Token (T_Uses, "uses");
         New_Token (T_Value_Base, "ValueBase");
         New_Token (T_Value_Type, "valuetype");
         New_Token (T_Void, "void");
         New_Token (T_Wchar, "wchar");
         New_Token (T_Wstring, "wstring");
         New_Token (T_Semi_Colon, ";");
         New_Token (T_Left_Brace, "{");
         New_Token (T_Right_Brace, "}");
         New_Token (T_At, "@");
         New_Token (T_Colon, ":");
         New_Token (T_Comma, ",");
         New_Token (T_Colon_Colon, "::");
         New_Token (T_Dot_Dot, "..");
         New_Token (T_Left_Paren, "(");
         New_Token (T_Right_Paren, ")");
         New_Token (T_Equal, "=");
         New_Token (T_Bar, "|");
         New_Token (T_Circumflex, "^");
         New_Token (T_Ampersand, "&");
         New_Token (T_Greater_Greater, ">>");
         New_Token (T_Less_Less, "<<");
         New_Token (T_Plus, "+");
         New_Token (T_Minus, "-");
         New_Token (T_Star, "*");
         New_Token (T_Slash, "/");
         New_Token (T_Percent, "%");
         New_Token (T_Tilde, "~");
         New_Token (T_Less, "<");
         New_Token (T_Greater, ">");
         New_Token (T_Left_Bracket, "[");
         New_Token (T_Right_Bracket, "]");
         New_Token (T_Integer_Literal, "<int literal>");
         New_Token (T_Fixed_Point_Literal, "<fixed point literal>");
         New_Token (T_Floating_Point_Literal, "<floating point literal>");
         New_Token (T_Character_Literal, "<character literal>");
         New_Token (T_Wide_Character_Literal, "<wide character literal>");
         New_Token (T_String_Literal, "<string literal>");
         New_Token (T_Wide_String_Literal, "<wide string literal>");
         New_Token (T_Identifier, "<identifier>");
         New_Token (T_Pragma, "#pragma");
         New_Token (T_Pragma_Unrecognized, "<unrecognized>");
         New_Token (T_Pragma_Id, "ID");
         New_Token (T_Pragma_Prefix, "prefix");
         New_Token (T_Pragma_Version, "version");
         New_Token (T_Pragma_Range, "range");
         New_Token (T_Pragma_Range_Idl, "range_idl");  --  is not used
         New_Token (T_Pragma_Subtype, "subtype");
         New_Token (T_Pragma_Derived, "derived");
         New_Token (T_Pragma_Switchname, "switchname");
         New_Token (T_Pragma_javaPackage, "javaPackage");
         New_Token (T_EOF, "<end of file>");
      end if;

      --  Set up current source file

      Current_Source_File := Source;
      Buffer := Source.Buffer;

      --  Reset at the beginning

      Token                := T_Error;
      Token_Name           := No_Name;
      Token_Location.Scan  := 1;
      Token_Location.First := 1;
      Token_Location.Last  := 1;
      Set_New_Location (Token_Location, Source.Name, 1);
   end Process;

   ----------------------
   -- Push_Lexer_State --
   ----------------------

   procedure Push_Lexer_State is
   begin
      Lexer_State_Stack.Append
        ((Loc               => Token_Location,
          Preprocessed_File => Current_Source_File));
   end Push_Lexer_State;

   ---------------------
   -- Pop_Lexer_State --
   ---------------------

   procedure Pop_Lexer_State is
      S : constant Lexer_State := Lexer_State_Stack.Table
        (Lexer_State_Stack.Last);
   begin
      Lexer_State_Stack.Decrement_Last;

      Current_Source_File := S.Preprocessed_File;
      Buffer := Current_Source_File.Buffer;
      Token_Location := S.Loc;
   end Pop_Lexer_State;

   ------------------
   -- Quoted_Image --
   ------------------

   function Quoted_Image (T : Token_Type) return String is
   begin
      if T in T_Abstract .. T_Wstring then
         return Quoted (Get_Name_String (Token_Image (T)));
      elsif T in T_Ampersand .. T_Less_Less then
         return Quoted (Get_Name_String (Token_Image (T)), ''');
      end if;
      return Get_Name_String (Token_Image (T));
   end Quoted_Image;

   -------------------
   -- Restore_Lexer --
   -------------------

   procedure Restore_Lexer (State : Location) is
   begin
      Token_Location := State;
   end Restore_Lexer;

   ----------------
   -- Save_Lexer --
   ----------------

   procedure Save_Lexer (State : out Location) is
   begin
      State := Token_Location;
   end Save_Lexer;

   ------------------------------
   -- Scan_Chars_Literal_Value --
   ------------------------------

   procedure Scan_Chars_Literal_Value
     (Literal  : Token_Type;
      Fatal    : Boolean;
      Adjacent : Boolean   := True)
   is
      C         : Character;
      Delimiter : Character := ''';
      Wideness  : Boolean := False;
      Length    : Natural := 0;
      Errors    : Natural := 0;
   begin
      if Literal in T_String_Literal .. T_Wide_String_Literal then
         Delimiter := '"'; -- "
         if Literal = T_Wide_String_Literal then
            Wideness := True;
         end if;
      elsif Literal = T_Wide_Character_Literal then
         Wideness := True;
      end if;

      Name_Len := 0;
      loop
         C := Buffer (Token_Location.Scan);
         Token_Location.Scan := Token_Location.Scan + 1;

         if C = EOF then
            Token := T_Error;
            return;
         end if;

         --  Exit when (C = ''') or (C = '"' and not Adjacent)

         if C = Delimiter then
            exit when not Adjacent;

            --  Look for adjacent strings

            Skip_Spaces;
            exit when Buffer (Token_Location.Scan) /= Delimiter;

            C := Buffer (Token_Location.Scan + 1);
            Token_Location.Scan := Token_Location.Scan + 2;
         end if;

         --  Output only once error message for character literal of
         --  more than one character.

         if Delimiter = ''' and then Length = 1 then
            if Fatal then
               Errors := Errors + 1;
               Error_Loc (1) := Token_Location;
               DE ("strings are delimited by double quote character");
            end if;
         end if;

         Length := Length + 1;

         --  Read escaped character

         if C = '\' then
            case Buffer (Token_Location.Scan) is
               when '\'
                 |  '"' -- "
                 |  '''
                 |  '?' =>
                  Character_Literal_Value :=
                    Character'Pos (Buffer (Token_Location.Scan));
                  Token_Location.Scan := Token_Location.Scan + 1;

               when 'n' =>
                  Character_Literal_Value := Character'Pos (ASCII.LF);
                  Token_Location.Scan := Token_Location.Scan + 1;

               when 't' =>
                  Character_Literal_Value := Character'Pos (ASCII.HT);
                  Token_Location.Scan := Token_Location.Scan + 1;

               when 'v' =>
                  Character_Literal_Value := Character'Pos (ASCII.VT);
                  Token_Location.Scan := Token_Location.Scan + 1;

               when 'b' =>
                  Character_Literal_Value := Character'Pos (ASCII.BS);
                  Token_Location.Scan := Token_Location.Scan + 1;

               when 'r' =>
                  Character_Literal_Value := Character'Pos (ASCII.CR);
                  Token_Location.Scan := Token_Location.Scan + 1;

               when 'f' =>
                  Character_Literal_Value := Character'Pos (ASCII.FF);
                  Token_Location.Scan := Token_Location.Scan + 1;

               when 'a' =>
                  Character_Literal_Value := Character'Pos (ASCII.BEL);
                  Token_Location.Scan := Token_Location.Scan + 1;

               --  Read 1, 2 or 3 octal digits

               when '0' .. '7' =>
                  Scan_Integer_Literal_Value (8, Fatal, 3);
                  if Token = T_Error then
                     if Fatal then
                        Errors := Errors + 1;
                        Error_Loc (1) := Token_Location;
                        DE ("cannot parse octal digits");
                     end if;
                     Integer_Literal_Value := 0;
                  end if;
                  Character_Literal_Value :=
                    Unsigned_Short (Integer_Literal_Value);

               --  Read 1 or 2 hexadecimal digits

               when 'x' =>
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Scan_Integer_Literal_Value (16, Fatal, 2);
                  if Token = T_Error then
                     if Fatal then
                        Errors := Errors + 1;
                        Error_Loc (1) := Token_Location;
                        DE ("cannot parse hexadecimal digits");
                     end if;
                     Integer_Literal_Value := 0;
                  end if;
                  Character_Literal_Value :=
                    Unsigned_Short (Integer_Literal_Value);

               --  Read 1, 2, 3 or 4 hexadecimal digits

               when 'u'  =>
                  if not Wideness then
                     if Fatal then
                        Errors := Errors + 1;
                        Error_Loc (1) := Token_Location;
                        DE ("\u may only be used in wide characters " &
                            "and strings");
                     end if;
                  end if;

                  Token_Location.Scan := Token_Location.Scan + 1;
                  Scan_Integer_Literal_Value (16, Fatal, 4);
                  if Token = T_Error then
                     if Fatal then
                        Errors := Errors + 1;
                        Error_Loc (1) := Token_Location;
                        DE ("cannot parse hexadecimal digits");
                     end if;
                     Integer_Literal_Value := 0;
                  end if;
                  Character_Literal_Value :=
                    Unsigned_Short (Integer_Literal_Value);

               when others =>
                  if Fatal then
                     Errors := Errors + 1;
                     Error_Loc (1) := Token_Location;
                     DE ("unexcepted escaped character");
                  end if;
                  Character_Literal_Value := 0;
            end case;

         else
            Character_Literal_Value := Character'Pos (C);
         end if;

         if Literal in T_String_Literal .. T_Wide_String_Literal then
            if Wideness then
               Add_Char_To_Name_Buffer
                 (Character'Val (Character_Literal_Value / 256));
            end if;
            Add_Char_To_Name_Buffer
              (Character'Val (Character_Literal_Value and 255));
         end if;
      end loop;

      Token := Literal;

      if Literal in T_String_Literal .. T_Wide_String_Literal then
         --  If the string is empty, we assign Token_Name the No_Name value
         if Name_Len = 0 then
            Token_Name := No_Name;
         else
            Token_Name := Name_Find;
         end if;
         String_Literal_Value := Token_Name;
      end if;

      if Errors > 0 then
         Token_Name              := Incorrect_String;
         String_Literal_Value    := Incorrect_String;
         Character_Literal_Value := Incorrect_Character;
      end if;
   end Scan_Chars_Literal_Value;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   procedure Scan_Identifier
     (Fatal        : Boolean;
      Is_Directive : Boolean := False)
   is
      Escaped : Boolean  := False;
   begin

      --  Read escaped identifier

      if Buffer (Token_Location.Scan) = '_' then
         Escaped := True;
         Token_Location.Scan := Token_Location.Scan + 1;
      end if;

      --  Read identifier

      if Is_Directive then
         --  Scan past '#'

         Name_Len := 1;
         Name_Buffer (Name_Len) := '#';
         Token_Location.Scan := Token_Location.Scan + 1;

      else
         Name_Len := 0;
      end if;

      while Is_Identifier_Character (Buffer (Token_Location.Scan)) loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Buffer (Token_Location.Scan);
         Token_Location.Scan := Token_Location.Scan + 1;
      end loop;

      if Name_Len = 0 then
         if Fatal then
            Error_Loc (1) := Token_Location;
            DE ("identifier must start with alphabetic character");
         end if;
         Name_Buffer (1) := ' ';
         Name_Len := 1;
         return;
      end if;

      Token_Name := Name_Find;
      Token      := T_Identifier;

      --  Check whether it is a keyword or a pragma

      if not Escaped then
         Token := To_Token (Token_Name);

         --  Check that the case of keywords is correct.
         --  IDL Syntax and semantics, CORBA V2.3 § 3.2.4
         --
         --  keywords must be written exactly as in the above list. Identifiers
         --  that collide with keywords (...) are illegal.

         if Fatal and then
           Token in Keyword_Type and then
           Token_Name /= Token_Image (Token)
         then
            Error_Loc (1) := Token_Location;
            Error_Name (1) := Token_Image (Token);
            DE ("incorrect case; # expected");
         end if;

         if Token = T_Error then
            Token := T_Identifier;
         elsif Token = T_True then
            Token := T_Boolean_Literal;
            Integer_Literal_Value := 1;
         elsif Token = T_False then
            Token := T_Boolean_Literal;
            Integer_Literal_Value := 0;
         end if;
      end if;

      --  Check that identifier is well-formed

      if Token = T_Identifier then
         if not Is_Letter (Name_Buffer (1)) then
            if Escaped then
               if Fatal then
                  Error_Loc (1) := Token_Location;
                  DE ("incorrect escaped identifier");
               end if;

            else
               if Fatal then
                  Error_Loc (1) := Token_Location;
                  DE ("identifier must start with alphabetic character");
               end if;
            end if;
         end if;
      end if;
   end Scan_Identifier;

   --------------------------------
   -- Scan_Integer_Literal_Value --
   --------------------------------

   procedure Scan_Integer_Literal_Value
     (Base  : Unsigned_Short_Short;
      Fatal : Boolean;
      Size  : Natural := Natural'Last)
   is
   begin
      Token := T_Integer_Literal;
      Name_Len := 0;
      Scan_Integer_To_Name_Buffer (Base, Fatal, Size);
      if Token = T_Error then
         return;
      end if;
      Eval_Integer_From_Name_Buffer (Base, Fatal);
      if Token = T_Error then
         return;
      end if;
   end Scan_Integer_Literal_Value;

   ---------------------------------
   -- Scan_Integer_To_Name_Buffer --
   ---------------------------------

   procedure Scan_Integer_To_Name_Buffer
     (Base  : Unsigned_Short_Short;
      Fatal : Boolean;
      Size  : Natural := Natural'Last)
   is
      C   : Character;
      Len : Integer  := 0;
      Loc : Location := Token_Location;
   begin
      while Len < Size loop
         C := To_Lower (Buffer (Loc.Scan));
         if C in '0' .. '9' then
            if Base = 8 and then C in '8' .. '9' then
               if Fatal then
                  Error_Loc (1) := Loc;
                  DE ("digit >= base");
               end if;
               Skip_Identifier;
               Token := T_Error;
               return;
            end if;

         elsif Base = 16 and then C in 'a' .. 'f' then
            null;

         elsif Base = 10 and then (C = 'e' or else C = 'd') then
            exit;

         elsif Is_Identifier_Character (C) then
            if Fatal then
               Error_Loc (1) := Loc;
               DE ("illegal character");
            end if;
            Skip_Identifier;
            Token := T_Error;
            return;

         else
            exit;
         end if;

         Len := Len + 1;
         Add_Char_To_Name_Buffer (C);
         Loc.Scan := Loc.Scan + 1;
      end loop;

      Token_Location := Loc;
   end Scan_Integer_To_Name_Buffer;

   --------------------------------
   -- Scan_Numeric_Literal_Value --
   --------------------------------

   procedure Scan_Numeric_Literal_Value (Fatal : Boolean)
   is
      C : Character;
      L : Location renames Token_Location;

   begin
      L := Token_Location;
      Token := T_Integer_Literal;
      Name_Len := 0;
      Integer_Literal_Base := 10;
      Integer_Literal_Sign := 1;

      --  Read the sign

      C := To_Lower (Buffer (L.Scan));
      if C = '+' or else C = '-' then
         if C = '-' then
            Integer_Literal_Sign := -1;
         end if;
         L.Scan := L.Scan + 1;
         C := To_Lower (Buffer (L.Scan));
      end if;

      --  Case of an hexadecimal literal (C must not be clobbered here if the
      --  next character turns out to be other than 'x').

      if C = '0' and then To_Lower (Buffer (L.Scan + 1)) = 'x' then
         Integer_Literal_Base := 16;
         L.Scan := L.Scan + 2;

         --  Check the next character is a digit

         C := To_Lower (Buffer (L.Scan));
         if C not in '0' .. '9' and then C not in 'a' .. 'f' then
            if Fatal then
               Error_Loc (1) := L;
               DE ("digit excepted");
            end if;
            Skip_Identifier;
            Token := T_Error;
            return;
         end if;
      end if;

      --  Read the integer part

      if C /= '.' then
         Scan_Integer_To_Name_Buffer (Integer_Literal_Base, Fatal);

         --  Check whether there is a well-formed integer part

         if Token = T_Error then
            return;
         end if;

         C := To_Lower (Buffer (L.Scan));
      end if;

      --  Read the fraction part

      if C = '.' then

         --  It may be a fixed literal. This will be updated when the
         --  fixed literal suffix is detected.

         Token := T_Floating_Point_Literal;

         --  As there is a decimal point, the base is 10. Having a
         --  previous base sets to 8 is not a problem since the
         --  previous digits are in the range '0' .. '7'. But there is
         --  a problem with base 16 as the literal starts with 0x.

         if Integer_Literal_Base = 16 then
            if Fatal then
               Error_Loc (1) := L;
               DE ("cannot parse integer literal");
            end if;
            Skip_Identifier;
            Token := T_Error;
            return;
         end if;
         Integer_Literal_Base := 10;

         --  Append the decimal point and read the fraction part

         Add_Char_To_Name_Buffer (C);
         L.Scan  := L.Scan + 1;
         C := To_Lower (Buffer (L.Scan));
         if C in '0' .. '9' then
            Scan_Integer_To_Name_Buffer (10, Fatal);

            --  Check that the fraction part is a well-formed literal

            if Token = T_Error then
               return;
            end if;
            C := To_Lower (Buffer (L.Scan));
         end if;
      end if;

      --  Read the exponent

      if C = 'e' then
         Token := T_Floating_Point_Literal;
         Add_Char_To_Name_Buffer (C);

         --  Read the exponent sign.

         C := Buffer (L.Scan + 1);
         if C = '-' or else C = '+' then
            Add_Char_To_Name_Buffer (C);
            L.Scan := L.Scan + 1;
         end if;

         --  Check that the exponent part exists

         C := Buffer (L.Scan + 1);
         if C not in '0' .. '9' then
            if Fatal then
               Error_Loc (1) := L;
               DE ("exponent part cannot be missing");
            end if;
            Skip_Identifier;
            Token := T_Error;
            return;
         end if;
         L.Scan := L.Scan + 1;

         --  Read the exponent part

         Scan_Integer_To_Name_Buffer (10, Fatal);

         --  Check that the exponent part is a well-formed literal

         if Token = T_Error then
            return;
         end if;

      --  Skip fixed literal suffix

      elsif C = 'd' then
         Token := T_Fixed_Point_Literal;
         L.Scan := L.Scan + 1;
      end if;

      if (Name_Len > 0
          and then Name_Buffer (1) = '.')
        and then (Name_Len = 1
                  or else (Name_Len > 1
                           and then Name_Buffer (2) not in '0' .. '9'))
      then
         if Fatal then
            Error_Loc (1) := L;
            DE ("both integer and fraction part cannot be missing");
         end if;
         Skip_Identifier;
         Token := T_Error;
         return;
      end if;

      if Token = T_Floating_Point_Literal then
         Float_Literal_Value :=
           Long_Double'Value (Name_Buffer (1 .. Name_Len));

      else
         if Token = T_Fixed_Point_Literal then
            Decimal_Point_Position := 0;
            for I in 1 .. Name_Len loop
               if Name_Buffer (I) = '.' then
                  Decimal_Point_Position :=
                    Unsigned_Short_Short (Name_Len - I);
               end if;
               if Decimal_Point_Position > 0 then
                  Name_Buffer (I) := Name_Buffer (I + 1);
               end if;
            end loop;

            if Decimal_Point_Position > 0 then
               Name_Len := Name_Len - 1;
            end if;

         elsif Name_Len > 1
           and then Name_Buffer (1) = '0'
           and then Integer_Literal_Base /= 16
         then
            Integer_Literal_Base := 8;
         end if;

         Eval_Integer_From_Name_Buffer (Integer_Literal_Base, Fatal);
      end if;
   end Scan_Numeric_Literal_Value;

   ---------------------------------
   -- Scan_Preprocessor_Directive --
   ---------------------------------

   procedure Scan_Preprocessor_Directive is
      C : Character;
   begin
      if Token_Location.Scan = Buffer'Last then
         --  Malformed directive: lone # at end of file

         Token := T_Error;
         return;
      end if;

      --  Peek at next character

      C := Buffer (Token_Location.Scan + 1);

      --  Read pragma directive

      if Is_Letter (C) then
         Scan_Identifier (False, Is_Directive => True);
         return;
      end if;

      --  Scan past '#'

      Token_Location.Scan := Token_Location.Scan + 1;
      Skip_Spaces (Except_Newline => True);

      C := Buffer (Token_Location.Scan);

      --  Read line marker:
      --    # <line>
      --    # <line> "<file>"
      --    # <line> "<file>" <flags>

      --  The line marker is terminated by end-of-line

      if C in '0' .. '9' then
         declare
            Line : Natural;
         begin
            Scan_Integer_Literal_Value (10, True);
            Line := Natural (Integer_Literal_Value);

            Skip_Spaces (Except_Newline => True);

            --  Scan optional file name

            if Buffer (Token_Location.Scan) = '"' then
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_String_Literal;
               Scan_Chars_Literal_Value (T_String_Literal, True, False);
               Get_Name_String (String_Literal_Value);

               --  Remove marker for built-in or command line text

               if Name_Buffer (1) = '<'
                 and then Name_Buffer (Name_Len) = '>'
               then
                  null;

               --  Check the suffix is ".idl"

               elsif Name_Len < 5
                 or else Name_Buffer (Name_Len - 3 .. Name_Len) /= ".idl"
               then
                  Error_Loc (1) := Token_Location;
                  Error_Name (1) := String_Literal_Value;
                  DE ("incorrect suffix in %");

               else
                  --  Note: Skip_Line call below will increment then line
                  --  number, so compensate here by subtracting one.

                  Set_New_Location
                    (Token_Location, String_Literal_Value, Int (Line - 1));
               end if;

            else

               --  See above comment for subtraction of 1 from Line

               Token_Location.Line := Int (Line - 1);
            end if;

            Skip_Line;
            return;
         end;
      end if;

      --  Cannot handle other directives

      Token := T_Error;
      Skip_Line;
   end Scan_Preprocessor_Directive;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token (T : Token_Type) is
      Loc : Location := Token_Location;
   begin
      Scan_Token;
      if T /= Token then
         if T = T_Semi_Colon then
            Loc.Last := Loc.Scan;
            if Buffer (Loc.Last) = LF
              or else Buffer (Loc.Last) = FF
              or else Buffer (Loc.Last) = CR
              or else Buffer (Loc.Last) = VT
            then
               Loc.Last := Loc.Last - 1;
            end if;
         else
            Loc := Token_Location;
         end if;

         Error_Loc (1) := Loc;
         DE ("expected token %", Quoted_Image (T));
         Token := T_Error;
      end if;
   end Scan_Token;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token (L : Token_List_Type) is
   begin
      pragma Assert (L'Length > 1);
      Scan_Token;
      for Index in L'Range loop
         if L (Index) = Token then
            return;  --  All is well
         end if;
      end loop;

      --  Give error message

      Name_Len := 0;
      Add_Str_To_Name_Buffer (Quoted_Image (L (L'First)));
      for Index in L'First + 1 .. L'Last loop
         Add_Str_To_Name_Buffer (" or ");
         Add_Str_To_Name_Buffer (Quoted_Image (L (Index)));
      end loop;
      Error_Loc (1) := Token_Location;
      Error_Name (1) := Name_Find;
      DE ("expected tokens %");
      Token := T_Error;
   end Scan_Token;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token is
   begin
      Scan_Token (Fatal => True);
   end Scan_Token;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token (Fatal : Boolean) is
   begin
      if Token = T_EOF then
         return;
      end if;

      Token := T_Error;
      while Token = T_Error loop
         Skip_Spaces;

         Token_Location.Last := Token_Location.Scan;

         case Buffer (Token_Location.Scan) is
            when LF | FF | CR | VT =>
               New_Line;

            when ';' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Semi_Colon;

            when '{' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Left_Brace;

      --  The line marker is terminated by end-of-line

            when '}' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Brace;

            when '@' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               --  Currently, annotations are skipped.
               --  They are not yet analyzed or stored.
               if Is_Letter (Buffer (Token_Location.Scan)) then
                  while Is_Letter (Buffer (Token_Location.Scan))
                    or else Buffer (Token_Location.Scan) in '0' .. '9'
                    or else Buffer (Token_Location.Scan) = '_'
                  loop
                     Token_Location.Scan := Token_Location.Scan + 1;
                  end loop;
                  Skip_Spaces;
                  if Buffer (Token_Location.Scan) = '(' then
                     Token_Location.Scan := Token_Location.Scan + 1;
                     declare
                        Parentheses : Integer := 1;
                     begin
                        loop
                           Scan_Token (Fatal => False);
                           exit when Token = T_EOF;
                           if Token = T_Left_Paren then
                              Parentheses := Parentheses + 1;
                           elsif Token = T_Right_Paren then
                              exit when Parentheses <= 0;
                              Parentheses := Parentheses - 1;
                           else
                              exit when Parentheses <= 0;
                           end if;
                        end loop;
                     end;
                  end if;
               else
                  Token := T_At;
               end if;

            when ':' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = ':' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Colon_Colon;
               else
                  Token := T_Colon;
               end if;

            when ',' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Comma;

            when '(' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Left_Paren;

            when ')' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Paren;

            when '=' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Equal;

            when '|' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Bar;

            when '^' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Circumflex;

            when '&' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Ampersand;

            when '<' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '<' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Less_Less;
               else
                  Token := T_Less;
               end if;

            when '>' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '>' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Greater_Greater;
               else
                  Token := T_Greater;
               end if;

            when '+' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Plus;

            when '-' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Minus;

            when '*' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Star;

            when '/' =>

               --  Comment like //

               if Buffer (Token_Location.Scan + 1) = '/' then
                  Skip_Line;

               --  Comment like /* ... */ (not nested)

               elsif Buffer (Token_Location.Scan + 1) = '*' then
                  Token_Location.Scan := Token_Location.Scan + 2;
                  while Buffer (Token_Location.Scan) /= EOF
                    and then (Buffer (Token_Location.Scan) /= '*'
                              or else Buffer (Token_Location.Scan + 1) /= '/')
                  loop
                     Token_Location.Scan := Token_Location.Scan + 1;
                  end loop;

                  if Buffer (Token_Location.Scan) = EOF then
                     Error_Loc (1) := Token_Location;
                     DE ("unterminated comment");
                  end if;

                  --  Skip char sequence */
                  Token_Location.Scan := Token_Location.Scan + 2;

               else
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Slash;
               end if;

            when '%' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Percent;

            when '~' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Tilde;

            when '[' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Left_Bracket;

            when ']' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Bracket;

            when '0' .. '9' =>
               Scan_Numeric_Literal_Value (Fatal);

            when '.' =>
               if Buffer (Token_Location.Scan + 1) = '.' then
                  Token_Location.Scan := Token_Location.Scan + 2;
                  Token := T_Dot_Dot;
               else
                  Scan_Numeric_Literal_Value (Fatal);
               end if;

            when ''' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Scan_Chars_Literal_Value (T_Character_Literal, Fatal);

            when '"' => -- "
               Token_Location.Scan := Token_Location.Scan + 1;
               Scan_Chars_Literal_Value (T_String_Literal, Fatal, True);

            when '#' =>
               Scan_Preprocessor_Directive;

               --  No real token found. Loop again.
               if Token /= T_Pragma then
                  Token := T_Error;
               end if;

            when '_' =>
               Scan_Identifier (Fatal);

            when EOF =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_EOF;

            when others =>
               if Is_Letter (Buffer (Token_Location.Scan)) then

                  --
                  --  Wide Chars : 3.5.2.2
                  --  Wide characters literals have an L prefix, for example :
                  --      const wchar C1 = L'X';
                  --
                  --  Wide Strings : 3.5.2.4
                  --  Wide string literals have an L prefix, for example :
                  --      const wstring S1 = L"Hello";

                  if Buffer (Token_Location.Scan) = 'L' then

                     --  Read wide character literal

                     if Buffer (Token_Location.Scan + 1) = ''' then
                        Token_Location.Scan := Token_Location.Scan + 2;
                        Scan_Chars_Literal_Value
                          (T_Wide_Character_Literal, Fatal);
                        return;

                     --  Read wide string literal

                     elsif Buffer (Token_Location.Scan + 1) = '"' then
                        Token_Location.Scan := Token_Location.Scan + 2;
                        Scan_Chars_Literal_Value
                          (T_Wide_String_Literal, Fatal, True);
                        return;
                     end if;
                  end if;

                  Scan_Identifier (Fatal);

               else
                  Error_Loc (1) := Token_Location;
                  DE ("invalid character");

                  --  Try to rescue parser and find the beginning of a
                  --  potential token

                  Token_Location.Scan := Token_Location.Scan + 1;
                  while Is_Letter (Buffer (Token_Location.Scan))
                    or else Buffer (Token_Location.Scan) in '0' .. '9'
                    or else Buffer (Token_Location.Scan) = '_'
                  loop
                     Token_Location.Scan := Token_Location.Scan + 1;
                  end loop;
               end if;
         end case;
      end loop;
   end Scan_Token;

   ----------------------
   -- Skip_Declaration --
   ----------------------

   procedure Skip_Declaration (Delimiter : Token_Type) is
      Braces : Integer := 0;
      State  : Location;
   begin
      loop
         Save_Lexer (State);
         Scan_Token (Fatal => False);

         exit when Token = T_EOF;

         if Token in T_Left_Brace .. T_Left_Paren then
            Braces := Braces + 1;

         elsif Token in T_Right_Brace .. T_Right_Paren then
            exit when Braces <= 0
              and then Delimiter in T_Right_Brace .. T_Right_Paren;
            Braces := Braces - 1;

         elsif Token in T_Colon .. T_Semi_Colon then
            exit when Braces <= 0
              and then Delimiter in T_Colon .. T_Semi_Colon;
         end if;
      end loop;

      --  When we reach the end of the file without finding a proper
      --  delimiter, we cannot rescue the lexer.

      if Token /= T_EOF then
         Restore_Lexer (State);
         Scan_Token (Delimiter);
      end if;
   end Skip_Declaration;

   ---------------------
   -- Skip_Identifier --
   ---------------------

   procedure Skip_Identifier is
   begin
      while Is_Identifier_Character (Buffer (Token_Location.Scan)) loop
         Token_Location.Scan := Token_Location.Scan + 1;
      end loop;
   end Skip_Identifier;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line is
   begin
      loop
         case Buffer (Token_Location.Scan) is
            when LF | FF | CR | VT =>
               New_Line;
               exit;
            when others =>
               null;
         end case;
         Token_Location.Scan := Token_Location.Scan + 1;
      end loop;
   end Skip_Line;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces (Except_Newline : Boolean := False) is
   begin
      loop
         case Buffer (Token_Location.Scan) is
            when ' ' | HT =>
               Token_Location.Scan := Token_Location.Scan + 1;

            when LF | FF | CR | VT =>
               if Except_Newline then
                  exit;
               else
                  New_Line;
               end if;

            when others =>
               exit;
         end case;
      end loop;
   end Skip_Spaces;

   --------------------
   -- To_String_List --
   --------------------

   function To_String_List (V : String_Vectors.Vector) return String_List is
   begin
      return L : String_List (1 .. Integer (V.Length)) do
         for J in L'Range loop
            L (J) := V.Element (J);
         end loop;
      end return;
   end To_String_List;

   --------------
   -- To_Token --
   --------------

   function To_Token (Name : Name_Id) return Token_Type is
      B : Byte;
   begin
      Get_Name_String (Name);
      To_Lower (Name_Buffer (1 .. Name_Len));
      B := Get_Name_Table_Byte (Name_Find);
      if B <= Last_Token_Pos then
         return Token_Type'Val (B);
      end if;
      return T_Error;
   end To_Token;

   ----------------------
   -- Unexpected_Token --
   ----------------------

   procedure Unexpected_Token (T : Token_Type; C : String) is
   begin
      Error_Loc (1) := Token_Location;
      Set_Str_To_Name_Buffer (Quoted_Image (T));
      Error_Name (1) := Name_Find;
      Set_Str_To_Name_Buffer (C);
      Error_Name (2) := Name_Find;
      DE ("unexpected % in %");
   end Unexpected_Token;

   -----------
   -- Write --
   -----------

   procedure Write (T : Token_Type) is
   begin
      Write_Str (Image (T));
   end Write;

end Lexer;
