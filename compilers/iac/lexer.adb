with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Charset;   use Charset;
with Errors;    use Errors;
with Flags;     use Flags;
with Locations; use Locations;
with Namet;     use Namet;
with Types;     use Types;
with Utils;     use Utils;

with Platform;

package body Lexer is

   use ASCII;

   Buffer : Text_Buffer_Ptr;
   --  Once preprocessed, the idl file is loaded in Buffer and
   --  Token_Location.Scan is used to scan the source file.

   CPP_Tmp_File : Name_Id := No_Name;

   procedure Make_Cleanup;
   --  Cleanup temporary files when needed

   function Quoted_Image (T : Token_Type) return String;
   --  Return an image of token T. Keywords are output between double
   --  quotes and characters between single quotes.

   procedure Scan_Preprocessor_Directive;
   --  Once a '#' character has been detected, scan the remaining
   --  line. It can be either a pragma directive or a line
   --  directive. The latter is handled internally in order to update
   --  Token_Location.

   procedure Scan_Chars_Literal_Value
     (Delimiter : Character := '"'; --  "
      Adjacent  : Boolean   := True);
   --
   --  Char Literals : (3.2.5.2)
   --  A character literal is one or more characters enclosed in
   --  single quotes, as in 'x'. Nongraphic characters must be
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

   procedure Scan_String_Literal_Value (Adjacent : Boolean := False);
   procedure Scan_Character_Literal_Value;

   procedure Scan_Numeric_Literal_Value;
   --
   --  Integers Literals : (3.2.5.1)
   --  An integer literal consisting of a sequence of digits is taken
   --  to be decimal (base ten), unless it begins with 0 (digit zero).
   --  A sequence of digits starting with 0 is taken to be an octal
   --  integer (base eight).  The digits 8 and 9 are not octal digits.
   --  A sequence of digits preceded by 0x or 0X is taken to be a
   --  hexadecimal integer (base sixteen).  The hexadecimal digits
   --  include a or A through f or F with decimal values ten to
   --  through fifteen, repectively. For example, the number twelve
   --  can be written 12, 014 or 0XC
   --
   --  Floating-point literals : (3.2.5.3)
   --  A floating-point literal consists of an integer part, a decimal
   --  point, a fraction part, an e or E, and an optionnaly signed
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
     (Base : Integer;
      Size : Natural := Natural'Last);
   --  Scan an integer literal in Base with a maximum of Size
   --  digits. The result is stored in Integer_Literal_Value and Token
   --  is set to T_Integer_Literal. When the procedure cannot read any
   --  digit or when a digit is greater than the base, Token is set to
   --  T_Error and Integer_Literal_Value is set to 0.

   procedure Scan_Identifier;
   --
   --  Names : 3.2.3
   --  An identifier is an arbritrarily long sequence of ASCII
   --  alphabetic, digit and underscore characters.  The first
   --  character must be an ASCII alphabetic character. All characters
   --  are significant.
   --
   --  Keywords : 3.2.4
   --  keywords must be written exactly as in the above
   --  list. Names that collide with keywords (...) are
   --  illegal. For example, "boolean" is a valid keyword, "Boolean"
   --  and "BOOLEAN" are illegal identifiers.

   procedure Store_Encoded_Character (C : Natural);
   --  Use the brackets notation, where a wide character is
   --  represented by the sequence ["xx"] or ["xxxx"] where xx are
   --  hexadecimal characters to store C.

   procedure New_Token
     (Token : Token_Type;
      Image : String);
   --  Compute token image and store it in Token_Image table. When
   --  Token is a graphical character, embrace its image between
   --  single quotes ('<<' and '>>' are considered as graphical
   --  characters). When Token is a keyword, embrace its image between
   --  double quotes. Enter the lower-case form of a keyword image
   --  into the name table and set name table byte to its token
   --  position in order to resolve it easily.

   procedure New_Line;
   --  Increment the line number and save the current position in the
   --  buffer in order to compute later on the column number.

   procedure Skip_Line;
   --  Skip current line

   procedure Skip_Spaces;
   --  Skip all spaces

   function To_Token (Name : Name_Id) return Token_Type;
   --  Return the token matching Name. Otherwise, return T_Error.

   Token_Image : array (Token_Type) of Name_Id;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
   begin
      return Get_Name_String (Token_Image (T));
   end Image;

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

   ------------------
   -- Make_Ckeanup --
   ------------------

   procedure Make_Cleanup is
      Success : Boolean;
   begin
      if CPP_Tmp_File /= No_Name then
         Get_Name_String (CPP_Tmp_File);
         Name_Buffer (Name_Len + 1) := ASCII.NUL;
         Delete_File (Name_Buffer'Address, Success);
      end if;
   end Make_Cleanup;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Token_Location.Scan := Token_Location.Scan + 1;
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

      if Token in Keyword_Type then
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
      Scan_Token;
      Next_Token_Value       := Token;
      Token_Name             := Current_Token_Name;
      Token                  := Current_Token;
      Token_Location         := Current_Token_Location;
      return Next_Token_Value;
   end Next_Token;

   ------------
   -- Output --
   ------------

   procedure Output
     (Source : File_Descriptor)
   is
      Length  : constant := 1024;
      Buffer  : String (1 .. Length);
      Result  : Integer;
   begin
      loop
         Result := Read  (Source, Buffer (1)'Address, Length);
         exit when Result <= 0;
         Result := Write (Standout, Buffer (1)'Address, Result);
      end loop;
      Close (Source);

      Make_Cleanup;
   end Output;

   ----------------
   -- Preprocess --
   ----------------

   procedure Preprocess
     (Source : Name_Id;
      Result : out File_Descriptor)
   is
      Success      : Boolean;
      Tmp_FDesc    : File_Descriptor;
      Tmp_FName    : Temp_File_Name;
      Preprocessor : String_Access;

   begin
      --  Use default CPP options:
      --  -E           only preprocess
      --  -C           do not discard comments
      --  -x c++       use c++ preprocessor semantic
      Add_CPP_Flag ("-E");
      Add_CPP_Flag ("-C");
      Add_CPP_Flag ("-x");
      Add_CPP_Flag ("c++");
      Add_CPP_Flag ("-ansi");

      --  Pass user options to the preprocessor.
      Goto_Section ("cppargs");
      while Getopt ("*") /= ASCII.Nul loop
         Add_CPP_Flag (Full_Switch);
      end loop;

      --  Add the current directory to the include list.
      Add_CPP_Flag ("-I");
      Add_CPP_Flag (".");

      Create_Temp_File (Tmp_FDesc, Tmp_FName);
      if Tmp_FDesc = Invalid_FD then
         DE ("cannot create tmp file");
         raise Fatal_Error;
      end if;
      Close (Tmp_FDesc);

      Add_CPP_Flag ("-o");
      Add_CPP_Flag (Tmp_FName);

      Set_Str_To_Name_Buffer (Tmp_FName);
      CPP_Tmp_File := Name_Find;

      Add_CPP_Flag (Get_Name_String (Source));

      Preprocessor := Locate_Exec_On_Path (Platform.Preprocessor);
      if Preprocessor = null then
         DE ("cannot locate " & Platform.Preprocessor);
         raise Fatal_Error;
      end if;

      Spawn (Preprocessor.all, CPP_Arg_Values (1 .. CPP_Arg_Count), Success);
      if not Success then
         Error_Name (1) := Source;
         DE ("fail to preprocess%");
         raise Fatal_Error;
      end if;

      Name_Buffer (1 .. Temp_File_Len) := Tmp_FName;
      Name_Buffer (Temp_File_Len + 1)  := ASCII.NUL;

      Tmp_FDesc := Open_Read (Name_Buffer'Address, Binary);
      if Tmp_FDesc = Invalid_FD then
         DE ("cannot open preprocessor output");
         raise Fatal_Error;
      end if;

      Result := Tmp_FDesc;

      Token_Location := Locations.No_Location;
      Set_New_Location (Token_Location, Source, 1);
   end Preprocess;

   -------------
   -- Process --
   -------------

   procedure Process
     (Source_File : File_Descriptor;
      Source_Name : Name_Id)
   is
      Result    : Integer;
      Length    : Integer;

   begin

      --  Load source file in a buffer

      Length := Integer (File_Length (Source_File));
      Buffer := new Text_Buffer (1 .. Text_Ptr (Length + 1));

      --  Force the last character to be EOF

      Buffer (Text_Ptr (Length + 1)) := EOF;

      Token_Location.Scan := 1;
      loop
         Result := Read
           (Source_File, Buffer (Token_Location.Scan)'Address, Length);
         exit when Result = Length;
         if Result <= 0 then
            DE ("cannot read preprocessor output");
            raise Fatal_Error;
         end if;
         Token_Location.Scan := Token_Location.Scan + Text_Ptr (Result);
         Length  := Length - Result;
      end loop;
      Close (Source_File);

      Make_Cleanup;

      --  Reset at the beginning

      Token_Location.Scan := 1;
      Token_Location.First := 1;
      Token_Location.Last  := 1;
      Set_New_Location (Token_Location, Source_Name, 1);

      --  Enter all the alphabetic keywords in the name table

      New_Token (T_Error, "<error>");
      New_Token (T_Abstract, "abstract");
      New_Token (T_Any, "any");
      New_Token (T_Attribute, "attribute");
      New_Token (T_Boolean, "boolean");
      New_Token (T_Case, "case");
      New_Token (T_Char, "char");
      New_Token (T_Const, "const");
      New_Token (T_Context, "context");
      New_Token (T_Custom, "custom");
      New_Token (T_Default, "default");
      New_Token (T_Double, "double");
      New_Token (T_Enum, "enum");
      New_Token (T_Exception, "exception");
      New_Token (T_Factory, "factory");
      New_Token (T_False, "FALSE");
      New_Token (T_Fixed, "fixed");
      New_Token (T_Float, "float");
      New_Token (T_In, "in");
      New_Token (T_Inout, "inout");
      New_Token (T_Interface, "interface");
      New_Token (T_Long, "long");
      New_Token (T_Module, "module");
      New_Token (T_Native, "native");
      New_Token (T_Object, "Object");
      New_Token (T_Octet, "octet");
      New_Token (T_Oneway, "oneway");
      New_Token (T_Out, "out");
      New_Token (T_Private, "private");
      New_Token (T_Public, "public");
      New_Token (T_Raises, "raises");
      New_Token (T_Readonly, "readonly");
      New_Token (T_Sequence, "sequence");
      New_Token (T_Short, "short");
      New_Token (T_String, "string");
      New_Token (T_Struct, "struct");
      New_Token (T_Supports, "supports");
      New_Token (T_Switch, "switch");
      New_Token (T_True, "TRUE");
      New_Token (T_Truncatable, "truncatable");
      New_Token (T_Typedef, "typedef");
      New_Token (T_Unsigned, "unsigned");
      New_Token (T_Union, "union");
      New_Token (T_Value_Base, "ValueBase");
      New_Token (T_Value_Type, "valuetype");
      New_Token (T_Void, "void");
      New_Token (T_Wchar, "wchar");
      New_Token (T_Wstring, "wstring");
      New_Token (T_Semi_Colon, ";");
      New_Token (T_Left_Brace, "{");
      New_Token (T_Right_Brace, "}");
      New_Token (T_Colon, ":");
      New_Token (T_Comma, ",");
      New_Token (T_Colon_Colon, "'::'");
      New_Token (T_Left_Paren, "(");
      New_Token (T_Right_Paren, ")");
      New_Token (T_Equal, "=");
      New_Token (T_Bar, "|");
      New_Token (T_Circumflex, "^");
      New_Token (T_Ampersand, "&");
      New_Token (T_Greater_Greater, "'>>'");
      New_Token (T_Less_Less, "'<<'");
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
      New_Token (T_Pragma, "<pragma>");
      New_Token (T_EOF, "<end of file>");
   end Process;

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

   ----------------------------------
   -- Scan_Character_Literal_Value --
   ----------------------------------

   procedure Scan_Character_Literal_Value is
   begin
      Scan_Chars_Literal_Value (''', False);
   end Scan_Character_Literal_Value;

   ------------------------------
   -- Scan_Chars_Literal_Value --
   ------------------------------

   procedure Scan_Chars_Literal_Value
     (Delimiter : Character := '"'; --  "
      Adjacent  : Boolean   := True)
   is
      C      : Character;
      Length : Natural := 0;
   begin
      if Delimiter = ''' then
         Token := T_Character_Literal;
      elsif Delimiter = '"' then --  "
         Token := T_String_Literal;
      end if;

      Name_Len := 0;

      --  Skip delimiter

      Token_Location.Scan := Token_Location.Scan + 1;
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
            Error_Loc (1) := Token_Location;
            DE ("strings are delimited by double quote character");
         end if;

         Length := Length + 1;

         --  Read escaped character

         if C = '\' then
            case Buffer (Token_Location.Scan) is
               when 'n' | 't' | 'v' | 'b' | 'r' | '"' | -- "
                    'f' | 'a' | '\' | ''' | '?' =>
                  Add_Char_To_Name_Buffer (Buffer (Token_Location.Scan));
                  Token_Location.Scan := Token_Location.Scan + 1;

               --  Read 1, 2 or 3 octal digits

               when '0' .. '7' =>

                  Scan_Integer_Literal_Value (8, 3);
                  if Token = T_Error then
                     Error_Loc (1) := Token_Location;
                     DE ("cannot parse octal digits");
                     return;
                  end if;
                  Store_Encoded_Character (Natural (Integer_Literal_Value));

               --  Read 1 or 2 hexadecimal digits

               when 'x' =>
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Scan_Integer_Literal_Value (16, 2);
                  if Token = T_Error then
                     Error_Loc (1) := Token_Location;
                     DE ("cannot parse hexadecimal digits");
                     return;
                  end if;
                  Store_Encoded_Character (Natural (Integer_Literal_Value));

               --  Read 1, 2, 3 or 4 hexadecimal digits

               when 'u'  =>
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Scan_Integer_Literal_Value (16, 4);
                  if Token = T_Error then
                     Error_Loc (1) := Token_Location;
                     DE ("cannot parse hexadecimal digits");
                     return;
                  end if;
                  if Token = T_String_Literal then
                     Token := T_Wide_String_Literal;
                  elsif Token = T_Character_Literal then
                     Token := T_Wide_Character_Literal;
                  end if;
                  Store_Encoded_Character (Natural (Integer_Literal_Value));

               when others =>
                  Error_Loc (1) := Token_Location;
                  DE ("unexcepted escaped character");
            end case;

         else
            Add_Char_To_Name_Buffer (C);
         end if;
      end loop;

      String_Literal_Value := Name_Find;
      Token_Name := String_Literal_Value;
   end Scan_Chars_Literal_Value;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   procedure Scan_Identifier is
      Escaped : Boolean  := False;
   begin

      --  Read escaped identifier

      if Buffer (Token_Location.Scan) = '_' then
         Escaped := True;
         Token_Location.Scan := Token_Location.Scan + 1;
      end if;

      --  Read identifier

      Name_Len := 0;
      while Is_Identifier_Character (Buffer (Token_Location.Scan)) loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Buffer (Token_Location.Scan);
         Token_Location.Scan := Token_Location.Scan + 1;
      end loop;

      Token_Name := Name_Find;
      Token      := T_Identifier;

      --  Check whether it is a keyword

      if not Escaped then
         Token := To_Token (Token_Name);
         if Token = T_Error then
            Token := T_Identifier;
         elsif Token = T_True then
            Token := T_Boolean_Literal;
            Boolean_Literal_Value := True;
         elsif Token = T_False then
            Token := T_Boolean_Literal;
            Boolean_Literal_Value := False;
         end if;
      end if;
   end Scan_Identifier;

   --------------------------------
   -- Scan_Integer_Literal_Value --
   --------------------------------

   procedure Scan_Integer_Literal_Value
     (Base : Integer;
      Size : Natural := Natural'Last)
   is
      C      : Character;
      Length : Integer := 0;
      Digit  : Integer;
   begin
      Integer_Literal_Value := 0;
      Token := T_Integer_Literal;

      while Length < Size loop
         C := To_Lower (Buffer (Token_Location.Scan));
         if C in '0' .. '9' then
            Digit := Character'Pos (C) - Character'Pos ('0');

         elsif Base = 16 and then C in 'a' .. 'f' then
            Digit := Character'Pos (C) - Character'Pos ('a') + 10;

         else
            exit;
         end if;

         if Digit >= Base then
            Error_Loc (1) := Token_Location;
            DE ("digit >= base");
            Token := T_Error;
            Integer_Literal_Value := 0;
            return;
         end if;

         Length := Length + 1;
         Token_Location.Scan := Token_Location.Scan + 1;

         Integer_Literal_Value :=
           Integer_Literal_Value * Long_Long_Integer (Base) +
           Long_Long_Integer (Digit);
      end loop;

      if Length = 0 then
         Integer_Literal_Value := 0;
         Token := T_Error;
      end if;
   end Scan_Integer_Literal_Value;

   --------------------------------
   -- Scan_Numeric_Literal_Value --
   --------------------------------

   procedure Scan_Numeric_Literal_Value is
      C             : Character;
      Integer_Part  : Long_Long_Integer := -1;
      Fraction_Part : Long_Long_Integer := -1;
      Exponent_Part : Long_Long_Integer := -1;
      Exponent_Sign : Long_Long_Integer := 1;
   begin
      Token := T_Error;
      Integer_Literal_Base := 10;

      --  Specific case to get the base

      if Buffer (Token_Location.Scan) = '0' then
         C := To_Lower (Buffer (Token_Location.Scan + 1));

         --  Base is 16

         if C = 'x' then
            Integer_Literal_Base := 16;
            Token_Location.Scan := Token_Location.Scan + 2;

            C := To_Lower (Buffer (Token_Location.Scan));
            if C not in '0' .. '9' and then C not in 'a' .. 'f' then
               Error_Loc (1) := Token_Location;
               DE ("digit excepted");
               return;
            end if;

         --  Base is 8

         elsif C in '0' .. '9' then
            Integer_Literal_Base := 8;
            Token_Location.Scan := Token_Location.Scan + 1;

         end if;
      end if;

      --  Read the integer part. If Base is not 10, we are sure that
      --  the current character is a digit in Base.

      if Buffer (Token_Location.Scan) /= '.' then
         Scan_Integer_Literal_Value (Integer_Literal_Base);
         if Token = T_Error then
            return;
         end if;
         Integer_Part := Integer_Literal_Value;

         if Integer_Literal_Base /= 10 then
            Token := T_Integer_Literal;
            return;
         end if;
      end if;

      C := To_Lower (Buffer (Token_Location.Scan));

      --  Read the fraction part.

      if C = '.' then
         Token_Location.Scan := Token_Location.Scan + 1;
         if Buffer (Token_Location.Scan) in '0' .. '9' then
            Scan_Integer_Literal_Value (10);
            Fraction_Part := Integer_Literal_Value;
         end if;

         --  Update C to get either the exponent or the fixed suffix

         if To_Lower (Buffer (Token_Location.Scan)) in 'd' .. 'e' then
            C := To_Lower (Buffer (Token_Location.Scan));
         end if;
      end if;

      --  Read the exponent.

      if C = 'e' then
         Token_Location.Scan := Token_Location.Scan + 1;

         --  Read the exponent sign.

         if Buffer (Token_Location.Scan) = '-' then
            Exponent_Sign := -1;
            Token_Location.Scan := Token_Location.Scan + 1;
         elsif Buffer (Token_Location.Scan) = '+' then
            Exponent_Sign := 1;
            Token_Location.Scan := Token_Location.Scan + 1;
         end if;

         --  Read the exponent part.

         if Buffer (Token_Location.Scan) in '0' .. '9' then
            Scan_Integer_Literal_Value (10);
            Exponent_Part := Integer_Literal_Value;
         end if;

      --  Skip fixed literal suffix

      elsif C = 'd' then
         Token_Location.Scan := Token_Location.Scan + 1;
      end if;

      if Integer_Part = -1 and then Fraction_Part = -1 then
         Error_Loc (1) := Token_Location;
         DE ("both integer and fraction part cannot be missing");
         return;
      end if;

      if C = 'e' and then Exponent_Part = -1 then
         Error_Loc (1) := Token_Location;
         DE ("exponent part cannot be missing");
         return;
      end if;

      --  Find a fixed point literal

      if C = 'd' then
         Token := T_Fixed_Point_Literal;

      --  Find a floating point literal

      elsif C = 'e' or else C = '.' or else Fraction_Part /= -1 then
         Token := T_Floating_Point_Literal;

      --  Find an integer literal

      else
         Token := T_Integer_Literal;
      end if;

      if Integer_Part = -1 then
         Integer_Part := 0;
      end if;

      if Fraction_Part = -1 then
         Fraction_Part := 0;
      end if;

      if Exponent_Part = -1 then
         Exponent_Part := 0;
      end if;

      Set_Dnat_To_Name_Buffer (Dnat (Integer_Part));

      if Token /= T_Integer_Literal then
         Add_Char_To_Name_Buffer ('.');
         Add_Dnat_To_Name_Buffer (Dnat (Fraction_Part));

         if Token = T_Floating_Point_Literal then
            Add_Char_To_Name_Buffer ('E');
            if Exponent_Sign = -1 then
               Add_Char_To_Name_Buffer ('-');
            end if;
            Add_Dnat_To_Name_Buffer (Dnat (Exponent_Part));
         end if;

         Float_Literal_Value
           := Long_Long_Float'Value (Name_Buffer (1 .. Name_Len));

         if Token = T_Fixed_Point_Literal then
            Add_Char_To_Name_Buffer ('D');
         end if;
      end if;

      Token_Name := Name_Find;
   end Scan_Numeric_Literal_Value;

   ---------------------------------
   -- Scan_Preprocessor_Directive --
   ---------------------------------

   procedure Scan_Preprocessor_Directive is
      C : Character;
   begin
      Skip_Spaces;

      C := Buffer (Token_Location.Scan);

      --  Read a directive like "# <line> "<file>" <code>

      if C in '0' .. '9' then
         declare
            Line : Natural;
         begin
            Scan_Integer_Literal_Value (10);
            Line := Natural (Integer_Literal_Value);

            Skip_Spaces;
            if Buffer (Token_Location.Scan) = '"' then --  "
               Scan_String_Literal_Value (False);
               Get_Name_String (String_Literal_Value);

               --  Remove CPP special info
               if Name_Buffer (1) = '<'
                 and then Name_Buffer (Name_Len) = '>'
               then
                  Skip_Line;

               --  Check the suffix is ".idl"
               elsif Name_Len < 5
                 or else Name_Buffer (Name_Len - 3 .. Name_Len) /= ".idl"
               then
                  Error_Loc (1) := Token_Location;
                  DE ("incorrect suffix");

               else
                  Skip_Line;
                  Set_New_Location
                    (Token_Location, String_Literal_Value, Int (Line));
               end if;

               return;
            end if;
         end;

      --   Read pragma directive

      elsif Is_Alphabetic_Character (C) then
         Scan_Identifier;
         if To_Token (Token_Name) = T_Pragma then
            Token := T_Pragma;
            return;
         end if;
      end if;

      --  Cannot handle other directives

      Token := T_Error;
      Skip_Line;
   end Scan_Preprocessor_Directive;

   -------------------------------
   -- Scan_String_Literal_Value --
   -------------------------------

   procedure Scan_String_Literal_Value (Adjacent : Boolean := False) is
   begin
      Scan_Chars_Literal_Value ('"', Adjacent); -- "
   end Scan_String_Literal_Value;

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
         DE ("expected token " & Quoted_Image (T));
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
            return;
         end if;
      end loop;
      Set_Str_To_Name_Buffer ("expected token");
      if L'Length > 1 then
         Add_Char_To_Name_Buffer ('s');
      end if;
      Add_Char_To_Name_Buffer (' ');
      Add_Str_To_Name_Buffer (Quoted_Image (L (L'First)));
      for Index in L'First + 1 .. L'Last loop
         Add_Str_To_Name_Buffer (" or ");
         Add_Str_To_Name_Buffer (Quoted_Image (L (Index)));
      end loop;
      declare
         S : constant String := Name_Buffer (1 .. Name_Len);
      begin
         Error_Loc (1) := Token_Location;
         DE (S);
      end;
      Token := T_Error;
   end Scan_Token;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token is
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

            when '}' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Brace;

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
               Scan_Numeric_Literal_Value;

            when '.' =>
               Scan_Numeric_Literal_Value;

            when ''' =>
               Scan_Character_Literal_Value;

            when '"' => -- "
               Scan_String_Literal_Value (True);

            when '#' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Scan_Preprocessor_Directive;

               --  No real token found. Loop again.

               Token := T_Error;

            when '_' =>
               if Is_Alphabetic_Character
                 (Buffer (Token_Location.Scan + 1)) then
                  Scan_Identifier;
               else
                  Error_Loc (1) := Token_Location;
                  DE ("incorrect escaped identifier");
               end if;

            when EOF =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_EOF;

            when others =>
               if Is_Alphabetic_Character (Buffer (Token_Location.Scan)) then

                  --
                  --  Wide Chars : 3.5.2.2
                  --  Wide characters litterals have an L prefix, for example :
                  --      const wchar C1 = L'X';
                  --
                  --  Wide Strings : 3.5.2.4
                  --  Wide string literals have an L prefix, for example :
                  --      const wstring S1 = L"Hello";

                  if Buffer (Token_Location.Scan) = 'L' then

                     --  Read wide character literal

                     if Buffer (Token_Location.Scan + 1) = ''' then
                        Token_Location.Scan := Token_Location.Scan + 1;
                        Scan_Character_Literal_Value;
                        if Token = T_Character_Literal then
                           Token := T_Wide_Character_Literal;
                        end if;
                        return;

                     --  Read wide string literal

                     elsif Buffer (Token_Location.Scan + 1) = '"' then --  "
                        Token_Location.Scan := Token_Location.Scan + 1;
                        Scan_String_Literal_Value (True);
                        if Token = T_String_Literal then
                           Token := T_Wide_String_Literal;
                        end if;
                        return;
                     end if;
                  end if;

                  Scan_Identifier;

               else
                  Error_Loc (1) := Token_Location;
                  DE ("invalid character");
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
         Scan_Token;

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

   procedure Skip_Spaces is
   begin
      loop
         case Buffer (Token_Location.Scan) is
            when ' '
              | HT =>
               Token_Location.Scan := Token_Location.Scan + 1;
            when LF | FF | CR | VT =>
               New_Line;
            when others =>
               exit;
         end case;
      end loop;
   end Skip_Spaces;

   -----------------------------
   -- Store_Encoded_Character --
   -----------------------------

   procedure Store_Encoded_Character (C : Natural) is

      procedure Set_Hex_Chars (N : Natural);
      --  Stores given value, which is in the range 0 .. 255, as two hex
      --  digits (using lower case a-f) in Name_Buffer, incrementing Name_Len

      procedure Set_Hex_Chars (N : Natural) is
         Hexd : constant String := "0123456789abcdef";

      begin
         Add_Char_To_Name_Buffer (Hexd (N / 16 + 1));
         Add_Char_To_Name_Buffer (Hexd (N mod 16 + 1));
      end Set_Hex_Chars;

   begin
      if C < 256 then
         Add_Char_To_Name_Buffer (Character'Val (C));

      else
         Add_Str_To_Name_Buffer ("[""");
         Set_Hex_Chars (C / 256);
         Set_Hex_Chars (C mod 256);
         Add_Str_To_Name_Buffer ("]""");
      end if;
   end Store_Encoded_Character;

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

   procedure Unexpected_Token (T : Token_Type; C : String := "") is
      Where  : constant String  := " in " & C;
      Length : Natural := 0;
   begin
      if C'Length /= 0 then
         Length := Where'Length;
      end if;
      Error_Loc (1) := Token_Location;
      DE ("unexpected " & Quoted_Image (T) & Where (1 .. Length));
   end Unexpected_Token;

end Lexer;

