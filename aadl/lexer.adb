with Ada.Characters.Handling;
with GNAT.OS_Lib;     use GNAT.OS_Lib;

with Charset;         use Charset;
with Namet;           use Namet;
with Types;           use Types;
with Errors;          use Errors;
with Locations;       use Locations;

package body Lexer is

   use ASCII;

   Buffer        : Text_Buffer_Ptr;
   --  Once preprocessed, the idl file is loaded in Buffer and
   --  Token_Location.Scan is used to scan the source file.

   Display_Error : Boolean := True;
   Token_Image   : array (Token_Type) of Name_Id;

   procedure New_Line;
   --  Increment the line number and save the current position in the
   --  buffer in order to compute later on the column number.

   procedure New_Token
     (Token : Token_Type;
      Image : String);
   --  Compute token image and store it in Token_Image table.
   --  When Token is a reserved word, embrace its image between
   --  double quotes. Enter the lower-case form of a reserved word
   --  image into the name table and set name table byte to its
   --  token position in order to resolve it easily.

   procedure Scan_Identifier;
   --
   --  Identifiers : (AADL v0.96 12.3)
   --
   --  Syntax :
   --     identifier ::= identifier_letter { [underline] letter_or_digit }
   --     letter_or_digit ::= identifier_letter | digit
   --
   --  All characters of an identifier are significant, including any
   --  underline character. Identifiers differing only in the use of
   --  corresponding upper and lower case letters are considered the same.
   --
   --  NOTE: all characters will be converted to lower case (when possible)

   procedure Scan_Decimal_Integer_Value;
   --
   --  Scan an integer in the conventional decimal notation.
   --  This procedure is used in Scan_Numeric_Literal_Value because of two
   --  reasons:
   --     1. all numeric literal begins with a decimal integer literal
   --     2. Scan_Based_Integer_Value (Base) checks digits for validity
   --        which is more complex (so less efficient)

   procedure Scan_Based_Integer_Value (Base : Short_Short_Unsigned);
   --
   --  Scan an integer in the given base notation.
   --  This procedure checks digits for validity.
   --  Example:  1234 is an invalid number in base 4

   procedure Scan_Numeric_Literal_Value;
   --
   --  Numeric Literals : (AADL v0.96 12.4)
   --
   --  There are two kinds of numeric literals, real literals and
   --  integer literals. A real literal is a numeric literal that
   --  includes a point; an integer literal is a numeric literal
   --  without a point.

   procedure Scan_String_Literal_Value;
   --
   --  String Literals : (AADL v0.96 12.5)
   --
   --  A string_literal is formed by a sequence of graphic characters
   --  (possibly none) enclosed between two quotation marks used as
   --  string brackets.
   --  Syntax:
   --     string_lateral ::= "{string_element}"
   --     string_element ::= "" | non_quotation_mark_graphic_character

   procedure Skip_Line;
   --  Skip current line

   procedure Skip_Spaces;
   --  Skip all spaces

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
   begin
      return Get_Name_String (Token_Image (T));
   end Image;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
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

      if Token in Reserved_Word_Type then
         To_Lower (Name_Buffer (1 .. Name_Len));
         Set_Name_Table_Byte (Name_Find, Byte (Token_Type'Pos (Token)));
      end if;
   end New_Token;

   ----------------
   -- Preprocess --
   ----------------

   procedure Preprocess
     (Source : Name_Id;
      Result : out File_Descriptor)
   is
      Tmp_FDesc    : File_Descriptor;

   begin
      Get_Name_String (Source);
      Name_Buffer (Name_Len + 1) := ASCII.NUL;

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
      Close (Source_File);   --  source file is totally loaded

      --  Reset at the beginning

      Token_Location.Scan  := 1;
      Token_Location.First := 1;
      Token_Location.Last  := 1;
      Set_New_Location (Token_Location, Source_Name, 1);

      --  Enter all lexical elements in the name table

      New_Token (T_Error, "<error>");
      New_Token (T_Identifier, "<identifier>");

      New_Token (T_Quotation_Mark, """");
      New_Token (T_Number_Sign, "#");
      New_Token (T_Ampersand, "%");
      New_Token (T_Vertical_Line, "|");
      New_Token (T_Underline, "_");

      New_Token (T_Apostrophe, "'");
      New_Token (T_Left_Parenthesis, "(");
      New_Token (T_Right_Parenthesis, ")");
      New_Token (T_Comma, ",");

      New_Token (T_Plus, "+");
      New_Token (T_Minus, "-");
      New_Token (T_Multiply, "*");
      New_Token (T_Divide, "/");

      New_Token (T_Dot, ".");
      New_Token (T_Colon, ":");
      New_Token (T_Semicolon, ";");
      New_Token (T_Less_Than_Sign, "<");
      New_Token (T_Equals_Sign, "=");
      New_Token (T_Greater_Than_Sign, ">");
      New_Token (T_Left_Square_Bracket, "[");
      New_Token (T_Right_Square_Bracket, "]");
      New_Token (T_Left_Curly_Bracket, "{");
      New_Token (T_Right_Curly_Bracket, "}");

      New_Token (T_Association, "=>");
      New_Token (T_Additive_Association, "+=>");
      New_Token (T_Connection, "->");
      New_Token (T_Immediate_Connection, "->>");
      New_Token (T_Interval, "..");
      New_Token (T_Left_Step_Bracket, "-[");
      New_Token (T_Right_Step_Bracket, "]->");
      New_Token (T_Begin_Annex, "{**");
      New_Token (T_End_Annex, "**}");

      New_Token (T_Access, "access");
      New_Token (T_Annex, "annex");
      New_Token (T_Applies, "applies");
      New_Token (T_Binding, "binding");
      New_Token (T_Boolean, "boolean");
      New_Token (T_Bus, "bus");
      New_Token (T_Case, "case");
      New_Token (T_Classifier, "classifier");
      New_Token (T_Client, "client");
      New_Token (T_Component, "component");
      New_Token (T_Connections, "connections");
      New_Token (T_Constant, "constant");
      New_Token (T_Data, "data");
      New_Token (T_Delta, "delta");
      New_Token (T_Device, "device");
      New_Token (T_End, "end");
      New_Token (T_Enumeration, "enumeration");
      New_Token (T_Event, "event");
      New_Token (T_Extends, "extends");

      New_Token (T_False, "FALSE");
      New_Token (T_True, "TRUE");

      New_Token (T_Group, "group");
      New_Token (T_Implementation, "implementation");
      New_Token (T_In, "in");
      New_Token (T_Infinity, "infinity");
      New_Token (T_Inherit, "inherit");
      New_Token (T_Initial, "initial");
      New_Token (T_Integer, "integer");
      New_Token (T_Inverse, "inverse");
      New_Token (T_Is, "is");
      New_Token (T_List, "list");
      New_Token (T_Memory, "memory");
      New_Token (T_Mode, "mode");
      New_Token (T_Modes, "modes");
      New_Token (T_None, "none");
      New_Token (T_Not, "not");
      New_Token (T_Of, "of");
      New_Token (T_Or, "or");
      New_Token (T_Orless, "orless");
      New_Token (T_Ormore, "ormore");
      New_Token (T_Others, "others");
      New_Token (T_Out, "out");
      New_Token (T_Package, "package");
      New_Token (T_Port, "port");
      New_Token (T_Process, "process");
      New_Token (T_Processor, "processor");
      New_Token (T_Properties, "properties");
      New_Token (T_Property, "property");
      New_Token (T_Provides, "provides");
      New_Token (T_Public, "public");
      New_Token (T_Range, "range");
      New_Token (T_Real, "real");
      New_Token (T_Refined, "refined");
      New_Token (T_Refines, "refines");
      New_Token (T_Requires, "requires");
      New_Token (T_Server, "server");
      New_Token (T_String, "string");
      New_Token (T_Subcomponents, "subcomponents");
      New_Token (T_Subprogram, "subprogram");
      New_Token (T_System, "system");
      New_Token (T_Thread, "thread");
      New_Token (T_To, "to");
      New_Token (T_Type, "type");
      New_Token (T_Units, "units");

      New_Token (T_Real_Literal, "<real literal>");
      New_Token (T_Integer_Literal, "<integer literal>");

      New_Token (T_String_Literal, "<string literal>");
      New_Token (T_Comment, "<comment>");

      New_Token (T_EOF, "<end of file>");
   end Process;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   procedure Scan_Identifier is
      B : Byte;
   begin
      --  The first character of identifier is an alphabetic character.
      --  Buffer (Token_Location.Scan) is tested in Scan_Token before
      --  procedure call.

      Name_Len := 0;   --  initialize string buffer
      Add_Char_To_Name_Buffer (Buffer (Token_Location.Scan));
      Token_Location.Scan := Token_Location.Scan + 1;

      while Is_Identifier_Character (Buffer (Token_Location.Scan)) loop
         Add_Char_To_Name_Buffer (To_Lower (Buffer (Token_Location.Scan)));
         Token_Location.Scan := Token_Location.Scan + 1;
      end loop;

      --  check whether it is a reserved word

      B := Get_Name_Table_Byte (Name_Find);
      if B in First_Reserved_Word_Pos .. Last_Reserved_Word_Pos then
         Token := Token_Type'Val (B);
         --  Identifier_Name is not necessairy here
      else
         Token := T_Identifier;
         Identifier_Name := Name_Find;
      end if;
   end Scan_Identifier;

   ------------------------------
   -- Scan_Based_Integer_Value --
   ------------------------------

   procedure Scan_Based_Integer_Value (Base : Short_Short_Unsigned) is
      Ch    : Character;
      Size  : Integer := 0;  --  number of scanned digits
      Digit : Short_Short_Unsigned;
   begin
      Integer_Literal_Value := 0;
      Token := T_Integer_Literal;

      loop
         Ch := Ada.Characters.Handling.To_Upper (Buffer (Token_Location.Scan));
         if Ch in '0' .. '9' then
            Digit := Character'Pos (Ch) - Character'Pos ('0');
         elsif Ch in 'A' .. 'F' then
            Digit := Character'Pos (Ch) - Character'Pos ('A') + 10;
         else
            exit;
         end if;

         if Digit >= Base then
            Error_Loc (1) := Token_Location;
            De ("digit '" & Ch & "' is invalid in base " &
                Short_Short_Unsigned'Image (Base));
            Token := T_Error;
            Integer_Literal_Value := 0;
            return;
         end if;

         Size := Size + 1;
         Token_Location.Scan := Token_Location.Scan + 1;

         Integer_Literal_Value :=
           Integer_Literal_Value * LLU (Base) + LLU (Digit);
      end loop;

      if Size = 0 then
         Error_Loc (1) := Token_Location;
         DE ("invalid digit '" & Ch & "'");
         Token := T_Error;
      end if;
   end Scan_Based_Integer_Value;

   --------------------------------
   -- Scan_Decimal_Integer_Value --
   --------------------------------

   procedure Scan_Decimal_Integer_Value is
      Ch   : Character;
      Size : Integer := 0;  --  number of scanned digits
   begin
      Integer_Literal_Value := 0;
      Token := T_Integer_Literal;

      loop
         Ch := Buffer (Token_Location.Scan);
         if Ch in '0' .. '9' then
            if Size >= Max_Number_Of_Digits then
               Error_Loc (1) := Token_Location;
               DE ("too long number, digit ignored", K_Warning);
            else
               Integer_Literal_Value := Integer_Literal_Value * 10 +
                 Character'Pos (Ch) - Character'Pos ('0');
               Size := Size + 1;
            end if;
            Token_Location.Scan := Token_Location.Scan + 1;
         else
            exit;
         end if;
      end loop;

      if Size = 0 then
         Error_Loc (1) := Token_Location;
         DE ("invalid decimal digit '" & Ch & "'");
         Token := T_Error;
      end if;
   end Scan_Decimal_Integer_Value;

   --------------------------------
   -- Scan_Numeric_Literal_Value --
   --------------------------------

   procedure Scan_Numeric_Literal_Value is
   begin
      Scan_Decimal_Integer_Value;

   end Scan_Numeric_Literal_Value;

   -------------------------------
   -- Scan_String_Literal_Value --
   -------------------------------

   procedure Scan_String_Literal_Value is
      Quoted : Boolean := False;   --  a quotation mark '"' is scanned
      Ch     : Character;
   begin
      Name_Len := 0;   --  initialize string buffer
      loop
         Ch := Buffer (Token_Location.Scan);

         if Ch = '"' then
            if Quoted then
               Quoted := False;
               Add_Char_To_Name_Buffer ('"');
            else
               Quoted := True;
            end if;
            Token_Location.Scan := Token_Location.Scan + 1;
         else
            if Quoted then
               exit;   --  end of string, DO NOT increment scan position
            else
               if Ada.Characters.Handling.Is_Graphic (Ch) then
                  Add_Char_To_Name_Buffer (Ch);
               else
                  Error_Loc (1) := Token_Location;
                  DE ("non graphic character '" & Ch &
                      "' is not allowed in string lateral, ignored");
                  if Ch = EOF then
                     DE ("scanning string, end of file reached, exit");
                     exit;
                  end if;
               end if;
               Token_Location.Scan := Token_Location.Scan + 1;
            end if;
         end if;
      end loop;

      Token := T_String_Literal;
      String_Literal_Value := Name_Find;
   end Scan_String_Literal_Value;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token is
   begin
      Token := T_Error;

      while Token = T_Error loop  --  loop to ignore all invalid tokens
         Skip_Spaces;

         Token_Location.Last := Token_Location.Scan;

         case Buffer (Token_Location.Scan) is
            when LF | FF | CR | VT =>
               New_Line;

            when '"' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Scan_String_Literal_Value;

            when '#' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Number_Sign;

            when '&' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Ampersand;

            when '|' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Vertical_Line;

            when '_' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Underline;

            when ''' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Apostrophe;

            when '(' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Left_Parenthesis;

            when ')' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Parenthesis;

            when ',' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Comma;

            when '+' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '=' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  if Buffer (Token_Location.Scan) = '>' then
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Token := T_Additive_Association;
                  else
                     Error_Loc (1) := Token_Location;
                     DE ("'>' is expected for additive association '+=>'");
                  end if;
               else
                  Token := T_Plus;
               end if;

            when '-' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '-' then
                  Skip_Line; --  continue to loop
               elsif Buffer (Token_Location.Scan) = '[' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Left_Step_Bracket;
               elsif Buffer (Token_Location.Scan) = '>' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  if Buffer (Token_Location.Scan) = '>' then
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Token := T_Immediate_Connection;
                  else
                     Token := T_Connection;
                  end if;

               end if;

            when '*' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '*' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  if Buffer (Token_Location.Scan) = '}' then
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Token := T_End_Annex;
                  else
                     Error_Loc (1) := Token_Location;
                     DE ("'}' is expected for end of annex '**}'");
                  end if;
               else
                  Token := T_Multiply;
               end if;

            when '/' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Divide;

            when '.' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '.' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Interval;
               else
                  Token := T_Dot;
               end if;

            when ':' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Colon;

            when ';' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Semicolon;

            when '<' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Less_Than_Sign;

            when '=' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '>' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Association;
               else
                  Token := T_Equals_Sign;
               end if;

            when '>' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Greater_Than_Sign;

            when '[' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Left_Square_Bracket;

            when ']' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '-' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  if Buffer (Token_Location.Scan) = '>' then
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Token := T_Right_Step_Bracket;
                  else
                     Error_Loc (1) := Token_Location;
                     DE ("'>' is expected for right step bracket ']->'");
                  end if;
               else
                  Token := T_Right_Square_Bracket;
               end if;

            when '{' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '*' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  if Buffer (Token_Location.Scan) = '*' then
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Token := T_Begin_Annex;
                  else
                     Error_Loc (1) := Token_Location;
                     DE ("'*' is expected for begin of annex '{**'");
                  end if;
               else
                  Token := T_Left_Curly_Bracket;
               end if;

            when '}' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Curly_Bracket;

            when EOF =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_EOF;

            when '0' .. '9' =>
               Scan_Numeric_Literal_Value;

            when others =>
               if Is_Alphabetic_Character (Buffer (Token_Location.Scan)) then
                  Scan_Identifier;
               else
                  Error_Loc (1) := Token_Location;
                  DE ("character '" & Buffer (Token_Location.Scan) &
                      "' is invalid");
               end if;

         end case;
      end loop;

   end Scan_Token;

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
            when ' ' | HT =>
               Token_Location.Scan := Token_Location.Scan + 1;
            when LF | FF | CR | VT =>
               New_Line;
            when others =>
               exit;
         end case;
      end loop;
   end Skip_Spaces;

end Lexer;
