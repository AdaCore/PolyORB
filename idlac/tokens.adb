--  idlac: IDL to Ada compiler.
--  Copyright (C) 1999 Tristan Gingold.
--
--  emails: gingold@enst.fr
--          adabroker@adabroker.eu.org
--
--  IDLAC is free software;  you can  redistribute it and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Software
--  Foundation;  either version 2,  or (at your option) any later version.
--  IDLAC is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for  more details.  You should have  received  a copy of the GNU General
--  Public License  distributed with IDLAC;  see file COPYING.  If not, write
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston,
--  MA 02111-1307, USA.
--

with Ada.Text_Io;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gnat.Case_Util;
with Types; use Types;
with Errors;

package body Tokens is

   -----------------------------------
   --  low level string processing  --
   -----------------------------------

   type State_Type is record
      Line_Number : Natural;
      Line_Len : Natural;
   end record;

   Current_State : State_Type := (Line_Number => 0,
                                  Line_Len => 0);
   Line : String (1 .. 2047);
   Col : Natural := 2048;
   Col_Offset : Natural;
   Token_Col : Natural;
   Mark_Pos : Natural;

   --  The current token, set by next_token.
   Current_Token : Idl_Token := T_Error;

   --  If not t_error, the replacement token.
   Replacement_Token : Idl_Token := T_Error;


   --  reads the next line
   procedure Read_Line is
   begin
      --  Get next line and append a LF at the end.
      Ada.Text_Io.Get_Line (Line, Current_State.Line_Len);
      Current_State.Line_Len := Current_State.Line_Len + 1;
      Line (Current_State.Line_Len) := Lf;
      Current_State.Line_Number := Current_State.Line_Number + 1;
      Col := Line'First;
      Col_Offset := 0;
      Token_Col := Col;
      Mark_Pos := Col;
   end Read_Line;

   --  skips current char
   procedure Skip_Char is
   begin
      Col := Col + 1;
      if Col > Current_State.Line_Len then
         Read_Line;
      end if;
   end Skip_Char;

   --  skips the current line
   procedure Skip_Line is
   begin
      Read_Line;
   end Skip_Line;

   --  Gets the next char and consume it
   function Next_Char return Character is
   begin
      Skip_Char;
      return Line (Col);
   end Next_Char;

   --  returns the next char without consuming it
   --  warning : if it is the end of a line, returns
   --  LF and not the first char of the next line
   function View_Next_Char return Character is
   begin
      if Col = Current_State.Line_Len then
         return Lf;
      else
         return Line (Col + 1);
      end if;
   end View_Next_Char;

   --  returns the current char
   function Get_Current_Char return Character is
   begin
      return Line (Col);
   end Get_Current_Char;

   --  calculates the new offset of the column when a tabulation
   --  occurs
   procedure Refresh_Offset is
   begin
      Col_Offset := Col_Offset + 8 - (Col + Col_Offset) mod 8;
   end Refresh_Offset;

   --  Skips all spaces.
   --  Actually, only used in scan_preprocessor
   procedure Skip_Spaces is
   begin
      loop
         case View_Next_Char is
            when Space | Cr | Vt | Ff | Ht =>
               Skip_Char;
            when others =>
               return;
         end case;
      end loop;
   end Skip_Spaces;

   --  Skips a /* ... */ comment
   procedure Skip_Comment is
   begin
      loop
         while Next_Char /= '*' loop
            null;
         end loop;
         if Next_Char = '/' then
            return;
         end if;
      end loop;
   end Skip_Comment;

   --  Sets a mark in the text.
   --  If the line changes, the mark is replaced at the beginning
   --  of the new line
   procedure Set_Mark is
   begin
      Mark_Pos := Col;
   end Set_Mark;

   --  gets the text from the mark to the current position
   function Get_Marked_Text return String is
   begin
      return Line (Mark_Pos .. Col);
   end Get_Marked_Text;


   ---------------------------------
   --  low level char processing  --
   ---------------------------------

   function Is_Alphabetic_Character (C : Standard.Character) return Boolean is
   begin
      case C is
         when 'A' .. 'Z'
           | LC_A .. LC_Z
           | UC_A_Grave .. UC_I_Diaeresis
           | LC_A_Grave .. LC_I_Diaeresis
           | UC_N_Tilde .. UC_O_Diaeresis
           | LC_N_Tilde .. LC_O_Diaeresis
           | UC_O_Oblique_Stroke .. UC_U_Diaeresis
           | LC_O_Oblique_Stroke .. LC_U_Diaeresis
           | LC_German_Sharp_S
           | LC_Y_Diaeresis =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Alphabetic_Character;

   function Is_Digit_Character (C : Standard.Character) return Boolean is
   begin
      case C is
         when '0' .. '9' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Digit_Character;

   function Is_Octal_Digit_Character (C : Standard.Character) return Boolean is
   begin
      case C is
         when '0' .. '7' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Octal_Digit_Character;

   function Is_Hexa_Digit_Character (C : Standard.Character) return Boolean is
   begin
      case C is
         when '0' .. '9' | 'A' .. 'F' | LC_A .. LC_F =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Hexa_Digit_Character;

   function Is_Identifier_Character (C : Standard.Character) return Boolean is
   begin
      case C is
         when 'A' .. 'Z'
           | LC_A .. LC_Z
           | UC_A_Grave .. UC_I_Diaeresis
           | LC_A_Grave .. LC_I_Diaeresis
           | UC_N_Tilde .. UC_O_Diaeresis
           | LC_N_Tilde .. LC_O_Diaeresis
           | UC_O_Oblique_Stroke .. UC_U_Diaeresis
           | LC_O_Oblique_Stroke .. LC_U_Diaeresis
           | LC_German_Sharp_S
           | LC_Y_Diaeresis
           | '0' .. '9'
           | ''' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Identifier_Character;


   ---------------------------------
   --  more high level functions  --
   ---------------------------------

   procedure Set_Replacement_Token (Tok : Idl_Token) is
   begin
      if Tok = T_Error or else Replacement_Token /= T_Error then
         raise Internal_Error;
      end if;
      Replacement_Token := Tok;
   end Set_Replacement_Token;


   --  checks whether s is an Idl keyword or not
   procedure Is_Idl_Keyword (S : in String;
                             Is_A_Keyword : out Idl_Keyword_State;
                             Tok : out Idl_Token) is
      Result : Ident_Equality;
      Pos : Natural := 0;
   begin
      for I in All_Idl_Keywords'Range loop
         Pos := Pos + 1;
         Result := Idl_Identifier_Equal (S, All_Idl_Keywords (I).all);
         case Result is
            when Differ =>
               null;
            when Case_Differ =>
               Is_A_Keyword := Idl_Keyword_State (Bad_Case);
               Tok := Idl_Token'Val (Pos);
               return;
            when Equal =>
               Is_A_Keyword := Idl_Keyword_State (Is_Keyword);
               Tok := Idl_Token'Val (Pos);
               return;
         end case;
      end loop;
      Is_A_Keyword := Idl_Keyword_State (Is_Identifier);
      Tok := Idl_Token (T_Error);
      return;
   end Is_Idl_Keyword;


   --  Called when the current character is a '.
   --  This procedure sets Cureent_Token and returns.
   --  The get_marked_text function returns then the
   --  character
   --
   --  IDL Syntax and semantics, CORBA V2.3 § 3.2.5
   --
   --  Char Literals : (3.2.5.2)
   --  A character literal is one or more characters enclosed in single
   --  quotes, as in 'x'.
   --  Nongraphic characters must be represented using escape sequences as
   --  defined in Table 3-9. (escape sequences are \n, \t, \v, \b, \r, \f,
   --  \a, \\, \?, \', \", \ooo, \xhh and \uhhhh)
   --
   --  The escape \ooo consists of the backslash followed by one, two or
   --  three octal digits that are taken to specify the value of the desired
   --  character. The escape \xhh consists of the backslash followed by x
   --  followed by one or two hexadecimal digits that are taken to specify
   --  the value of the desired character.
   --
   --  The escape \uhhhh consist of a backslash followed by the character
   --  'u', followed by one, two, three or four hexadecimal digits.
   procedure Scan_Char is
   begin
      Set_Mark;
      if Next_Char = '\' then
         case Next_Char is
            when 'n' | 't' | 'v' | 'b' | 'r' | 'f'
              | 'a' | '\' | '?' | Quotation =>
               Current_Token := T_Lit_Escape_Char;
            when ''' =>
               if View_Next_Char /= ''' then
                  Errors.Display_Error ("Invalid character : '\', "
                                        & "it should probably be '\\'",
                                        Current_State.Line_Number,
                                        Col - 1,
                                        Errors.Error);
                  raise Fatal_Error;
               else
                  Current_Token := T_Lit_Escape_Char;
               end if;
            when '0' .. '7' =>
               if Is_Octal_Digit_Character (View_Next_Char) then
                  Skip_Char;
               end if;
               if Is_Octal_Digit_Character (View_Next_Char) then
                  Skip_Char;
               end if;
               if Is_Octal_Digit_Character (View_Next_Char) then
                  Errors.Display_Error ("Too much octal digits in "
                                        & "character "
                                        & Get_Marked_Text
                                        & "...', maximum is 3 in a char "
                                        & "definition",
                                        Current_State.Line_Number,
                                        Col - 1,
                                        Errors.Error);
                  raise Fatal_Error;
               end if;
               Current_Token := T_Lit_Octal_Char;
            when 'x' =>
               if Is_Hexa_Digit_Character (Next_Char) then
                  if Is_Hexa_Digit_Character (View_Next_Char) then
                     Skip_Char;
                  end if;
                  if Is_Hexa_Digit_Character (View_Next_Char) then
                     Errors.Display_Error ("Too much hexadecimal digits in "
                                           & "character "
                                           & Get_Marked_Text
                                           & "...', maximum is 2 in a char "
                                           & "definition",
                                           Current_State.Line_Number,
                                           Col - 1,
                                           Errors.Error);
                     raise Fatal_Error;
                  end if;
                  Current_Token := T_Lit_Hexa_Char;
               else
                  Errors.Display_Error ("Invalid hexadecimal character code : "
                                        & Get_Marked_Text
                                        & "...'",
                                        Current_State.Line_Number,
                                        Col - 1,
                                        Errors.Error);
                  raise Fatal_Error;
               end if;
            when 'u' =>
               if Is_Hexa_Digit_Character (Next_Char) then
                  if Is_Hexa_Digit_Character (View_Next_Char) then
                     Skip_Char;
                  end if;
                  if Is_Hexa_Digit_Character (View_Next_Char) then
                     Skip_Char;
                  end if;
                  if Is_Hexa_Digit_Character (View_Next_Char) then
                     Skip_Char;
                  end if;
                  if Is_Hexa_Digit_Character (View_Next_Char) then
                     Errors.Display_Error ("Too much hexadecimal digits in "
                                           & "character "
                                           & Get_Marked_Text
                                           & "...', maximum is 4 in a unicode "
                                           & "char definition",
                                           Current_State.Line_Number,
                                           Col - 1,
                                           Errors.Error);
                     raise Fatal_Error;
                  end if;
                  Current_Token := T_Lit_Unicode_Char;
               else
                  Errors.Display_Error ("Invalid unicode character code : "
                                        & Get_Marked_Text
                                        & "...'",
                                        Current_State.Line_Number,
                                        Col - 1,
                                        Errors.Error);
                  raise Fatal_Error;
               end if;
            when '8' | '9' | 'A' .. 'F' | LC_C .. LC_e =>
               Errors.Display_Error ("Invalid octal character code, "
                                     & Get_Marked_Text
                                     & "' for hexadecimal codes, use \xhh",
                                     Current_State.Line_Number,
                                     Col - 1,
                                     Errors.Error);
               raise Fatal_Error;
            when others =>
               Errors.Display_Error ("Invalid definition of character : "
                                     & Get_Marked_Text
                                     & "'",
                                     Current_State.Line_Number,
                                     Col - 1,
                                     Errors.Error);
               raise Fatal_Error;
         end case;
      elsif Get_Current_Char = ''' then
         if View_Next_Char = ''' then
            Errors.Display_Error ("Invalid character : ''', "
                                  & "it should probably be '\''",
                                  Current_State.Line_Number,
                                  Col - 1,
                                  Errors.Error);
            raise Fatal_Error;
         else
            Current_Token := T_Lit_Simple_Char;
            return;
         end if;
      else
         Current_Token := T_Lit_Simple_Char;
      end if;
      if Next_Char /= ''' then
         Errors.Display_Error ("Invalid character : '"
                               & Get_Current_Char
                               & "' should be '''",
                               Current_State.Line_Number,
                               Col - 1,
                               Errors.Error);
         raise Fatal_Error;
      end if;
   end Scan_Char;


   --  Called when the current character is a letter.
   --  This procedure sets TOKEN and returns.
   --  The get_marked_text function returns then the
   --  name of the identifier
   function Scan_Identifier return Idl_Token is
      Is_A_Keyword : Idl_Keyword_State;
      Tok : Idl_Token;
   begin
      Set_Mark;
      --  CORBA V2.3, 3.2.5.2
      --  Wide characters litterals have an L prefix, for example :
      --      const wchar C1 = L'X';
      if Get_Current_Char = 'L' and View_Next_Char = ''' then
         Skip_Char;
         Scan_Char;
         case Current_Token is
            when T_Lit_Simple_Char =>
               return T_Lit_Wide_Simple_Char;
            when T_Lit_Escape_Char =>
               return T_Lit_Wide_Escape_Char;
            when T_Lit_Octal_Char =>
               return T_Lit_Wide_Octal_Char;
            when T_Lit_Hexa_Char =>
               return T_Lit_Wide_Hexa_Char;
            when T_Lit_Unicode_Char =>
               return T_Lit_Wide_Unicode_Char;
            when others =>
               raise Internal_Error;
         end case;
      else
         --  CORBA V2.3, 3.2.3:
         --  An identifier is an arbritrarily long sequence of ASCII
         --  alphabetic, digit and underscore characters.  The first
         --  character must be an ASCII alphabetic character. All
         --  characters are significant.
         while Is_Identifier_Character (View_Next_Char)
         loop
            Skip_Char;
         end loop;
         --  check if it is a reserved keyword or not :
         --  CORBA V2.3, 3.2.4 :
         --  keywords must be written exactly as in the above list. Identifiers
         --  that collide with keywords (...) are illegal. For example,
         --  "boolean" is a valid keyword, "Boolean" and "BOOLEAN" are
         --  illegal identifiers.
         Is_Idl_Keyword (Get_Marked_Text,
                         Is_A_Keyword,
                         tok);
         case Is_A_Keyword is
            when Idl_Keyword_State (Is_Keyword) =>
               return Tok;
            when Idl_Keyword_State (Is_Identifier) =>
               return T_Identifier;
            when Idl_Keyword_State (Bad_Case) =>
               Errors.Display_Error ("Bad identifier or bad case"
                                     & "for idl keyword",
                                     Current_State.Line_Number,
                                     Col,
                                     Errors.Error);
               raise Fatal_Error;
         end case;
      end if;
   end Scan_Identifier;


   --  IDL Syntax and semantics, CORBA V2.3 § 3.2.5
   --
   --  Integers Literals : (3.2.5.1)
   --  An integer literal consisting of a sequence of digits is taken to be
   --  decimal (base ten), unless it begins with 0 (digit zero).  A sequence
   --  of digits starting with 0 is taken to be an octal integer (base eight).
   --  The digits 8 and 9 are not octal digits.  A sequence of digits preceded
   --  by 0x or 0X is taken to be a hexadecimal integer (base sixteen).  The
   --  hexadecimal digits include a or A through f or F with decimal values
   --  ten to through fifteen, repectively. For example, the number twelve can
   --  be written 12, 014 or 0XC
   --
   --  Floating-point literals : (3.2.5.3)
   --  A floating-point literal consists of an integer part, a decimal point,
   --  a fraction part, an e or E, and an optionnaly signed integer exponent.
   --  The integer and fraction parts both consists of a sequence of decimal
   --  (base ten) digits. Either the integer part or the fraction part (but not
   --  both may be missing; either the decimal point or the letter e (or E) and
   --  the exponent (but not both) may be missing.
   --
   --  Fixed-point literals : (3.2.5.5)
   --  A fixed-point decimal literal consists of an integer part, a decimal
   --  point, a fraction part and a d or D. The integer and fraction part both
   --  consist of a sequence of decimal (base ten) digits. Either the integer
   --  part or the fraction part (but not both) may be missing; the decimal
   --  point (but not the letter d (or D)) may be missing
   procedure Scan_Numeric is
   begin
      Set_Mark;
      if Get_Current_Char = '0' and then View_Next_Char /= '.' then
         if View_Next_Char = 'x' or View_Next_Char = 'X' then
            Skip_Char;
            while Is_Hexa_Digit_Character (View_Next_Char) loop
               Skip_Char;
            end loop;
            Current_Token := T_Lit_Hexa_Integer;
         elsif Is_Octal_Digit_Character (View_Next_Char) then
            while Is_Octal_Digit_Character (View_Next_Char) loop
               Skip_Char;
            end loop;
            Current_Token := T_Lit_Octal_Integer;
         else
            --  This is only a digit.
            Current_Token := T_Lit_Decimal_Integer;
         end if;
      else
         if Get_Current_Char /= '.' then
            while Is_Digit_Character (View_Next_Char) loop
               Skip_Char;
            end loop;
         end if;
         if Get_Current_Char /= '.' and View_Next_Char = '.' then
            Skip_Char;
         end if;
         if Get_Current_Char = '.' then
            while Is_Digit_Character (View_Next_Char) loop
               Skip_Char;
            end loop;
            if View_Next_Char = 'D' or else View_Next_Char = 'd' then
               Skip_Char;
               Current_Token := T_Lit_Floating_Fixed_Point;
            elsif View_Next_Char = 'E' or else View_Next_Char = 'e' then
               Skip_Char;
               if View_Next_Char = '+' or else View_Next_Char = '-' then
                  Skip_Char;
               end if;
               while Is_Digit_Character (View_Next_Char) loop
                  Skip_Char;
               end loop;
               Current_Token := T_Lit_Exponent_Floating_Point;
            else
               Current_Token := T_Lit_Simple_Floating_Point;
            end if;
         elsif View_Next_Char = 'E' or else View_Next_Char = 'e' then
            Skip_Char;
            if View_Next_Char = '+' or else View_Next_Char = '-' then
               Skip_Char;
            end if;
            while Is_Digit_Character (View_Next_Char) loop
               Skip_Char;
            end loop;
            Current_Token := T_Lit_Pure_Exponent_Floating_Point;
         elsif View_Next_Char = 'D' or else View_Next_Char = 'd' then
            Skip_Char;
            Current_Token := T_Lit_Simple_Fixed_Point;
         else
            Current_Token := T_Lit_Decimal_Integer;
         end if;
      end if;
   end Scan_Numeric;


   procedure Scan_Preprocessor is
   begin
      if Get_Current_Char /= '#' then
         raise Internal_Error;
      end if;
      Skip_Spaces;
      case View_Next_Char is
         when 'A' .. 'Z' | 'a' .. 'z' =>
            --  This is a preprocessor directive
            Skip_Char;
            Set_Mark;
            while View_Next_Char in 'a' .. 'z'
              or else View_Next_Char in 'A' .. 'Z'
              or else View_Next_Char = '_'
            loop
               Skip_Char;
            end loop;
            if Get_Marked_Text = "define"
              or else Get_Marked_Text = "if"
              or else Get_Marked_Text = "ifdef"
              or else Get_Marked_Text = "ifndef"
              or else Get_Marked_Text = "undef"
              or else Get_Marked_Text = "include"
              or else Get_Marked_Text = "assert"
            then
               Errors.Display_Error
                 ("cannot handle preprocessor directive, use -p",
                  Current_State.Line_Number,
                  Col,
                  Errors.Error);
               raise Fatal_Error;
            elsif Get_Marked_Text = "pragma" then
               --  Currently ignored.
               --  FIXME
               Skip_Line;
            else
               Errors.Display_Error
                 ("unknow preprocessor directive.  -p can help",
                  Current_State.Line_Number,
                  Col,
                  Errors.Error);
            end if;
         when '0' .. '9' =>
            --  This is line directive
            --  Skip it.
            --  FIXME.
            Skip_Line;
         when Lf =>
            --  This is an end of line.
            null;
         when others =>
            Errors.Display_Error ("bad preprocessor line",
                                  Current_State.Line_Number,
                                  Col,
                                  Errors.Error);
      end case;
   end Scan_Preprocessor;

   --  Get the next token.
   procedure Next_Token is
   begin
      if Replacement_Token /= T_Error then
         Current_Token := Replacement_Token;
         Replacement_Token := T_Error;
         return;
      end if;
      loop
         case Next_Char is
            when Space | Cr | Vt | Ff | Lf =>
               null;
            when Ht =>
               Refresh_Offset;
               Token_Col := Col + Col_offset;
            when Exclamation =>
               --  invalid character.
               Current_Token := T_Error;
               return;
            when '(' =>
               Current_Token := T_Left_Paren;
               return;
            when ')' =>
               Current_Token := T_Right_Paren;
               return;
            when '*' =>
               Current_Token := T_Star;
               return;
            when '+' =>
               Current_Token := T_Plus;
               return;
            when '#' =>
               Scan_Preprocessor;
               Skip_Line;
            when '/' =>
               if View_Next_Char = '/' then
                  --  This is a line comment.
                  Skip_Line;
               elsif View_Next_Char = '*' then
                  --  This is the beginning of a comment
                  Skip_Char;
                  Skip_Comment;
               else
                  Current_Token := T_Slash;
                  return;
               end if;
            when ':' =>
               if view_next_char = ':' then
                  Skip_Char;
                  Current_Token := T_Colon_Colon;
               else
                  Current_Token := T_Colon;
               end if;
               return;
            when ';' =>
               Current_Token := T_Semi_Colon;
               return;
            when '<' =>
               if View_Next_Char = '<' then
                  Skip_Char;
                  Current_Token := T_Less_Less;
               else
                  Current_Token := T_Less;
               end if;
               return;
            when '>' =>
               if View_Next_Char = '>' then
                  Skip_Char;
                  Current_Token := T_Greater_Greater;
               else
                  Current_Token := T_Greater;
               end if;
               return;
            when ',' =>
               Current_Token := T_Comma;
               return;
            when '=' =>
               Current_Token := T_Equal;
               return;
            when '0' .. '9' =>
               Scan_Numeric;
               return;
            when '.' =>
               Scan_Numeric;
               return;
            when 'A' .. 'Z'
              | LC_A .. LC_Z
              | UC_A_Grave .. UC_I_Diaeresis
              | LC_A_Grave .. LC_I_Diaeresis
              | UC_N_Tilde .. UC_O_Diaeresis
              | LC_N_Tilde .. LC_O_Diaeresis
              | UC_O_Oblique_Stroke .. UC_U_Diaeresis
              | LC_O_Oblique_Stroke .. LC_U_Diaeresis
              | LC_German_Sharp_S
              | LC_Y_Diaeresis =>
               --  Keyword or identifier.
               Current_Token := Scan_Identifier;
               return;
            when '[' =>
               Current_Token := T_Left_Sbracket;
               return;
            when ']' =>
               Current_Token := T_Right_Sbracket;
               return;
            when '{' =>
               Current_Token := T_Left_Cbracket;
               return;
            when '}' =>
               Current_Token := T_Right_Cbracket;
               return;
               --  mapping of escaped identifiers
               --  CORBA 2.3 - 3.2.3.1 :
               --  "users may lexically "escape" identifiers by prepending an
               --  underscore (_) to an identifier.
            when '_' =>
               if Is_Alphabetic_Character (View_Next_Char) then
                  Skip_Char;
                  Current_Token := Scan_Identifier;
                  if Current_Token = T_Identifier then
                     Errors.Display_Error
                       ("Invalid identifier name. An identifier cannot begin" &
                        " with '_', except if the end is an idl keyword",
                        Current_State.Line_Number,
                        Col - 1,
                        Errors.Error);
                     raise Fatal_Error;
                  else
                     Current_Token := T_Identifier;
                     return;
                  end if;
               else
                  Errors.Display_Error ("Invalid character '_'",
                                        Current_State.Line_Number,
                                        Col - 1,
                                        Errors.Error);
                  raise Fatal_Error;
               end if;
            when ''' =>
               Scan_Char;
               return;
            when others =>
               if Get_Current_Char >= ' ' then
                  Errors.Display_Error ("bad character `" & Line (Col) & "'",
                                        Current_State.Line_Number,
                                        Col,
                                        Errors.Error);
               else
                  Errors.Display_Error
                    ("bad character "
                     & Natural'Image (Character'Pos (Line (Col))),
                     Current_State.Line_Number,
                     Col,
                     Errors.Error);
               end if;
               Current_Token := T_Error;
               return;
         end case;
      end loop;
      exception
         when Ada.Text_Io.End_Error =>
            Current_Token := T_Eof;
            return;
   end Next_Token;

   function Token return Idl_Token is
   begin
      return Current_Token;
   end Token;

   function Get_Loc return Location is
   begin
      return Location'( --  Filename => null,
                       Line => Current_State.Line_Number,
                       Col => Token_Col);
   end Get_Loc;

   function Get_Identifier return String is
   begin
      if Current_Token = T_Identifier then
         return Get_Marked_Text;
      else
         raise Internal_Error;
      end if;
   end Get_Identifier;

   function Get_Literal return String is
   begin
      case Current_Token is
         when T_Lit_Decimal_Integer |
           T_Lit_Octal_Integer |
           T_Lit_Hexa_Integer |
           T_Lit_Simple_Floating_Point |
           T_Lit_Exponent_Floating_Point |
           T_Lit_Pure_Exponent_Floating_Point |
           T_Lit_Simple_Fixed_Point |
           T_Lit_Floating_Fixed_Point =>
            return Get_Marked_Text;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Literal;

   function Image (Tok : Idl_Token) return String is
   begin
      case Tok is
         when T_Identifier =>
            if Tok = Token then
               return "identifier `" & Get_Identifier & ''';
            else
               return "identifier";
            end if;
         when others =>
            return '`' & Idl_Token'Image (Token) & ''';
      end case;
   end Image;

   function Idl_Compare (Left, Right : String) return Boolean is
      use Gnat.Case_Util;
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in Left'Range loop
         if To_Lower (Left (I))
           /= To_Lower (Right (Right'First + I - Left'First))
         then
            return False;
         end if;
      end loop;
      return True;
   end Idl_Compare;

   function Idl_Identifier_Equal (Left, Right : String)
                                  return Ident_Equality is
      use Gnat.Case_Util;
   begin
      if Left'Length /= Right'Length then
         return Differ;
      end if;
      for I in Left'Range loop
         if To_Lower (Left (I))
           /= To_Lower (Right (Right'First + I - Left'First))
         then
            return Differ;
         end if;
      end loop;
      if Left /= Right then
         return Case_Differ;
      else
         return Equal;
      end if;
   end Idl_Identifier_Equal;

end Tokens;

