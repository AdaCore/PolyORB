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
--  with Ada.Characters.Handling;
with Gnat.Case_Util;
with Types; use Types;
with Idents;
with Errors;

package body Tokens is
   type State_Type is record
      Line_Number : Natural;
      Line_Len : Natural;
   end record;

   Current_State : State_Type;
   Line : String (1 .. 2047);
   Col : Natural;
   Col_Offset : Natural;
   Token_Col : Natural;

   --  The current token, set by next_token.
   Current_Token : Idl_Token;

   --  If not t_error, the replacement token.
   Replacement_Token : Idl_Token;

   Current_Str_Start, Current_Str_End : Natural;

   procedure Read_Line is
   begin
      --  Get next line and append a LF at the end.
      Ada.Text_Io.Get_Line (Line, Current_State.Line_Len);
      Current_State.Line_Len := Current_State.Line_Len + 1;
      Line (Current_State.Line_Len) := Lf;
      Current_State.Line_Number := Current_State.Line_Number + 1;
      Col := Line'First;
      Col_Offset := 0;
   end Read_Line;

   procedure Initialize is
   begin
      --  Initialize current state.
      Current_State := (Line_Number => 0,
                        Line_Len => 0);
      Token_Col := Line'First;
      Current_Token := T_Error;
      Replacement_Token := T_Error;
      Read_Line;
   end Initialize;

   procedure Set_Replacement_Token (Tok : Idl_Token) is
   begin
      if Tok = T_Error or else Replacement_Token /= T_Error then
         raise Internal_Error;
      end if;
      Replacement_Token := Tok;
   end Set_Replacement_Token;

   --  Called when the current character is a letter.
   --  This procedure sets TOKEN and return.
   function Scan_Identifier return Idl_Token is
      use Idents;
      Id : Uniq_Id;
   begin
      Current_Str_Start := Col;

      --  CORBA V2.2, 3.2.3:
      --  An identifier is an arbritrarily long sequence of alphabetic, digit
      --  and underscore characters.  The first character must be an
      --  alphabetic character.  All characters are significant.
      Col := Col + 1;
      while Line (Col) in 'A' .. 'Z' or else Line (Col) in 'a' .. 'z'
        or else Line (Col) in '0' .. '9' or else Line (Col) = '_'
      loop
         Col := Col + 1;
      end loop;

      --  The identifier was scanned.
      Current_Str_End := Col - 1;
      Id := Get_Identifier (Line (Current_Str_Start .. Current_Str_End));
      if Id in Id_First_Tok .. Id_Last_Tok then
         return Idl_Token'Val (Uniq_Id'Pos (Id));
      else
         return T_Identifier;
      end if;
   end Scan_Identifier;

   --  IDL Syntax and semantics, § 3.2.5
   --  Integers Literals:
   --  An integer literal consisting of a sequence of digits is taken to be
   --  decimal (base ten), unless it begins with 0 (digit zero).  A sequence
   --  of digits starting with 0 is taken to be an octal integer (base eight).
   --  The digits 8 and 9 are not octal digits.  A sequence of digits preceded
   --  by 0x or 0X is taken to be a hexadecimal integer (base sixteen).  The
   --  hexadecimal digits include a or A through f or F with decimal values
   --  ten to through fifteen, repectively.
   --
   --
   procedure Scan_Numeric is
   begin
      Current_Str_Start := Col;
      if Line (Col) = '0' and then Line (Col + 1) /= '.' then
         if Line (Col + 1) = 'x' or Line (Col + 1) = 'X' then
            Col := Col + 2;
            while Line (Col) in '0' .. '9'
              or else Line (Col) in 'A' .. 'F'
              or else Line (Col) in 'a' .. 'f' loop
               Col := Col + 1;
            end loop;
            Current_Token := T_Lit_Integer;
         elsif Line (Col + 1) in '0' .. '7' then
            while Line (Col) in '0' .. '7' loop
               Col := Col + 1;
            end loop;
            Current_Token := T_Lit_Integer;
         else
            --  This is only a digit.
            Col := Col + 1;
            Current_Token := T_Lit_Integer;
         end if;
      else
         while Line (Col) in '0' .. '9' loop
            Col := Col + 1;
         end loop;
         if Line (Col) = '.' then
            Col := Col + 1;
            while Line (Col) in '0' .. '9' loop
               Col := Col + 1;
            end loop;
            if Line (Col) = 'D' or else Line (Col) = 'd' then
               Col := Col + 1;
               Current_Token := T_Lit_Fixed_Point;
            elsif Line (Col) = 'E' or else Line (Col) = 'e' then
               Col := Col + 1;
               if Line (Col) = '+' or else Line (Col) = '-' then
                  Col := Col + 1;
               end if;
               while Line (Col) in '0' .. '9' loop
                  Col := Col + 1;
               end loop;
               Current_Token := T_Lit_Floating_Point;
            else
               Current_Token := T_Lit_Floating_Point;
            end if;
         else
            Current_Token := T_Lit_Integer;
         end if;
      end if;
      Current_Str_End := Col - 1;
   end Scan_Numeric;

   --  Skip all spaces.
   procedure Skip_Spaces is
   begin
      loop
         case Line (Col) is
            when Space | Cr | Vt | Ff =>
               Col := Col + 1;
            when Ht =>
               Col_Offset := Col_Offset + 8 - (Col + Col_Offset) mod 8;
               Col := Col + 1;
            when others =>
               return;
         end case;
      end loop;
   end Skip_Spaces;

   procedure Scan_Preprocessor is
      S, E : Natural;
   begin
      if Line (Col) /= '#' then
         raise Internal_Error;
      end if;
      Col := Col + 1;
      Skip_Spaces;
      case Line (Col) is
         when 'A' .. 'Z' | 'a' .. 'z' =>
            --  This is a preprocessor directive
            S := Col;
            Col := Col + 1;
            while Line (Col) in 'a' .. 'z'
              or else Line (Col) in 'A' .. 'Z'
              or else Line (Col) = '_'
            loop
               Col := Col + 1;
            end loop;
            E := Col - 1;
            if Line (S .. E) = "define" or else Line (S .. E) = "if"
              or else Line (S .. E) = "ifdef" or else Line (S .. E) = "ifndef"
              or else Line (S .. E) = "undef" or else Line (S .. E) = "include"
              or else Line (S .. E) = "assert"
            then
               Errors.Display_Error
                 ("cannot handle preprocessor directive, use -p",
                  Current_State.Line_Number,
                  Col,
                  Errors.Error);
               raise Fatal_Error;
            elsif Line (S .. E) = "pragma" then
               --  Currently ignored.
               --  FIXME
               Col := Current_State.Line_Len;
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
            Col := Current_State.Line_Len;
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

      << Next_Line >> null;
      if Col > Current_State.Line_Len then
         Read_Line;
      end if;

      << Next_Char >> null;
      Skip_Spaces;
      Token_Col := Col + Col_offset;
      case Line (Col) is
         when Lf =>
            if Col /= Current_State.Line_Len then
               raise Internal_Error;
            else
               Col := Col + 1;
               goto Next_Line;
            end if;
         when Space | Cr | Vt | Ff | Ht =>
            --  Must not happen, since eat by skip_spaces.
            raise Internal_Error;
         when Exclamation =>
            --  invalid character.
            Current_Token := T_Error;
            return;
         when '(' =>
            Col := Col + 1;
            Current_Token := T_Left_Paren;
            return;
         when ')' =>
            Col := Col + 1;
            Current_Token := T_Right_Paren;
            return;
         when '*' =>
            Col := Col + 1;
            Current_Token := T_Star;
            return;
         when '+' =>
            Col := Col + 1;
            Current_Token := T_Plus;
            return;
         when '#' =>
            Scan_Preprocessor;
            goto Next_Line;
         when '/' =>
            if Line (Col + 1) = '/' then
               --  This is a line comment.
               Col := Current_State.Line_Len + 1;
               goto Next_Line;
            else
               Current_Token := T_Slash;
               Col := Col + 1;
               return;
            end if;
         when ':' =>
            Col := Col + 1;
            if Line (Col) = ':' then
               Col := Col + 1;
               Current_Token := T_Colon_Colon;
            else
               Current_Token := T_Colon;
            end if;
            return;
         when ';' =>
            Col := Col + 1;
            Current_Token := T_Semi_Colon;
            return;
         when '<' =>
            Col := Col + 1;
            if Line (Col) = '<' then
               Col := Col + 1;
               Current_Token := T_Less_Less;
            else
               Current_Token := T_Less;
            end if;
            return;
         when '>' =>
            Col := Col + 1;
            if Line (Col) = '>' then
               Col := Col + 1;
               Current_Token := T_Greater_Greater;
            else
               Current_Token := T_Greater;
            end if;
            return;
         when ',' =>
            Col := Col + 1;
            Current_Token := T_Comma;
            return;
         when '=' =>
            Col := Col + 1;
            Current_Token := T_Equal;
            return;
         when '0' .. '9' =>
            Scan_Numeric;
            return;
         when 'A' .. 'Z' | 'a' .. 'z' =>
            --  Keyword or identifier.
            Current_Token := Scan_Identifier;
            return;
         when '[' =>
            Col := Col + 1;
            Current_Token := T_Left_Sbracket;
            return;
         when ']' =>
            Col := Col + 1;
            Current_Token := T_Right_Sbracket;
            return;
         when '{' =>
            Col := Col + 1;
            Current_Token := T_Left_Cbracket;
            return;
         when '}' =>
            Col := Col + 1;
            Current_Token := T_Right_Cbracket;
            return;
         when others =>
            if Line (Col) >= ' ' then
               Errors.Display_Error ("bad characater `" & Line (Col) & "'",
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
         return Line (Current_Str_Start .. Current_Str_End);
      else
         raise Internal_Error;
      end if;
   end Get_Identifier;

   function Get_Literal return String is
   begin
      case Current_Token is
         when T_Lit_Integer | T_Lit_Floating_Point | T_Lit_Fixed_Point =>
            return Line (Current_Str_Start .. Current_Str_End);
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

