------------------------------------------------------------------------------
--                                                                          --
--                          GNATDIST COMPONENTS                             --
--                                                                          --
--                             X E _ S C A N                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--              GNATDIST is maintained by ACT Europe.                       --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Csets;
with Namet;         use Namet;
with Types;         use Types;
with Osint;         use Osint;
with Output;        use Output;
with XE;            use XE;

package body XE_Scan is

   Location : Location_Type;
   Buffer   : Source_Buffer_Ptr;
   Scan_Ptr : Source_Ptr;

   Up_To_Low : constant := Character'Pos ('A') - Character'Pos ('a');

   procedure New_Line;

   procedure Write_Token (T : Token_Type) is
   begin
      case T is
         when Tok_String_Literal =>
            Write_Str ("string literal");
         when Tok_Identifier =>
            Write_Str ("identifier");
         when Tok_Dot =>
            Write_Str (""".""");
         when Tok_Apostrophe =>
            Write_Str ("""'""");
         when Tok_Left_Paren =>
            Write_Str ("""(""");
         when Tok_Right_Paren =>
            Write_Str (""")""");
         when Tok_Comma =>
            Write_Str (""",""");
         when Tok_Colon_Equal =>
            Write_Str (""":=""");
         when Tok_Colon =>
            Write_Str (""":""");
         when Tok_Configuration =>
            Write_Str ("""configuration""");
         when Tok_Pragma =>
            Write_Str ("""pragma""");
         when Tok_Procedure =>
            Write_Str ("""procedure""");
         when Tok_Is =>
            Write_Str ("""is""");
         when Tok_In =>
            Write_Str ("""in""");
         when Tok_For =>
            Write_Str ("""for""");
         when Tok_Use =>
            Write_Str ("""use""");
         when Tok_Null =>
            Write_Str ("""null""");
         when Tok_Function =>
            Write_Str ("""function""");
         when Tok_End =>
            Write_Str ("""end""");
         when Tok_Begin =>
            Write_Str ("""begin""");
         when Tok_Arrow =>
            Write_Str ("""=>""");
         when Tok_EOF =>
            Write_Str ("end of file");
         when Tok_Semicolon =>
            Write_Str (""";""");
         when Tok_Return =>
            Write_Str ("""return""");
         when Tok_Unknown =>
            Write_Str ("");
         when Tok_Reserved =>
            raise Scanning_Error;
      end case;
   end Write_Token;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Scan_Ptr       := Scan_Ptr + 1;
      Location.Line  := Location.Line + 1;
      Location.First := Scan_Ptr;
      Location.Last  := Scan_Ptr;
   end New_Line;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File (File : in File_Name_Type) is
      Dummy : Source_Ptr;
   begin
      Read_Source_File (File, First_Source_Ptr, Dummy, Buffer);
      if Buffer = null then
         Write_Program_Name;
         Write_Str (": Cannot open file ");
         Write_Name (File);
         Write_Eol;
         raise Fatal_Error;
      else
         Scan_Ptr := Buffer.all'First;
      end if;
      Location.Line  := 1;
      Location.First := Scan_Ptr;
      Location.Last  := Scan_Ptr;
   end Load_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Set_Token ("configuration", Tok_Configuration);
      Set_Token ("pragma",        Tok_Pragma);
      Set_Token ("procedure",     Tok_Procedure);
      Set_Token ("is",            Tok_Is);
      Set_Token ("in",            Tok_In);
      Set_Token ("for",           Tok_For);
      Set_Token ("use",           Tok_Use);
      Set_Token ("function",      Tok_Function);
      Set_Token ("end",           Tok_End);
      Set_Token ("begin",         Tok_Begin);
      Set_Token ("null",          Tok_Null);
      Set_Token ("return",        Tok_Return);
      Set_Token ("mod",           Tok_Reserved);
      Set_Token ("rem",           Tok_Reserved);
      Set_Token ("new",           Tok_Reserved);
      Set_Token ("abs",           Tok_Reserved);
      Set_Token ("others",        Tok_Reserved);
      Set_Token ("delta",         Tok_Reserved);
      Set_Token ("digits",        Tok_Reserved);
      Set_Token ("range",         Tok_Reserved);
      Set_Token ("and",           Tok_Reserved);
      Set_Token ("or",            Tok_Reserved);
      Set_Token ("not",           Tok_Reserved);
      Set_Token ("abstract",      Tok_Reserved);
      Set_Token ("access",        Tok_Reserved);
      Set_Token ("aliased",       Tok_Reserved);
      Set_Token ("all",           Tok_Reserved);
      Set_Token ("array",         Tok_Reserved);
      Set_Token ("body",          Tok_Reserved);
      Set_Token ("constant",      Tok_Reserved);
      Set_Token ("do",            Tok_Reserved);
      Set_Token ("limited",       Tok_Reserved);
      Set_Token ("of",            Tok_Reserved);
      Set_Token ("out",           Tok_Reserved);
      Set_Token ("record",        Tok_Reserved);
      Set_Token ("renames",       Tok_Reserved);
      Set_Token ("reverse",       Tok_Reserved);
      Set_Token ("tagged",        Tok_Reserved);
      Set_Token ("then",          Tok_Reserved);
      Set_Token ("reverse",       Tok_Reserved);
      Set_Token ("abort",         Tok_Reserved);
      Set_Token ("accept",        Tok_Reserved);
      Set_Token ("case",          Tok_Reserved);
      Set_Token ("delay",         Tok_Reserved);
      Set_Token ("else",          Tok_Reserved);
      Set_Token ("elsif",         Tok_Reserved);
      Set_Token ("exception",     Tok_Reserved);
      Set_Token ("exit",          Tok_Reserved);
      Set_Token ("goto",          Tok_Reserved);
      Set_Token ("if",            Tok_Reserved);
      Set_Token ("raise",         Tok_Reserved);
      Set_Token ("requeue",       Tok_Reserved);
      Set_Token ("select",        Tok_Reserved);
      Set_Token ("terminate",     Tok_Reserved);
      Set_Token ("until",         Tok_Reserved);
      Set_Token ("when",          Tok_Reserved);
      Set_Token ("declare",       Tok_Reserved);
      Set_Token ("loop",          Tok_Reserved);
      Set_Token ("while",         Tok_Reserved);
      Set_Token ("entry",         Tok_Reserved);
      Set_Token ("protected",     Tok_Reserved);
      Set_Token ("task",          Tok_Reserved);
      Set_Token ("type",          Tok_Reserved);
      Set_Token ("subtype",       Tok_Reserved);
      Set_Token ("generic",       Tok_Reserved);
      Set_Token ("package",       Tok_Reserved);
      Set_Token ("private",       Tok_Reserved);
      Set_Token ("with",          Tok_Reserved);
      Set_Token ("separate",      Tok_Reserved);

   end Initialize;

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token is

      use Ascii;

      Char  : Character;
      Found : Boolean := False;

   begin

      while not Found loop

         --  Skip blank and tab characters

         while Buffer (Scan_Ptr) = ' ' or else Buffer (Scan_Ptr) = HT loop
            Scan_Ptr := Scan_Ptr + 1;
         end loop;

         --  First non-blank character

         Location.Last  := Scan_Ptr;

         Found := True;

         case Buffer (Scan_Ptr) is

            when LF | FF | CR | VT =>

               New_Line;
               Found := False;

            when '-' =>

               if Buffer (Scan_Ptr + 1) = '-' then

                  --  Comment

                  Scan_Ptr := Scan_Ptr + 2;
                  loop
                     if Buffer (Scan_Ptr) = LF or else
                        Buffer (Scan_Ptr) = FF or else
                        Buffer (Scan_Ptr) = CR or else
                        Buffer (Scan_Ptr) = VT then
                        New_Line;
                        exit;
                     elsif Buffer (Scan_Ptr) = EOF then
                        Token := Tok_EOF;
                        Scan_Ptr := Scan_Ptr + 1;
                        exit;
                     end if;
                     Scan_Ptr := Scan_Ptr + 1;
                  end loop;
                  if Token = Tok_EOF then
                     Found := True;
                  else
                     Found := False;
                  end if;

               else

                  Token := Tok_Unknown;

               end if;

            when '"' => -- "

               Name_Len := 0;
               Scan_Ptr := Scan_Ptr + 1;
               loop

                  if Buffer (Scan_Ptr) = '"' then --  "

                     --  end of string literal

                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Buffer (Scan_Ptr) /= '"'; -- "

                  elsif Buffer (Scan_Ptr) = EOF then

                     Token := Tok_Unknown;
                     raise Scanning_Error;

                  end if;

                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Buffer (Scan_Ptr);
                  Scan_Ptr := Scan_Ptr + 1;

               end loop;

               Token_Name := Name_Find;
               Token      := Tok_String_Literal;

            when 'A' .. 'Z' | 'a' .. 'z' =>

               --  Identifier

               Name_Len := 0;
               loop

                  Char := Buffer (Scan_Ptr);
                  case Char is
                     when 'A' .. 'Z' =>
                        Char := Character'Val
                          (Character'Pos (Char) - Up_To_Low);
                     when 'a' .. 'z' | '0' .. '9' | '_' =>
                        null;
                     when others =>
                        exit;
                  end case;
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Char;
                  Scan_Ptr := Scan_Ptr + 1;

               end loop;

               if Name_Len = 0 then
                  Token      := Tok_Unknown;
               else
                  Token      := Tok_Identifier;
                  Token_Name := Name_Find;
               end if;

            when ':' =>

               Scan_Ptr := Scan_Ptr + 1;
               if Buffer (Scan_Ptr) = '=' then
                  Scan_Ptr := Scan_Ptr + 1;
                  Token := Tok_Colon_Equal;
               else
                  Token := Tok_Colon;
               end if;

            when '.' =>

               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Dot;

            when '(' =>

               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Left_Paren;

            when ')' =>

               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Right_Paren;

            when ',' =>

               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Comma;

            when ';' =>

               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Semicolon;

            when ''' =>

               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Apostrophe;

            when EOF =>

               Token := Tok_EOF;

            when '=' =>

               if Buffer (Scan_Ptr + 1) = '>' then
                  Scan_Ptr := Scan_Ptr + 2;
                  Token := Tok_Arrow;
               else
                  Token := Tok_Unknown;
               end if;

            when others =>

               Token := Tok_Unknown;

         end case;

      end loop;

      if Token = Tok_Identifier then

         declare
            T : Token_Type;
         begin

            T := Get_Token (Token_Name);
            if T = Tok_Reserved then
               Write_Location (Location);
               Write_Str  ("reserved word """);
               Write_Name (Token_Name);
               Write_Str  (""" cannot be used as identifier");
               Write_Eol;
               raise Scanning_Error;
            elsif T /= Tok_Unknown then
               Token := T;
            end if;

         end;

      elsif Token = Tok_Unknown then

         Write_Location (Location);
         Write_Str ("character '");
         Write_Char (Buffer (Scan_Ptr));
         Write_Str ("' not allowed (");
         Write_Int (Character'Pos (Buffer (Scan_Ptr)));
         Write_Str (")");
         Write_Eol;
         raise Scanning_Error;

      end if;

   end Next_Token;

   ------------------------
   -- Get_Token_Location --
   ------------------------

   function Get_Token_Location return Location_Type is
   begin
      return Location;
   end Get_Token_Location;

   ------------------------
   -- Set_Token_Location --
   ------------------------

   procedure Set_Token_Location
     (Where : in Location_Type) is
   begin
      Location := Where;
      Scan_Ptr := Where.Last;
   end Set_Token_Location;

   --------------------
   -- Write_Location --
   --------------------

   procedure Write_Location
     (Where   : in Location_Type) is

      use Ascii;

   begin
      Write_Name (Configuration_File);
      Write_Str (":");
      Write_Int (Where.Line);
      Write_Str (":");
      Write_Int (Int (Where.Last - Where.First) + 1);
      Write_Str (": ");
   end Write_Location;

   --------------------
   -- Location_To_XY --
   --------------------

   procedure Location_To_XY
     (Where : in  Location_Type;
      Loc_X : out Int;
      Loc_Y : out Int) is
   begin
      Loc_X := Where.Line;
      Loc_Y := Int (Where.Last - Where.First) + 1;
   end Location_To_XY;

end XE_Scan;
