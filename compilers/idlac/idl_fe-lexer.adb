------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . L E X E R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling;

with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNAT.Table;

with Idlac_Flags;
with Idl_Fe.Debug;
pragma Elaborate_All (Idl_Fe.Debug);

with Idl_Fe.Files;
with Idl_Fe.Types; use Idl_Fe.Types;

package body Idl_Fe.Lexer is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural := Idl_Fe.Debug.Is_Active ("idl_fe.lexer");
   procedure O is new Idl_Fe.Debug.Output (Flag);

   subtype Line_Type is String (1 .. 2047);
   --  Used for define identical line buffers in global variable and
   --  state table.

   subtype Idl_Token_Keyword is Idl_Token range T_Abstract .. T_Wstring;

   All_Idl_Keywords : constant array (Idl_Token_Keyword)
     of String_Cacc :=
     (T_Abstract    => new String'("abstract"),
      T_Any         => new String'("any"),
      T_Attribute   => new String'("attribute"),
      T_Boolean     => new String'("boolean"),
      T_Case        => new String'("case"),
      T_Char        => new String'("char"),
      T_Component   => new String'("component"),
      T_Const       => new String'("const"),
      T_Consumes    => new String'("consumes"),
      T_Context     => new String'("context"),
      T_Custom      => new String'("custom"),
      T_Default     => new String'("default"),
      T_Double      => new String'("double"),
      T_Emits       => new String'("emits"),
      T_Enum        => new String'("enum"),
      T_EventType   => new String'("eventtype"),
      T_Exception   => new String'("exception"),
      T_Factory     => new String'("factory"),
      T_False       => new String'("FALSE"),
      T_Finder      => new String'("finder"),
      T_Fixed       => new String'("fixed"),
      T_Float       => new String'("float"),
      T_GetRaises   => new String'("getraises"),
      T_Home        => new String'("home"),
      T_Import      => new String'("import"),
      T_In          => new String'("in"),
      T_Inout       => new String'("inout"),
      T_Interface   => new String'("interface"),
      T_Local       => new String'("local"),
      T_Long        => new String'("long"),
      T_Module      => new String'("module"),
      T_Multiple    => new String'("multiple"),
      T_Native      => new String'("native"),
      T_Object      => new String'("Object"),
      T_Octet       => new String'("octet"),
      T_Oneway      => new String'("oneway"),
      T_Out         => new String'("out"),
      T_PrimaryKey  => new String'("primarykey"),
      T_Private     => new String'("private"),
      T_Provides    => new String'("provides"),
      T_Public      => new String'("public"),
      T_Publishes   => new String'("publishes"),
      T_Raises      => new String'("raises"),
      T_Readonly    => new String'("readonly"),
      T_Sequence    => new String'("sequence"),
      T_SetRaises   => new String'("setraises"),
      T_Short       => new String'("short"),
      T_String      => new String'("string"),
      T_Struct      => new String'("struct"),
      T_Supports    => new String'("supports"),
      T_Switch      => new String'("switch"),
      T_True        => new String'("TRUE"),
      T_Truncatable => new String'("truncatable"),
      T_Typedef     => new String'("typedef"),
      T_TypeId      => new String'("typeid"),
      T_TypePrefix  => new String'("typeprefix"),
      T_Unsigned    => new String'("unsigned"),
      T_Union       => new String'("union"),
      T_Uses        => new String'("uses"),
      T_ValueBase   => new String'("ValueBase"),
      T_ValueType   => new String'("valuetype"),
      T_Void        => new String'("void"),
      T_Wchar       => new String'("wchar"),
      T_Wstring     => new String'("wstring"));

   Idl_File_Name : GNAT.OS_Lib.String_Access;
   --  Name of file in which preprocessor output is saved, and which is now
   --  processed by lexer

   Idl_File      : Ada.Text_IO.File_Type;
   --  Currently processed file

   ---------------------------------
   -- A state for pragma scanning --
   ---------------------------------

   Pragma_State : Boolean := False;
   --  True when the lexer is scanning a pragma line

   ---------------------------------
   -- Low-level string processing --
   ---------------------------------

   Current_Location : Idlac_Errors.Location;
   --  The current location in the parsed file

   Current_Token_Location : Idlac_Errors.Location;
   --  The current_token location

   Current_Line_Len : Natural;
   --  The length of the current line

   Line : Line_Type;
   --  The current line in the parsed file

   Offset : Natural;
   --  The current offset on the line (the offset is due to tabs)

   Mark_Pos : Natural;
   End_Mark_Pos : Natural;
   --  The current position of the marks in the line. The marks
   --  are used to memorize the begining and the end of an
   --  identifier for example.

   -----------------
   -- State stack --
   -----------------

   --  ??? More comments needed

   type State_Item is record
      Idl_File_Name          : GNAT.OS_Lib.String_Access;
      Current_Location       : Idlac_Errors.Location;
      Current_Token_Location : Idlac_Errors.Location;
      Current_Line_Len       : Natural;
      Line                   : Line_Type;
      Offset                 : Natural;
      TIO_Line               : Ada.Text_IO.Positive_Count;
      TIO_Col                : Ada.Text_IO.Positive_Count;
   end record;

   package State_Stack is
     new GNAT.Table
     (Table_Component_Type => State_Item,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);

   Initialized : Boolean := False;
   --  Flag to detect nested calls to Initialize. If True then lexer
   --  is already initialized, else its initialization for new file
   --  current state should be saved in state stack.

   procedure Push_State;
   --  Store lexer state in stack

   procedure Pop_State;
   --  Restore lexer state from stack

   ----------------
   -- Push_State --
   ----------------

   procedure Push_State is
   begin
      State_Stack.Append
        ((Idl_File_Name          => Idl_File_Name,
          Current_Location       => Current_Location,
          Current_Token_Location => Current_Token_Location,
          Current_Line_Len       => Current_Line_Len,
          Line                   => Line,
          Offset                 => Offset,
          TIO_Line               => Ada.Text_IO.Line (Idl_File),
          TIO_Col                => Ada.Text_IO.Col (Idl_File)));
      Ada.Text_IO.Close (Idl_File);
   end Push_State;

   ---------------
   -- Pop_State --
   ---------------

   procedure Pop_State is
      S : constant State_Item := State_Stack.Table (State_Stack.Last);
   begin
      State_Stack.Set_Last (State_Stack.Last - 1);

      Idl_File_Name          := S.Idl_File_Name;
      Current_Location       := S.Current_Location;
      Current_Token_Location := S.Current_Token_Location;
      Current_Line_Len       := S.Current_Line_Len;
      Line                   := S.Line;
      Offset                 := S.Offset;

      Ada.Text_IO.Open (Idl_File, Ada.Text_IO.In_File, Idl_File_Name.all);
      Ada.Text_IO.Set_Input (Idl_File);
      Ada.Text_IO.Set_Line  (Idl_File, S.TIO_Line);
      Ada.Text_IO.Set_Col   (Idl_File, S.TIO_Col);
   end Pop_State;

   ------------------------
   -- Set_Token_Location --
   ------------------------

   procedure Set_Token_Location is
   begin
      Current_Token_Location.Filename := Current_Location.Filename;
      Current_Token_Location.Dirname  := Current_Location.Dirname;
      Current_Token_Location.Line     := Current_Location.Line;
      Current_Token_Location.Col      := Current_Location.Col
                                           + Offset - Line'First;
   end Set_Token_Location;

   -----------------------
   -- Get_Real_Location --
   -----------------------

   function Get_Real_Location return Idlac_Errors.Location is
   begin
      pragma Debug (O ("Get_Real_Location: Line = " &
                       Natural'Image (Current_Location.Line) &
                       ", Col = " &
                       Natural'Image (Current_Location.Col +
                                      Offset - Line'First) &
                       ", Filename = " &
                       Current_Location.Filename.all));
      return (Filename => Current_Location.Filename,
              Dirname  => Current_Location.Dirname,
              Line     => Current_Location.Line,
              Col      => Current_Location.Col + Offset - Line'First);
   end Get_Real_Location;

   ---------------
   -- Read_Line --
   ---------------

   procedure Read_Line is
   begin

      --  Get next line and append LF

      Ada.Text_IO.Get_Line (Line, Current_Line_Len);
      Current_Line_Len := Current_Line_Len + 1;
      Line (Current_Line_Len) := LF;
      Current_Location.Line := Current_Location.Line + 1;
      Current_Location.Col := Line'First;
      Offset := 0;
      Mark_Pos := Current_Location.Col;
      End_Mark_Pos := Current_Location.Col;
   end Read_Line;

   ---------------
   -- Skip_Char --
   ---------------

   procedure Skip_Char is
   begin
      Current_Location.Col := Current_Location.Col + 1;
      if Current_Location.Col > Current_Line_Len then
         Read_Line;
      end if;
   end Skip_Char;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line is
   begin
      Read_Line;
      Current_Location.Col := Current_Location.Col - 1;
   end Skip_Line;

   ---------------
   -- Next_Char --
   ---------------

   function Next_Char return Character is
   begin
      Skip_Char;
      return Line (Current_Location.Col);
   end Next_Char;

   --------------------
   -- View_Next_Char --
   --------------------

   function View_Next_Char return Character is
   begin
      if Current_Location.Col = Current_Line_Len then
         return LF;
      else
         return Line (Current_Location.Col + 1);
      end if;
   end View_Next_Char;

   -------------------------
   -- View_Next_Next_Char --
   -------------------------

   function View_Next_Next_Char return Character is
   begin
      if Current_Location.Col > Current_Line_Len - 2 then
         return LF;
      else
         return Line (Current_Location.Col + 2);
      end if;
   end View_Next_Next_Char;

   ----------------------
   -- Get_Current_Char --
   ----------------------

   function Get_Current_Char return Character is
   begin
      return Line (Current_Location.Col);
   end Get_Current_Char;

   --------------------
   -- Refresh_Offset --
   --------------------

   procedure Refresh_Offset is
   begin
      Offset :=
        ((Current_Location.Col + Offset + 7) / 8) * 8 - Current_Location.Col;
   end Refresh_Offset;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces is
   begin
      loop
         case View_Next_Char is
            when Space | CR | VT | FF | HT =>
               Skip_Char;
            when others =>
               return;
         end case;
      end loop;
   end Skip_Spaces;

   ------------------
   -- Skip_Comment --
   ------------------

   procedure Skip_Comment is
   begin
      pragma Debug (O ("Skip_Comment: enter"));
      loop
         while Next_Char /= '*' loop
            null;
         end loop;
         pragma Debug (O ("Skip_Comment: '*' found"));
         while Next_Char = '*' loop
            null;
         end loop;
         pragma Debug (O ("Skip_Comment: no more '*'s"));
         exit when Get_Current_Char = '/';
      end loop;
      pragma Debug (O ("Skip_Comment: end"));
   end Skip_Comment;

   --------------
   -- Set_Mark --
   --------------

   procedure Set_Mark is
   begin
      Mark_Pos := Current_Location.Col;
      End_Mark_Pos := Mark_Pos;
   end Set_Mark;

   ---------------------------
   -- Set_Mark_On_Next_Char --
   ---------------------------

   procedure Set_Mark_On_Next_Char is
   begin
      Mark_Pos := Current_Location.Col + 1;
      End_Mark_Pos := Mark_Pos;
   end Set_Mark_On_Next_Char;

   ------------------
   -- Set_End_Mark --
   ------------------

   procedure Set_End_Mark is
   begin
      End_Mark_Pos := Current_Location.Col;
   end Set_End_Mark;

   -----------------------------------
   -- Set_End_Mark_On_Previous_Char --
   -----------------------------------

   procedure Set_End_Mark_On_Previous_Char is
   begin
      End_Mark_Pos := Current_Location.Col - 1;
   end Set_End_Mark_On_Previous_Char;

   ---------------------
   -- Get_Marked_Text --
   ---------------------

   function Get_Marked_Text return String is
   begin
      return Line (Mark_Pos .. End_Mark_Pos);
   end Get_Marked_Text;

   -----------------------
   -- Go_To_End_Of_Char --
   -----------------------

   procedure Go_To_End_Of_Char is
   begin
      while View_Next_Char /= '''
        and then View_Next_Char /= LF
      loop
         Skip_Char;
      end loop;
   end Go_To_End_Of_Char;

   -------------------------
   -- Go_To_End_Of_String --
   -------------------------

   procedure Go_To_End_Of_String is
   begin
      while View_Next_Char /= Quotation loop
         Skip_Char;
      end loop;
      Skip_Char;
   end Go_To_End_Of_String;

   -------------------------------
   -- Low-level char processing --
   -------------------------------

   -----------------------------
   -- Is_Alphabetic_Character --
   -----------------------------

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

   ------------------------
   -- Is_Digit_Character --
   ------------------------

   function Is_Digit_Character (C : Standard.Character) return Boolean is
   begin
      return C in '0' .. '9';
   end Is_Digit_Character;

   ------------------------------
   -- Is_Octal_Digit_Character --
   ------------------------------

   function Is_Octal_Digit_Character (C : Standard.Character) return Boolean is
   begin
      return C in '0' .. '7';
   end Is_Octal_Digit_Character;

   -----------------------------
   -- Is_Hexa_Digit_Character --
   -----------------------------

   function Is_Hexa_Digit_Character (C : Standard.Character) return Boolean is
   begin
      case C is
         when '0' .. '9' | 'A' .. 'F' | LC_A .. LC_F =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Hexa_Digit_Character;

   -----------------------------
   -- Is_Identifier_Character --
   -----------------------------

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
           | Low_Line
           | '0' .. '9'
           | ''' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Identifier_Character;

   ---------------------------
   -- IDL string processing --
   ---------------------------

   --------------------------
   -- Idl_Identifier_Equal --
   --------------------------

   function Idl_Identifier_Equal
     (Left, Right : String) return Ident_Equality
   is
      use GNAT.Case_Util;
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

   --------------------
   -- Is_Idl_Keyword --
   --------------------

   procedure Is_Idl_Keyword
     (S            :     String;
      Is_Escaped   :     Boolean;
      Is_A_Keyword : out Idl_Keyword_State;
      Tok          : out Idl_Token)
   is
      Result : Ident_Equality;
   begin
      for I in All_Idl_Keywords'Range loop
         Result := Idl_Identifier_Equal (S, All_Idl_Keywords (I).all);
         case Result is
            when Differ =>
               null;
            when Case_Differ =>
               if Is_Escaped then
                  Is_A_Keyword := Is_Identifier;
                  Tok := T_Error;
                  return;
               else
                  Is_A_Keyword := Bad_Case;
                  Tok := I;
                  return;
               end if;
            when Equal =>
               if Is_Escaped then
                  Is_A_Keyword := Is_Identifier;
                  Tok := T_Error;
                  return;
               else
                  Is_A_Keyword := Is_Keyword;
                  Tok := I;
                  return;
               end if;
         end case;
      end loop;
      Is_A_Keyword := Is_Identifier;
      Tok := T_Error;
   end Is_Idl_Keyword;

   --------------------------------------
   -- Scanners for chars, identifiers, --
   --  numerics, string literals, and  --
   --     preprocessor directives.     --
   --------------------------------------

   ---------------
   -- Scan_Char --
   ---------------

   function Scan_Char (Wide : Boolean) return Idl_Token is
      Result : Idl_Token;
   begin
      Set_Mark_On_Next_Char;
      if Next_Char = '\' then
         case View_Next_Char is
            when 'n' | 't' | 'v' | 'b' | 'r' | 'f'
              | 'a' | '\' | '?' | Quotation =>
               Skip_Char;
               Result := T_Lit_Char;
            when ''' =>
               if View_Next_Next_Char /= ''' then
                  Idlac_Errors.Error ("Invalid character: '\', "
                                & "it should probably be '\\'",
                                Idlac_Errors.Error,
                                Get_Real_Location);
                  Result := T_Error;
               else
                  Skip_Char;
                  Result := T_Lit_Char;
               end if;
            when '0' .. '7' =>
               Skip_Char;
               if Is_Octal_Digit_Character (View_Next_Char) then
                  Skip_Char;
               end if;
               if Is_Octal_Digit_Character (View_Next_Char) then
                  Skip_Char;
               end if;
               if Is_Octal_Digit_Character (View_Next_Char) then
                  Go_To_End_Of_Char;
                  Set_End_Mark;
                  Idlac_Errors.Error ("Too much octal digits in "
                                & "character "
                                & Get_Marked_Text
                                & ", maximum is 3 in a char "
                                & "definition",
                                Idlac_Errors.Error,
                                Get_Real_Location);
                  Result := T_Error;
               else
                  Result := T_Lit_Char;
               end if;
            when 'x' =>
               Skip_Char;
               if Is_Hexa_Digit_Character (Next_Char) then
                  if Is_Hexa_Digit_Character (View_Next_Char) then
                     Skip_Char;
                  end if;
                  if Is_Hexa_Digit_Character (View_Next_Char) then
                     Go_To_End_Of_Char;
                     Set_End_Mark;
                     Idlac_Errors.Error ("Too much hexadecimal digits "
                                   & "in character "
                                   & Get_Marked_Text
                                   & ", maximum is 2 in a char "
                                   & "definition",
                                   Idlac_Errors.Error,
                                   Get_Real_Location);
                     Result := T_Error;
                  else
                     Result := T_Lit_Char;
                  end if;
               else
                  Go_To_End_Of_Char;
                  Set_End_Mark;
                  Idlac_Errors.Error ("Invalid hexadecimal character " &
                                "code: "
                                & Get_Marked_Text,
                                Idlac_Errors.Error,
                                Get_Real_Location);
                  Result := T_Error;
               end if;
            when 'u' =>
               Skip_Char;
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
                     Go_To_End_Of_Char;
                     Set_End_Mark;
                     if Wide then
                        Idlac_Errors.Error ("Too much hexadecimal "
                                      & "digits in character "
                                      & Get_Marked_Text
                                      & ", maximum is 4 in a "
                                      & "unicode char definition",
                                      Idlac_Errors.Error,
                                      Get_Real_Location);
                     end if;
                     Result := T_Error;
                  else
                     Result := T_Lit_Char;
                  end if;
               else
                  Go_To_End_Of_Char;
                  Set_End_Mark;
                  if Wide then
                     Idlac_Errors.Error ("Invalid unicode character " &
                                   "code: "
                                   & Get_Marked_Text,
                                   Idlac_Errors.Error,
                                   Get_Real_Location);
                  end if;
                  Result := T_Error;
               end if;
               if not Wide then
                  Idlac_Errors.Error ("Unicode character is not " &
                                             "allowed in a non wide " &
                                             "character.",
                                             Idlac_Errors.Error,
                                             Get_Real_Location);
                  Result := T_Error;
               end if;
            when '8' | '9' | 'A' .. 'F' | LC_C .. LC_E =>
               Go_To_End_Of_Char;
               Set_End_Mark;
               Idlac_Errors.Error ("Invalid octal character code: "
                             & Get_Marked_Text
                             & ". For hexadecimal codes, " &
                             "use \xhh",
                             Idlac_Errors.Error,
                             Get_Real_Location);
               Result := T_Error;
            when others =>
               Go_To_End_Of_Char;
               Set_End_Mark;
               Idlac_Errors.Error ("Invalid definition of character: "
                             & Get_Marked_Text,
                             Idlac_Errors.Error,
                             Get_Real_Location);
               Result := T_Error;
         end case;
      elsif Get_Current_Char = ''' then
         if View_Next_Char = ''' then
            Idlac_Errors.Error ("Invalid character: ''', "
                          & "it should probably be '\''",
                          Idlac_Errors.Error,
                          Get_Real_Location);
            Result := T_Error;
         else
            Idlac_Errors.Error ("Invalid character: ''",
                          Idlac_Errors.Error,
                          Get_Real_Location);
            return T_Error;
         end if;
      else
         Result := T_Lit_Char;
      end if;
      Set_End_Mark;
      if Next_Char /= ''' then
         Go_To_End_Of_Char;
         Idlac_Errors.Error ("Invalid character: '"
                       & Get_Marked_Text & "'",
                       Idlac_Errors.Error,
                       Get_Real_Location);
         Result := T_Error;
      end if;
      return Result;
   end Scan_Char;

   -----------------
   -- Scan_String --
   -----------------

   function Scan_String (Wide : Boolean) return Idl_Token is
      Several_Lines : Boolean := False;
   begin
      Set_Mark_On_Next_Char;
      loop
         case Next_Char is
            when Quotation =>
               if View_Next_Char = Quotation then
                  Skip_Char;
               else
                  Set_End_Mark_On_Previous_Char;
                  return T_Lit_String;
               end if;
            when '\' =>
               case View_Next_Char is
                  when LC_N | LC_T | LC_V | LC_B | LC_R
                    | LC_F | LC_A | '\' | '?' | ''' | Quotation =>
                     Skip_Char;
                  when '0' =>
                     if Is_Octal_Digit_Character
                       (View_Next_Next_Char) then
                        Skip_Char;
                     else
                        Go_To_End_Of_String;
                        Idlac_Errors.Error
                          ("A string literal may not contain"
                           & " the character '\0'",
                           Idlac_Errors.Error,
                           Get_Real_Location);
                        return T_Error;
                     end if;
                  when '1' .. '7' =>
                     Skip_Char;
                  when LC_X =>
                     if Is_Hexa_Digit_Character
                       (View_Next_Next_Char) then
                        Skip_Char;
                     else
                        Go_To_End_Of_String;
                        Idlac_Errors.Error
                          ("bad hexadecimal character in string",
                           Idlac_Errors.Error,
                           Get_Real_Location);
                        return T_Error;
                     end if;
                  when LC_U =>
                     if Is_Hexa_Digit_Character
                       (View_Next_Next_Char) then
                        Skip_Char;
                     else
                        Go_To_End_Of_String;
                        if Wide then
                           Idlac_Errors.Error
                             ("bad unicode character in string",
                              Idlac_Errors.Error,
                              Get_Real_Location);
                        else
                           Idlac_Errors.Error
                             ("bad unicode character in string. " &
                              "Anyway, it is not allowed in a non " &
                              "wide string.",
                              Idlac_Errors.Error,
                              Get_Real_Location);
                        end if;
                        return T_Error;
                     end if;
                     if not Wide then
                        Idlac_Errors.Error
                          ("Unicode characters are not allowed " &
                           "in a non wide string.",
                           Idlac_Errors.Error,
                           Get_Real_Location);
                     end if;
                  when others =>
                     Go_To_End_Of_String;
                     Idlac_Errors.Error
                       ("bad escape sequence in string",
                        Idlac_Errors.Error,
                        Get_Real_Location);
                     return T_Error;
               end case;
            when LF =>
               if Several_Lines = False then
                  Idlac_Errors.Error
                    ("A String may not go over several lines",
                     Idlac_Errors.Error,
                     Get_Real_Location);
                  Several_Lines := True;
               end if;
            when others =>
               null;
         end case;
      end loop;
   exception
      when Ada.Text_IO.End_Error =>
         Idlac_Errors.Error ("unexpected end of file in the middle "
                       & "of a string, you probably forgot the "
                       & Quotation
                       & " at the end of a string",
                       Idlac_Errors.Fatal,
                       Get_Real_Location);

         --  Not reached

         raise Idlac_Errors.Fatal_Error;
   end Scan_String;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   function Scan_Identifier (Is_Escaped : Boolean) return Idl_Token is
      Is_A_Keyword : Idl_Keyword_State;
      Tok : Idl_Token;
   begin
      Set_Mark;
      if not Is_Escaped
        and then Get_Current_Char = 'L'
        and then View_Next_Char = ''' then
         Skip_Char;
         Set_End_Mark;
         case Scan_Char (True) is
            when T_Lit_Char =>
               return T_Lit_Wide_Char;
            when T_Error  =>
               return T_Error;
            when others =>
               raise Idlac_Errors.Internal_Error;
         end case;
      elsif not Is_Escaped
        and then Get_Current_Char = 'L'
        and then View_Next_Char = Quotation then
         Skip_Char;
         Set_End_Mark;
         case Scan_String (True) is
            when T_Lit_String =>
               return T_Lit_Wide_String;
            when T_Error  =>
               return T_Error;
            when others =>
               raise Idlac_Errors.Internal_Error;
         end case;
      else
         while Is_Identifier_Character (View_Next_Char)
         loop
            Skip_Char;
         end loop;
         Set_End_Mark;
         Is_Idl_Keyword (Get_Marked_Text,
                         Is_Escaped,
                         Is_A_Keyword,
                         Tok);
         case Is_A_Keyword is
            when Is_Keyword =>
               return Tok;
            when Is_Identifier =>
               return T_Identifier;
            when Bad_Case =>
               Idlac_Errors.Error
                 ("Bad identifier or bad case for IDL keyword.",
                  Idlac_Errors.Error,
                  Get_Real_Location);
               return Tok;
         end case;
      end if;
   end Scan_Identifier;

   ------------------
   -- Scan_Numeric --
   ------------------

   function Scan_Numeric return Idl_Token is
   begin
      Set_Mark;
      if Get_Current_Char = '0' and then View_Next_Char /= '.' then
         if View_Next_Char = 'x' or else View_Next_Char = 'X' then
            Skip_Char;
            while Is_Hexa_Digit_Character (View_Next_Char) loop
               Skip_Char;
            end loop;
            Set_End_Mark;
            return T_Lit_Hexa_Integer;
         elsif Is_Octal_Digit_Character (View_Next_Char) then
            while Is_Octal_Digit_Character (View_Next_Char) loop
               Skip_Char;
            end loop;
            Set_End_Mark;
            return T_Lit_Octal_Integer;
         elsif View_Next_Char = 'D' or else View_Next_Char = 'd'
           or else View_Next_Char = 'E' or else View_Next_Char = 'e' then
            null;
         else
            --  This is only a digit
            return T_Lit_Decimal_Integer;
         end if;
      end if;
      if Get_Current_Char /= '.' then
         while Is_Digit_Character (View_Next_Char) loop
            Skip_Char;
         end loop;
      end if;
      if Get_Current_Char /= '.' and then View_Next_Char = '.' then
         Skip_Char;
      end if;
      if Get_Current_Char = '.' then
         while Is_Digit_Character (View_Next_Char) loop
            Skip_Char;
         end loop;
         if View_Next_Char = 'D' or else View_Next_Char = 'd' then
            Skip_Char;
            Set_End_Mark;
            return T_Lit_Floating_Fixed_Point;
         elsif View_Next_Char = 'E' or else View_Next_Char = 'e' then
            Skip_Char;
            if View_Next_Char = '+' or else View_Next_Char = '-' then
               Skip_Char;
            end if;
            while Is_Digit_Character (View_Next_Char) loop
               Skip_Char;
            end loop;
            Set_End_Mark;
            return T_Lit_Exponent_Floating_Point;
         else
            Set_End_Mark;
            return T_Lit_Simple_Floating_Point;
         end if;
      elsif View_Next_Char = 'E' or else View_Next_Char = 'e' then
         Skip_Char;
         if View_Next_Char = '+' or else View_Next_Char = '-' then
            Skip_Char;
         end if;
         while Is_Digit_Character (View_Next_Char) loop
            Skip_Char;
         end loop;
         Set_End_Mark;
         return T_Lit_Pure_Exponent_Floating_Point;
      elsif View_Next_Char = 'D' or else View_Next_Char = 'd' then
         Skip_Char;
         Set_End_Mark;
         return T_Lit_Simple_Fixed_Point;
      else
         Set_End_Mark;
         return T_Lit_Decimal_Integer;
      end if;
   end Scan_Numeric;

   ---------------------
   -- Scan_Underscore --
   ---------------------

   function Scan_Underscore return Idl_Token is
   begin
      if Is_Alphabetic_Character (View_Next_Char) then
         Skip_Char;
         return Scan_Identifier (True);
      else
         Idlac_Errors.Error ("Invalid character '_' in identifier",
                       Idlac_Errors.Error, Get_Real_Location);
         return T_Error;
      end if;
   end Scan_Underscore;

   -----------------------
   -- Scan_Preprocessor --
   -----------------------

   function Scan_Preprocessor return Boolean is
      use Ada.Characters.Handling;
   begin
      Skip_Spaces;
      case View_Next_Char is
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
            --  This is a preprocessor directive
            Skip_Char;
            Set_Mark;
            while Is_Identifier_Character (View_Next_Char)
            loop
               Skip_Char;
            end loop;
            Set_End_Mark;
            if To_Lower (Get_Marked_Text) = "if"
              or else To_Lower (Get_Marked_Text) = "elif"
              or else To_Lower (Get_Marked_Text) = "else"
              or else To_Lower (Get_Marked_Text) = "endif"
              or else To_Lower (Get_Marked_Text) = "define"
              or else To_Lower (Get_Marked_Text) = "undef"
              or else To_Lower (Get_Marked_Text) = "ifdef"
              or else To_Lower (Get_Marked_Text) = "ifndef"
              or else To_Lower (Get_Marked_Text) = "include"
              or else To_Lower (Get_Marked_Text) = "error" then
               Idlac_Errors.Error
                 ("cannot handle preprocessor directive in "
                  & "lexer, please run cpp first.",
                  Idlac_Errors.Error,
                  Get_Real_Location);
               Skip_Line;
            elsif To_Lower (Get_Marked_Text) = "pragma" then
               Pragma_State := True;
               return True;
            else
               Idlac_Errors.Error
                 ("Unknown preprocessor directive: "
                  & Get_Marked_Text & ".",
                  Idlac_Errors.Error,
                  Get_Real_Location);
               Skip_Line;
            end if;
         when  '0' .. '9' =>

            --  Line number and maybe file name must be updated

            declare
               New_Line_Number : Natural;
               Last : Positive;
               package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
            begin
               Natural_IO.Get (Line (Current_Location.Col .. Line'Last),
                               New_Line_Number,
                               Last);
               Current_Location.Col := Last;

               --  GCC-4.1's C preprocessor output "built-in" section with
               --  line number 0, so we need to check and workaround this
               --  situation

               if New_Line_Number /= 0 then
                  Current_Location.Line := New_Line_Number - 1;

               else
                  Current_Location.Line := 0;
               end if;

               Skip_Spaces;
               case View_Next_Char is
                  when Quotation =>

                     --  A filename is present

                     Skip_Char;
                     Set_Mark_On_Next_Char;
                     Go_To_End_Of_String;
                     Set_End_Mark_On_Previous_Char;
                     declare
                        use GNAT.OS_Lib;
                        Text : constant String := Get_Marked_Text;
                     begin
                        if Text (Text'First) = '<'
                          and then Text (Text'Last) = '>'
                        then

                           --  This is an internal # line generated by the
                           --  GCC 3 preprocessor.

                           goto Ignore_Location;
                        end if;

                        --  Verify that the name ends with ".idl"

                        if Text'Length < 4
                          or else Text (Text'Last - 3 .. Text'Last) /= ".idl"
                        then
                           Idlac_Errors.Error
                             ("An IDL file name must have a " &
                              Ada.Characters.Latin_1.Quotation &
                              ".idl" &
                              Ada.Characters.Latin_1.Quotation &
                              " extension.",
                              Idlac_Errors.Error,
                              Get_Real_Location);
                        end if;

                        Idlac_Errors.Set_Path (Current_Location, Text);
                     end;

                  <<Ignore_Location>>
                     Skip_Spaces;
                     while View_Next_Char /= LF loop

                        --  A flag is present

                        case Next_Char is
                           when '1' | '2' | '3' | '4' =>
                              --  Expected, ignore
                              null;
                           when others =>
                              --  Unexpected preprocessor output, bail out
                              raise Idlac_Errors.Internal_Error;
                        end case;
                        Skip_Spaces;
                     end loop;
                  when LF =>
                     --  End of preprocessor directive
                     null;
                  when others =>
                     --  Unexpected preprocessor output, bail out
                     raise Idlac_Errors.Internal_Error;
               end case;
            end;
         when LF =>
            --  End of line
            return False;
         when others =>
            pragma Debug (O ("Scan_Preprocessor: bad preprocessor line"));
            Idlac_Errors.Error ("bad preprocessor line",
                                       Idlac_Errors.Error,
                                       Get_Real_Location);
            Skip_Line;
      end case;
      return False;
   end Scan_Preprocessor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Filename : String) is
      use GNAT.OS_Lib;

   begin
      if Initialized then
         Push_State;
      else
         Initialized := True;
      end if;

      if Filename'Length = 0 then
         Idlac_Errors.Error ("Missing IDL file as argument",
                       Idlac_Errors.Fatal,
                       Get_Real_Location);
         return;
      end if;
      Current_Location.Line := 0;
      Current_Location.Col := 0;
      Current_Line_Len := 0;

      Idlac_Errors.Set_Path (Current_Location, Filename);

      Idl_File_Name := new String'(Files.Preprocess_File (Filename));
      Ada.Text_IO.Open (Idl_File, Ada.Text_IO.In_File, Idl_File_Name.all);
      Ada.Text_IO.Set_Input (Idl_File);

      pragma Debug (O ("Initialize: end"));
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if Idlac_Flags.Keep_Temporary_Files then
         Ada.Text_IO.Close (Idl_File);
      else
         Ada.Text_IO.Delete (Idl_File);
      end if;

      if State_Stack.Last /= 0 then
         Pop_State;
      else
         Initialized := False;
      end if;
   end Finalize;

   --------------------
   -- Get_Next_Token --
   --------------------

   function Get_Next_Token return Idl_Token is
   begin
      loop
         case Next_Char is
            when Space | CR | VT | FF =>
               null;
            when LF =>
               if Pragma_State then
                  Pragma_State := False;
                  return T_End_Pragma;
               else
                  null;
               end if;
            when HT =>
               Refresh_Offset;
            when ';' =>
               Set_Token_Location;
               return T_Semi_Colon;
            when '{' =>
               Set_Token_Location;
               return T_Left_Cbracket;
            when '}' =>
               Set_Token_Location;
               return T_Right_Cbracket;
            when ':' =>
               Set_Token_Location;
               if View_Next_Char = ':' then
                  Skip_Char;
                  return T_Colon_Colon;
               else
                  return T_Colon;
               end if;
            when ',' =>
               Set_Token_Location;
               return T_Comma;
            when '(' =>
               Set_Token_Location;
               return T_Left_Paren;
            when ')' =>
               Set_Token_Location;
               return T_Right_Paren;
            when '=' =>
               Set_Token_Location;
               return T_Equal;
            when '|' =>
               Set_Token_Location;
               return T_Bar;
            when '^' =>
               Set_Token_Location;
               return T_Circumflex;
            when '&' =>
               Set_Token_Location;
               return T_Ampersand;
            when '<' =>
               Set_Token_Location;
               if View_Next_Char = '<' then
                  Skip_Char;
                  return T_Less_Less;
               else
                  return T_Less;
               end if;
            when '>' =>
               Set_Token_Location;
               if View_Next_Char = '>' then
                  Skip_Char;
                  return T_Greater_Greater;
               else
                  return T_Greater;
               end if;
            when '+' =>
               Set_Token_Location;
               return T_Plus;
            when '-' =>
               Set_Token_Location;
               return T_Minus;
            when '*' =>
               Set_Token_Location;
               return T_Star;
            when '/' =>
               if View_Next_Char = '/' then
                  --  Line comment
                  Skip_Line;
               elsif View_Next_Char = '*' then
                  --  Start of a multi-line comment
                  Skip_Char;
                  Skip_Comment;
               else
                  Set_Token_Location;
                  return T_Slash;
               end if;
            when '%' =>
               Set_Token_Location;
               return T_Percent;
            when '~' =>
               Set_Token_Location;
               return T_Tilde;
            when '[' =>
               Set_Token_Location;
               return T_Left_Sbracket;
            when ']' =>
               Set_Token_Location;
               return T_Right_Sbracket;
            when '0' .. '9' =>
               Set_Token_Location;
               return Scan_Numeric;
            when '.' =>
               Set_Token_Location;
               return Scan_Numeric;
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
               Set_Token_Location;
               return Scan_Identifier (False);
            when '_' =>
               Set_Token_Location;
               return Scan_Underscore;
            when ''' =>
               Set_Token_Location;
               return Scan_Char (False);
            when Quotation =>
               Set_Token_Location;
               return Scan_String (False);
            when Number_Sign =>
               Set_Token_Location;
               if Scan_Preprocessor then
                  return T_Pragma;
               end if;
            when others =>
               if Get_Current_Char >= ' ' then
                  Idlac_Errors.Error ("Invalid character '"
                                & Get_Current_Char
                                & "'",
                                Idlac_Errors.Error,
                                Get_Real_Location);
               else
                  Idlac_Errors.Error
                    ("Invalid character, ASCII code "
                     & Natural'Image (Character'Pos (Get_Current_Char)),
                     Idlac_Errors.Error,
                     Get_Real_Location);
               end if;
               return T_Error;
         end case;
      end loop;
   exception
      when Ada.Text_IO.End_Error =>
         return T_Eof;
   end Get_Next_Token;

   package body Lexer_State is

      ------------------------
      -- Get_Lexer_Location --
      ------------------------

      function Get_Lexer_Location return Idlac_Errors.Location is
      begin
         pragma Debug (O ("Get_Lexer_Location: filename is " &
                          Current_Token_Location.Filename.all));
         return Current_Token_Location;
      end Get_Lexer_Location;

      ----------------------
      -- Get_Lexer_String --
      ----------------------

      function Get_Lexer_String return String
        renames Get_Marked_Text;

   end Lexer_State;

end Idl_Fe.Lexer;
