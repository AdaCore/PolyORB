------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  A L I                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Binderr; use Binderr;
with Butil;   use Butil;
with Debug;   use Debug;
with Fname;   use Fname;
with Namet;   use Namet;
with Opt;     use Opt;
with Osint;   use Osint;
with Output;  use Output;

package body ALI is

   use Ascii;
   --  Make control characters visible

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Accumulate_Checksum (C : Character; Csum : in out Word);
   pragma Inline (Accumulate_Checksum);
   --  This routine accumulates the checksum given character C. During the
   --  scanning of a source file, this routine is called with every character
   --  in the source, excluding blanks, and all control characters (except
   --  that ESC is included in the checksum). Upper case letters not in string
   --  literals are folded by the caller. See Sinput spec for the documentation
   --  of the checksum algorithm. Note: checksum values are only used if we
   --  generate code, so it is not necessary to worry about making the right
   --  sequence of calls in any error situation.

   -------------------------
   -- Accumulate_Checksum --
   -------------------------

   procedure Accumulate_Checksum (C : Character; Csum : in out Word) is
   begin
      Csum := Csum + Csum + Character'Pos (C);

      if Csum > 16#8000_0000# then
         Csum := (Csum + 1) and 16#7FFF_FFFF#;
      end if;
   end Accumulate_Checksum;

   -----------------------
   -- Get_File_Checksum --
   -----------------------

   function Get_File_Checksum (Fname : Name_Id) return Word is
      Src  : Source_Buffer_Ptr;
      Hi   : Source_Ptr;
      Csum : Word;
      Ptr  : Source_Ptr;

      Bad : exception;
      --  Raised if file not found, or file format error

      use Ascii;
      --  Make control characters visible

      procedure Free_Source;
      --  Free source file buffer

      procedure Free_Source is
         procedure free (Arg : Source_Buffer_Ptr);
         pragma Import (C, free, "free");

      begin
         free (Src);
      end Free_Source;

   --  Start of processing for Get_File_Checksum

   begin
      Read_Source_File (Fname, 0, Hi, Src);

      --  If we cannot find the file, then return an impossible checksum,
      --  impossible becaues checksums have the high order bit zero, so
      --  that checksums do not match.

      if Src = null then
         raise Bad;
      end if;

      Csum := 0;
      Ptr := 0;

      loop
         case Src (Ptr) is

            --  Spaces and formatting information are ignored in checksum

            when ' ' | CR | LF | VT | FF | HT =>
               Ptr := Ptr + 1;

            --  EOF is ignored unless it is the last character

            when EOF =>
               if Ptr = Hi then
                  Free_Source;
                  return Csum;
               else
                  Ptr := Ptr + 1;
               end if;

            --  Non-blank characters that are included in the checksum

            when '#' | '&' | '*' | ':' | '(' | ',' | '.' | '=' | '>' |
                 '<' | ')' | '/' | ';' | '|' | '!' | '+' | '_' |
                 '0' .. '9' | 'a' .. 'z'
            =>
               Accumulate_Checksum (Src (Ptr), Csum);
               Ptr := Ptr + 1;

            --  Upper case letters, fold to lower case

            when 'A' .. 'Z' =>
               Accumulate_Checksum
                 (Character'Val (Character'Pos (Src (Ptr)) + 32), Csum);
               Ptr := Ptr + 1;

            --  Left bracket, really should do wide character thing here,
            --  but for now, don't bother.

            when '[' =>
               raise Bad;

            --  Minus, could be comment

            when '-' =>
               if Src (Ptr + 1) = '-' then
                  Ptr := Ptr + 2;

                  while Src (Ptr) >= ' ' or else Src (Ptr) = HT loop
                     Ptr := Ptr + 1;
                  end loop;

               else
                  Accumulate_Checksum ('-', Csum);
                  Ptr := Ptr + 1;
               end if;

            --  String delimited by double quote

            when '"' =>
               Accumulate_Checksum ('"', Csum);

               loop
                  Ptr := Ptr + 1;
                  exit when Src (Ptr) = '"';

                  if Src (Ptr) < ' ' then
                     raise Bad;
                  end if;

                  Accumulate_Checksum (Src (Ptr), Csum);
               end loop;

               Accumulate_Checksum ('"', Csum);
               Ptr := Ptr + 1;

            --  String delimited by percent

            when '%' =>
               Accumulate_Checksum ('%', Csum);

               loop
                  Ptr := Ptr + 1;
                  exit when Src (Ptr) = '%';

                  if Src (Ptr) < ' ' then
                     raise Bad;
                  end if;

                  Accumulate_Checksum (Src (Ptr), Csum);
               end loop;

               Accumulate_Checksum ('%', Csum);
               Ptr := Ptr + 1;

            --  Quote, could be character constant

            when ''' =>
               Accumulate_Checksum (''', Csum);

               if Src (Ptr + 2) = ''' then
                  Accumulate_Checksum (Src (Ptr + 1), Csum);
                  Accumulate_Checksum (''', Csum);
                  Ptr := Ptr + 3;

               --  Otherwise assume attribute char. We should deal with wide
               --  character cases here, but that's hard, so forget it.

               else
                  Ptr := Ptr + 1;
               end if;

            --  Upper half character, more to be done here, we should worry
            --  about folding Latin-1, folding other character sets, and
            --  dealing with the nasty case of upper half wide encoding.

            when Upper_Half_Character =>
               Accumulate_Checksum (Src (Ptr), Csum);
               Ptr := Ptr + 1;

            --  Escape character, we should do the wide character thing here,
            --  but for now, do not bother.

            when ESC =>
               raise Bad;

            --  Invalid control characters

            when NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS  | SO  |
                 SI  | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN |
                 EM  | FS  | GS  | RS  | US  | DEL
            =>
               raise Bad;

            --  Invalid graphic characters

            when '$' | '?' | '@' | '`' | '\' |
                 '^' | '~' | ']' | '{' | '}'
            =>
               raise Bad;

         end case;
      end loop;

   exception
      when Bad =>
         Free_Source;
         return 16#FFFF_FFFF#;

   end Get_File_Checksum;

   --------------------
   -- Initialize_ALI --
   --------------------

   procedure Initialize_ALI is
   begin
      --  When (re)initializing ALI data structures the ALI user expects to
      --  get a fresh set of data structures. Thus we first need to erase the
      --  marks put in the name table by the previous set of ALI routine calls.
      --  These loops are empty and harmless the first time in.

      for J in ALIs.First .. ALIs.Last loop
         Set_Name_Table_Info (ALIs.Table (J).Afile, 0);
      end loop;

      for J in Source.First .. Source.Last loop
         Set_Name_Table_Info (Source.Table (J).Sfile, 0);
         Source.Table (J).Source_Found := False;
      end loop;

      ALIs.Init;
      Unit.Init;
      Withs.Init;
      Sdep.Init;
      Source.Init;
      Linker_Options.Init;
   end Initialize_ALI;

   --------------
   -- Read_ALI --
   --------------

   procedure Read_ALI (Id : ALI_Id) is
      Afile  : File_Name_Type;
      Text   : Text_Buffer_Ptr;
      Idread : ALI_Id;

   begin
      for I in ALIs.Table (Id).First_Unit .. ALIs.Table (Id).Last_Unit loop
         for J in Unit.Table (I).First_With .. Unit.Table (I).Last_With loop

            Afile := Withs.Table (J).Afile;

            --  Only process if not a generic (Afile /= No_File) and if
            --  file has not been processed already.

            if Afile /= No_File and then Get_Name_Table_Info (Afile) = 0 then

               Text := Read_Library_Info (Afile);

               if Text = null then
                  Error_Msg_Name_1 := Afile;
                  Error_Msg_Name_2 := Withs.Table (J).Sfile;
                  Error_Msg ("% not found, % must be compiled");
                  Set_Name_Table_Info (Afile, Int (No_Unit_Id));
                  return;
               end if;

               Idread := Scan_ALI (Afile, Text);

               if ALIs.Table (Idread).No_Object then
                  Error_Msg_Name_1 := Withs.Table (J).Sfile;
                  Error_Msg ("% must be recompiled");
                  Set_Name_Table_Info (Afile, Int (No_Unit_Id));
               end if;

               --  Recurse to get new dependents

               Read_ALI (Idread);
            end if;
         end loop;
      end loop;

   end Read_ALI;

   --------------
   -- Scan_ALI --
   --------------

   function Scan_ALI (F : File_Name_Type; T : Text_Buffer_Ptr) return ALI_Id is
      P    : Text_Ptr := T'First;
      Line : Logical_Line_Number := 1;
      Id   : ALI_Id;
      C    : Character;

      function At_Eol return Boolean;
      --  Test if at end of line

      function At_End_Of_Field return Boolean;
      --  Test if at end of line, or if at blank or horizontal tab

      procedure Check_At_End_Of_Field;
      --  Check if we are at end of field, fatal error if not

      procedure Checkc (C : Character);
      --  Check next character is C. If so bump past it, if not fatal error

      procedure Fatal_Error;
      --  Generate fatal error message for badly formatted ALI file

      function Getc return Character;
      --  Get next character, bumping P past the character obtained

      function Get_Name return Name_Id;
      --  Skip blanks, then scan out a name (name is left in Name_Buffer with
      --  length in Name_Len, as well as being returned in Name_Id form). The
      --  name is adjusted appropriately if it refers to a file that is to be
      --  substituted by another name as a result of a configuration pragma.

      function Get_Nat return Nat;
      --  Skip blanks, then scan out an unsigned integer value in Nat range

      function Get_Stamp return Time_Stamp_Type;
      --  Skip blanks, then scan out a time stamp

      function Nextc return Character;
      --  Return current character without modifying pointer P

      procedure Skip_Eol;
      --  Skip past end of line (fatal error if not at end of line)

      procedure Skip_Space;
      --  Skip past white space (blanks or horizontal tab)

      ------------
      -- At_Eol --
      ------------

      function At_Eol return Boolean is
      begin
         return Nextc = EOF or else Nextc = CR or else Nextc = LF;
      end At_Eol;

      ---------------------
      -- At_End_Of_Field --
      ---------------------

      function At_End_Of_Field return Boolean is
      begin
         return Nextc <= ' ';
      end At_End_Of_Field;

      ---------------------------
      -- Check_At_End_Of_Field --
      ---------------------------

      procedure Check_At_End_Of_Field is
      begin
         if not At_End_Of_Field then
            Fatal_Error;
         end if;
      end Check_At_End_Of_Field;

      ------------
      -- Checkc --
      ------------

      procedure Checkc (C : Character) is
      begin
         if Nextc = C then
            P := P + 1;
         else
            Fatal_Error;
         end if;
      end Checkc;

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error is
         Ptr1 : Text_Ptr;
         Ptr2 : Text_Ptr;
         Col  : Int;

         procedure Wchar (C : Character);
         --  Write a single character, replacing horizontal tab by spaces

         procedure Wchar (C : Character) is
         begin
            if C = HT then
               loop
                  Wchar (' ');
                  exit when Col mod 8 = 0;
               end loop;

            else
               Write_Char (C);
               Col := Col + 1;
            end if;
         end Wchar;

      --  Start of processing for Fatal_Error

      begin
         Write_Str ("fatal error: file ");
         Write_Name (F);
         Write_Str (" is incorrectly formatted");
         Write_Eol;
         Write_Str
           ("make sure you are using consistent versions of gcc/gnatbind");
         Write_Eol;

         --  Find start of line

         Ptr1 := P;

         while Ptr1 > T'First
           and then T (Ptr1 - 1) /= CR
           and then T (Ptr1 - 1) /= LF
         loop
            Ptr1 := Ptr1 - 1;
         end loop;

         Write_Int (Int (Line));
         Write_Str (". ");

         if Line < 100 then
            Write_Char (' ');
         end if;

         if Line < 10 then
            Write_Char (' ');
         end if;

         Col := 0;
         Ptr2 := Ptr1;

         while Ptr2 < T'Last
           and then T (Ptr2) /= CR
           and then T (Ptr2) /= LF
         loop
            Wchar (T (Ptr2));
            Ptr2 := Ptr2 + 1;
         end loop;

         Write_Eol;

         Write_Str ("     ");
         Col := 0;

         while Ptr1 < P loop
            if T (Ptr1) = HT then
               Wchar (HT);
            else
               Wchar (' ');
            end if;

            Ptr1 := Ptr1 + 1;
         end loop;

         Wchar ('|');
         Write_Eol;

         Exit_Program (E_Fatal);
      end Fatal_Error;

      ----------
      -- Getc --
      ----------

      function Getc return Character is
      begin
         if P = T'Last then
            return EOF;
         else
            P := P + 1;
            return T (P - 1);
         end if;
      end Getc;

      --------------
      -- Get_Name --
      --------------

      function Get_Name return Name_Id is
      begin
         Name_Len := 0;
         Skip_Space;

         if At_Eol then
            Fatal_Error;
         end if;

         loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Getc;
            exit when At_End_Of_Field;
         end loop;

         return Name_Find;
      end Get_Name;

      -------------
      -- Get_Nat --
      -------------

      function Get_Nat return Nat is
         V : Nat;

      begin
         V := 0;

         loop
            V := V * 10 + (Character'Pos (Getc) - Character'Pos ('0'));
            exit when At_End_Of_Field;
         end loop;

         return V;
      end Get_Nat;

      ---------------
      -- Get_Stamp --
      ---------------

      function Get_Stamp return Time_Stamp_Type is
         T     : Time_Stamp_Type;
         Start : Integer;

      begin
         Skip_Space;

         if At_Eol then
            Fatal_Error;
         end if;

         --  Following reads old style time stamp missing first two digits

         if Nextc in '7' .. '9' then
            T (1) := '1';
            T (2) := '9';
            Start := 3;

         --  Normal case of full year in time stamp

         else
            Start := 1;
         end if;

         for J in Start .. T'Last loop
            T (J) := Getc;
         end loop;

         return T;
      end Get_Stamp;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
      begin
         return T (P);
      end Nextc;

      --------------
      -- Skip_Eol --
      --------------

      procedure Skip_Eol is
      begin
         Skip_Space;
         if not At_Eol then Fatal_Error; end if;

         --  Loop to skip past blank lines (first time through skips this EOL)

         while Nextc < ' ' and then Nextc /= EOF loop
            if Nextc = LF then
               Line := Line + 1;
            end if;

            P := P + 1;
         end loop;
      end Skip_Eol;

      ----------------
      -- Skip_Space --
      ----------------

      procedure Skip_Space is
      begin
         while Nextc = ' ' or else Nextc = HT loop
            P := P + 1;
         end loop;
      end Skip_Space;

   --------------------------------------
   -- Start of processing for Scan_ALI --
   --------------------------------------

   begin
      ALIs.Increment_Last;
      Id := ALIs.Last;
      Set_Name_Table_Info (F, Int (Id));

      ALIs.Table (Id).Afile := F;
      ALIs.Table (Id).Ofile_Full_Name      := Full_Object_File_Name;
      ALIs.Table (Id).First_Unit           := No_Unit_Id;
      ALIs.Table (Id).Main_Priority        := -1;
      ALIs.Table (Id).Time_Slice_Value     := -1;
      ALIs.Table (Id).Main_Program         := None;
      ALIs.Table (Id).No_Object            := False;
      ALIs.Table (Id).Unit_Exception_Table := False;
      ALIs.Table (Id).Zero_Cost_Exceptions := False;

      --  Acquire library version

      Checkc ('V');
      Checkc (' ');
      Checkc ('"');

      for J in ALIs.Table (Id).Ver'Range loop
         ALIs.Table (Id).Ver (J) := Getc;
      end loop;

      Checkc ('"');
      Skip_Eol;

      --  Ignore Standard version line if present (allows new binder to read
      --  the old format gnatbind files, which used the 'S' line to identify
      --  the version of Standard, but this feature never proved useful)

      if Nextc = 'S' then
         while not At_Eol loop
            C := Getc;
         end loop;

         Skip_Eol;
      end if;

      --  Acquire main program line if present

      C := Getc;

      if C = 'M' then
         Checkc (' ');

         C := Getc;

         if C = 'F' then
            ALIs.Table (Id).Main_Program := Func;
         elsif C = 'P' then
            ALIs.Table (Id).Main_Program := Proc;
         else
            P := P - 1;
            Fatal_Error;
         end if;

         Skip_Space;

         if not At_Eol then
            if Nextc < 'A' then
               ALIs.Table (Id).Main_Priority := Get_Nat;
            end if;

            Skip_Space;

            if not At_Eol then
               Checkc ('T');
               Checkc ('=');
               ALIs.Table (Id).Time_Slice_Value := Get_Nat;
            end if;
         end if;

         Skip_Eol;
         C := Getc;

      end if;

      --  Skip argument lines

      Arg_Loop : while C = 'A' loop
         while not At_Eol loop
            C := Getc;
         end loop;

         Skip_Eol;
         C := Getc;
      end loop Arg_Loop;

      --  Check for obsolete Q line (queuing policy in 3.07 and previous)

      if C = 'Q' then
         Fatal_Error;
      end if;

      --  Acquire float format line if present

      if C = 'F' then
         Checkc (' ');
         Skip_Space;
         Float_Format := 'V';
         ALIs.Table (Id).Float_Format := Getc;
         Skip_Eol;
         C := Getc;

      --  Else set default IEEE format

      else
         ALIs.Table (Id).Float_Format := 'I';
      end if;

      --  Acquire tasking policy line if present

      ALIs.Table (Id).Queuing_Policy          := ' ';
      ALIs.Table (Id).Locking_Policy          := ' ';
      ALIs.Table (Id).Task_Dispatching_Policy := ' ';

      if C = 'P' then
         while not At_Eol loop
            Checkc (' ');

            case Getc is

               when 'L' =>
                  Checkc ('=');
                  Locking_Policy := Getc;
                  ALIs.Table (Id).Locking_Policy := Locking_Policy;

               when 'Q' =>
                  Checkc ('=');
                  Queuing_Policy := Getc;
                  ALIs.Table (Id).Queuing_Policy := Queuing_Policy;

               when 'T' =>
                  Checkc ('=');
                  Task_Dispatching_Policy := Getc;
                  ALIs.Table (Id).Task_Dispatching_Policy :=
                    Task_Dispatching_Policy;

               when others =>
                  Fatal_Error;

            end case;
         end loop;

         Skip_Eol;
         C := Getc;
      end if;

      --  Loop to acquire unit entries

      Unit_Loop : while C = 'U' loop
         Checkc (' ');
         Unit.Increment_Last;

         if ALIs.Table (Id).First_Unit = No_Unit_Id then
            ALIs.Table (Id).First_Unit := Unit.Last;
         end if;

         Unit.Table (Unit.Last).Uname          := Get_Name;
         Unit.Table (Unit.Last).Predefined     := Is_Predefined_Unit;
         Unit.Table (Unit.Last).My_ALI         := Id;
         Unit.Table (Unit.Last).Sfile          := Get_Name;
         Unit.Table (Unit.Last).Pure           := False;
         Unit.Table (Unit.Last).Preelab        := False;
         Unit.Table (Unit.Last).No_Elab        := False;
         Unit.Table (Unit.Last).Shared_Passive := False;
         Unit.Table (Unit.Last).RCI            := False;
         Unit.Table (Unit.Last).Remote_Types   := False;
         Unit.Table (Unit.Last).Has_RACW_Type  := False;
         Unit.Table (Unit.Last).Is_Generic     := False;
         Unit.Table (Unit.Last).Icasing        := Mixed_Case;
         Unit.Table (Unit.Last).Kcasing        := All_Lower_Case;
         Unit.Table (Unit.Last).Elaborate_Body := False;
         Unit.Table (Unit.Last).Version        := "00000000";
         Unit.Table (Unit.Last).First_With     := Withs.Last + 1;
         Unit.Table (Unit.Last).Elab_Position  := 0;

         if Debug_Flag_U then
            Write_Str (" ----> reading unit ");
            Write_Unit_Name (Unit.Table (Unit.Last).Uname);
            Write_Str (" from file ");
            Write_Name (Unit.Table (Unit.Last).Sfile);
            Write_Eol;
         end if;

         --  Check for duplicated unit in different files

         declare
            Info : constant Int := Get_Name_Table_Info
                                     (Unit.Table (Unit.Last).Uname);
         begin
            if Info /= 0
              and then Unit.Table (Unit.Last).Sfile /=
                       Unit.Table (Unit_Id (Info)).Sfile
            then
               Error_Msg ("duplicate unit name");
               Error_Msg_Name_1 := Unit.Table (Unit.Last).Uname;
               Error_Msg_Name_2 := Unit.Table (Unit.Last).Sfile;
               Error_Msg ("unit & found in file %");
               Error_Msg_Name_1 := Unit.Table (Unit_Id (Info)).Uname;
               Error_Msg_Name_2 := Unit.Table (Unit_Id (Info)).Sfile;
               Error_Msg ("unit & found in file %");
               raise Unrecoverable_Error;
            end if;
         end;

         Set_Name_Table_Info (Unit.Table (Unit.Last).Uname, Int (Unit.Last));

         --  Scan out possible version and other parameters

         loop
            Skip_Space;
            exit when At_Eol;
            C := Getc;

            --  Version field

            if C in '0' .. '9' or else C in 'a' .. 'f' then
               Unit.Table (Unit.Last).Version (1) := C;

               for J in 2 .. 8 loop
                  C := Getc;
                  Unit.Table (Unit.Last).Version (J) := C;
               end loop;

            --  EB parameter (elaborate body)

            elsif C = 'E' then
               Checkc ('B');
               Check_At_End_Of_Field;
               Unit.Table (Unit.Last).Elaborate_Body := True;

            --  GE parameter (generic)

            elsif C = 'G' then
               Checkc ('E');
               Check_At_End_Of_Field;
               Unit.Table (Unit.Last).Is_Generic := True;

            --  IL/IU parameters

            elsif C = 'I' then
               C := Getc;

               if C = 'L' then
                  Unit.Table (Unit.Last).Icasing := All_Lower_Case;

               elsif C = 'U' then
                  Unit.Table (Unit.Last).Icasing := All_Upper_Case;

               else
                  Fatal_Error;
               end if;

               Check_At_End_Of_Field;

            --  KM/KU parameters

            elsif C = 'K' then
               C := Getc;

               if C = 'M' then
                  Unit.Table (Unit.Last).Kcasing := Mixed_Case;

               elsif C = 'U' then
                  Unit.Table (Unit.Last).Kcasing := All_Upper_Case;

               else
                  Fatal_Error;
               end if;

               Check_At_End_Of_Field;

            --  NE/NO parameters

            elsif C = 'N' then
               C := Getc;

               --  NE parameter (no elaboration)

               if C = 'E' then
                  Check_At_End_Of_Field;
                  Unit.Table (Unit.Last).No_Elab := True;

               --  NO parameter (No object file)

               --  Note: this is really a file wide option, but for
               --  convenience it is given as a unit attribute, but in
               --  the ALI data tables, it is the ALI file entry that
               --  is flagged, not the unit entry.

               elsif C = 'O' then
                  Check_At_End_Of_Field;
                  ALIs.Table (Id).No_Object := True;
                  No_Object := True;

               else
                  Fatal_Error;
               end if;

            --  PR/PU/PK parameters

            elsif C = 'P' then
               C := Getc;

               --  PR parameter (preelaborate)

               if C = 'R' then
                  Check_At_End_Of_Field;
                  Unit.Table (Unit.Last).Preelab := True;

               --  PU parameter (pure)

               elsif C = 'U' then
                  Check_At_End_Of_Field;
                  Unit.Table (Unit.Last).Pure := True;

               --  PK indicates unit is package

               elsif C = 'K' then
                  Unit.Table (Unit.Last).Unit_Kind := 'p';
                  Check_At_End_Of_Field;

               else
                  Fatal_Error;
               end if;

            --  RC/RT parameters

            elsif C = 'R' then
               C := Getc;

               --  RC parameter (remote call interface)

               if C = 'C' then
                  Check_At_End_Of_Field;
                  Unit.Table (Unit.Last).RCI := True;

               --  RT parameter (remote types)

               elsif C = 'T' then
                  Check_At_End_Of_Field;
                  Unit.Table (Unit.Last).Remote_Types := True;

               --  RA parameter (remote access to class wide type)

               elsif C = 'A' then
                  Check_At_End_Of_Field;
                  Unit.Table (Unit.Last).Has_RACW_Type := True;

               else
                  Fatal_Error;
               end if;

            elsif C = 'S' then
               C := Getc;

               --  SP parameter (shared passive)

               if C = 'P' then
                  Check_At_End_Of_Field;
                  Unit.Table (Unit.Last).Shared_Passive := True;

               --  SU parameter indicates unit is subprogram

               elsif C = 'U' then
                  Unit.Table (Unit.Last).Unit_Kind := 's';
                  Check_At_End_Of_Field;

               else
                  Fatal_Error;
               end if;

            --  UX parameter (unit exception table generated)

            --  Note: this is really a file wide option, but for convenience
            --  it is given as a unit attribute, but in the ALI data tables,
            --  it is the ALI file entry that is flagged, not the unit entry.

            elsif C = 'U' then
               Checkc ('X');
               Check_At_End_Of_Field;
               ALIs.Table (Id).Unit_Exception_Table := True;

            --  ZX parameter (zero cost exceptions)

            --  Note: this is really a file wide option, but for convenience
            --  it is given as a unit attribute, but in the ALI data tables,
            --  it is the ALI file entry that is flagged, not the unit entry.

            elsif C = 'Z' then
               Checkc ('X');
               Check_At_End_Of_Field;
               ALIs.Table (Id).Zero_Cost_Exceptions := True;
               Zero_Cost_Exceptions := True;

            else
               Fatal_Error;
            end if;

         end loop;

         Skip_Eol;

         --  Scan out With lines for this unit

         C := Getc;

         With_Loop : while C = 'W' loop
            Checkc (' ');
            Withs.Increment_Last;
            Withs.Table (Withs.Last).Uname              := Get_Name;
            Withs.Table (Withs.Last).Elaborate          := False;
            Withs.Table (Withs.Last).Elaborate_All      := False;
            Withs.Table (Withs.Last).Elab_All_Desirable := False;

            --  Generic case with no object file available

            if At_Eol then
               Withs.Table (Withs.Last).Sfile := No_File;
               Withs.Table (Withs.Last).Afile := No_File;

            --  Normal case

            else
               Withs.Table (Withs.Last).Sfile := Get_Name;
               Withs.Table (Withs.Last).Afile := Get_Name;

               --  Scan out possible E, EA, and NE parameters

               while not At_Eol loop
                  Skip_Space;

                  if Nextc = 'E' then
                     P := P + 1;

                     if At_End_Of_Field then
                        Withs.Table (Withs.Last).Elaborate := True;

                     elsif Nextc = 'A' then
                        P := P + 1;
                        Check_At_End_Of_Field;
                        Withs.Table (Withs.Last).Elaborate_All := True;

                     else
                        Checkc ('D');
                        Check_At_End_Of_Field;

                        --  ED is ignored if full elaboration semantics forced
                        --  and also in horrible elaboration order mode.

                        if not Full_Elaboration_Semantics
                          and then not Horrible_Elab_Order
                        then
                           Withs.Table (Withs.Last).Elab_All_Desirable := True;
                        end if;
                     end if;
                  end if;
               end loop;
            end if;

            Skip_Eol;
            C := Getc;

         end loop With_Loop;

         Unit.Table (Unit.Last).Last_With := Withs.Last;

      end loop Unit_Loop;

      --  End loop through units for one ALI file

      ALIs.Table (Id).Last_Unit := Unit.Last;
      ALIs.Table (Id).Sfile := Unit.Table (ALIs.Table (Id).First_Unit).Sfile;

      --  Set types of the units (there can be at most 2 of them)

      if ALIs.Table (Id).First_Unit /= ALIs.Table (Id).Last_Unit then
         Unit.Table (ALIs.Table (Id).First_Unit).Utype := Is_Body;
         Unit.Table (ALIs.Table (Id).Last_Unit).Utype  := Is_Spec;

      else
         --  Deal with body only and spec only cases, note that the reason we
         --  do our own checking of the name (rather than using Is_Body_Name)
         --  is that Uname drags in far too much compiler junk!

         Get_Name_String (Unit.Table (Unit.Last).Uname);

         if Name_Buffer (Name_Len) = 'b' then
            Unit.Table (Unit.Last).Utype := Is_Body_Only;
         else
            Unit.Table (Unit.Last).Utype := Is_Spec_Only;
         end if;
      end if;

      --  If there are linker options lines present, scan them

      while C = 'L' loop
         Checkc (' ');
         Checkc ('"');

         Name_Len := 0;

         loop
            C := Getc;

            if C < ' ' then
               Fatal_Error;
            end if;

            exit when C = '"';
            Add_Char_To_Name_Buffer (C);
         end loop;

         Add_Char_To_Name_Buffer (Ascii.NUL);

         Skip_Eol;
         C := Getc;

         Linker_Options.Increment_Last;

         Linker_Options.Table (Linker_Options.Last).Name
           := Name_Enter;

         Linker_Options.Table (Linker_Options.Last).Unit
           := ALIs.Table (Id).First_Unit;

         Linker_Options.Table (Linker_Options.Last).Internal_File
           := Is_Internal_File_Name (F);

         Linker_Options.Table (Linker_Options.Last).Original_Pos
           := Linker_Options.Last;

      end loop;

      --  Scan out source dependency lines for this ALI file

      ALIs.Table (Id).First_Sdep := Sdep.Last + 1;

      while C = 'D' loop
         Checkc (' ');
         Sdep.Increment_Last;
         Sdep.Table (Sdep.Last).Sfile := Get_Name;
         Sdep.Table (Sdep.Last).Stamp := Get_Stamp;

         --  Check for version number present, and if so store it

         Skip_Space;

         declare
            Ctr : Natural;
            Chk : Word;

         begin
            Ctr := 0;
            Chk := 0;

            loop
               exit when At_Eol or else Ctr = 8;

               if Nextc in '0' .. '9' then
                  Chk := Chk * 16 +
                           Character'Pos (Nextc) - Character'Pos ('0');

               elsif Nextc in 'A' .. 'F' then
                  Chk := Chk * 16 +
                           Character'Pos (Nextc) - Character'Pos ('A') + 10;

               else
                  exit;
               end if;

               Ctr := Ctr + 1;
               P := P + 1;
            end loop;

            if Ctr = 8 and then At_End_Of_Field then
               Sdep.Table (Sdep.Last).Checksum_Present := True;
               Sdep.Table (Sdep.Last).Checksum         := Chk;
            else
               Sdep.Table (Sdep.Last).Checksum_Present := False;
            end if;
         end;

         --  Skip comments after stamp

         while not At_Eol loop
            P := P + 1;
         end loop;

         Skip_Eol;
         C := Getc;
      end loop;

      ALIs.Table (Id).Last_Sdep := Sdep.Last;

      if C /= EOF and then C /= 'X' then
         Fatal_Error;
      end if;

      return Id;
   end Scan_ALI;

   ----------------------
   -- Set_Source_Table --
   ----------------------

   procedure Set_Source_Table (A : ALI_Id) is
      F     : File_Name_Type;
      S     : Source_Id;
      Stamp : Time_Stamp_Type;

   begin
      Sdep_Loop : for D in
        ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
      loop
         F := Sdep.Table (D).Sfile;

         --  If this is the first time we are seeing this source file,
         --  then make a new entry in the source table.

         if Get_Name_Table_Info (F) = 0 then
            Source.Increment_Last;
            S := Source.Last;
            Set_Name_Table_Info (F, Int (S));
            Source.Table (S).Sfile := F;
            Source.Table (S).All_Timestamps_Match := True;

            --  Initialize checksum fields

            Source.Table (S).Checksum :=
              Sdep.Table (D).Checksum;
            Source.Table (S).All_Checksums_Match :=
              Sdep.Table (D).Checksum_Present;

            --  In check source files mode, try to get time stamp from file

            if Check_Source_Files then
               Stamp := Source_File_Stamp (F);

               --  If we got the stamp, then set the stamp in the source
               --  table entry and mark it as set from the source so that
               --  it does not get subsequently changed.

               if Stamp (Stamp'First) /= ' ' then
                  Source.Table (S).Stamp := Stamp;
                  Source.Table (S).Source_Found := True;

               --  If we could not find the file, then the stamp is set
               --  from the dependency table entry (to be possibly reset
               --  if we find a later stamp in subsequent processing)

               else
                  Source.Table (S).Stamp := Sdep.Table (D).Stamp;
                  Source.Table (S).Source_Found := False;

                  --  In All_Sources mode, flag error of file not found

                  if All_Sources then
                     Error_Msg_Name_1 := F;
                     Error_Msg ("cannot locate %");
                  end if;
               end if;

            --  First time for this source file, but Check_Source_Files
            --  is off, so simply initialize the stamp from the Sdep entry

            else
               Source.Table (S).Source_Found := False;
               Source.Table (S).Stamp := Sdep.Table (D).Stamp;
            end if;

         --  Here if this is not the first time for this source file,
         --  so that the source table entry is already constructed.

         else
            S := Source_Id (Get_Name_Table_Info (F));

            --  Update checksum flag

            if not Sdep.Table (D).Checksum_Present
              or else Sdep.Table (D).Checksum /= Source.Table (S).Checksum
            then
               Source.Table (S).All_Checksums_Match := False;
            end if;

            --  Check for time stamp mismatch

            if Sdep.Table (D).Stamp /= Source.Table (S).Stamp then
               Source.Table (S).All_Timestamps_Match := False;

               --  When we have a time stamp mismatch, we go look for the
               --  source file even if Check_Source_Files is false, since
               --  if we find it, then we can use it to resolve which of the
               --  two timestamps in the ALI files is likely to be correct.

               if not Check_Source_Files then
                  Stamp := Source_File_Stamp (F);

                  if Stamp (Stamp'First) /= ' ' then
                     Source.Table (S).Stamp := Stamp;
                     Source.Table (S).Source_Found := True;
                  end if;
               end if;

               --  If the stamp in the source table entry was set from the
               --  source file, then we do not change it (the stamp in the
               --  source file is always taken as the "right" one).

               if Source.Table (S).Source_Found then
                  null;

               --  Otherwise, we have no source file available, so we guess
               --  that the later of the two timestamps is the right one.
               --  Note that this guess only affects which error messages
               --  are issued later on, not correct functionality.

               else
                  if Sdep.Table (D).Stamp > Source.Table (S).Stamp then
                     Source.Table (S).Stamp := Sdep.Table (D).Stamp;
                  end if;
               end if;
            end if;
         end if;

         --  Here to set the checksum value in the source table if necessary

         if Sdep.Table (D).Checksum_Present then
            S := Source_Id (Get_Name_Table_Info (F));
            Source.Table (S).Checksum := Sdep.Table (D).Checksum;
         end if;

      end loop Sdep_Loop;

   end Set_Source_Table;

   ----------------------
   -- Set_Source_Table --
   ----------------------

   procedure Set_Source_Table is
   begin
      for A in ALIs.First .. ALIs.Last loop
         Set_Source_Table (A);
      end loop;

   end Set_Source_Table;

   -------------------------
   -- Time_Stamp_Mismatch --
   -------------------------

   function Time_Stamp_Mismatch (A : ALI_Id) return File_Name_Type is
      Src : Source_Id;
      --  Source file Id for the current Sdep entry

   begin
      for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
         Src := Source_Id (Get_Name_Table_Info (Sdep.Table (D).Sfile));

         if Opt.Minimal_Recompilation
           and then Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
         then

            --  If minimal recompilation is in action, replace the stamp
            --  of the source file in the table if checksums match.

            --  ??? It is probably worth updating the ALI file with a new
            --  field to avoid recomputing it each time.

            if Sdep.Table (D).Checksum_Present
              and then Get_File_Checksum (Sdep.Table (D).Sfile) =
              Source.Table (Src).Checksum
            then
               Sdep.Table (D).Stamp := Source.Table (Src).Stamp;
            end if;

         end if;

         if not Source.Table (Src).Source_Found
           or else Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
         then
            return Source.Table (Src).Sfile;
         end if;
      end loop;

      return No_File;

   end Time_Stamp_Mismatch;

end ALI;
