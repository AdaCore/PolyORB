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
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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
with Namet;   use Namet;
with Opt;     use Opt;
with Osint;   use Osint;
with Output;  use Output;

package body ALI is

   use Ascii;
   --  Make control characters visible

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
      Afile : File_Name_Type;
      Text  : Text_Buffer_Ptr;

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

               Read_ALI (Scan_ALI (Afile, Text));
               --  Scan and recurse
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

         --  Here is where we adjust any references to units that are
         --  affected by configuration pragmas.

         if Queuing_Policy = 'P' then
            if Name_Len = 12 then
               if Name_Buffer (1 .. 12) = "s-tasque.adb"
                 or else
                  Name_Buffer (1 .. 12) = "s-tasque.ali"
               then
                  Name_Buffer (8) := 'p';
               end if;
            end if;
         end if;

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
         T : Time_Stamp_Type;

      begin
         Skip_Space;

         if At_Eol then
            Fatal_Error;
         end if;

         for J in T'Range loop
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
      ALIs.Table (Id).Ofile_Full_Name  := Full_Object_File_Name;
      ALIs.Table (Id).First_Unit       := No_Unit_Id;
      ALIs.Table (Id).Main_Priority    := -1;
      ALIs.Table (Id).Time_Slice_Value := -1;
      ALIs.Table (Id).Main_Program     := None;

      --  Acquire library version

      Checkc ('V');
      Checkc (' ');
      Checkc ('"');

      for J in ALIs.Table (Id).Ver'Range loop
         ALIs.Table (Id).Ver (J) := Getc;
      end loop;

      Checkc ('"');
      Skip_Eol;

      --  Acquire standard version

      Checkc ('S');
      Checkc (' ');
      Checkc ('"');

      for J in ALIs.Table (Id).Std'Range loop
         ALIs.Table (Id).Std (J) := Getc;
      end loop;

      Checkc ('"');
      Skip_Eol;

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

      --  Acquire queuing policy line if present

      if C = 'Q' then
         Checkc (' ');

         C := Getc;
         ALIs.Table (Id).Queuing_Policy := C;

         if C = 'P' then
            Queuing_Policy := 'P';
         end if;

         Skip_Space;
         Skip_Eol;
         C := Getc;

      else
         ALIs.Table (Id).Queuing_Policy := ' ';
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
         Unit.Table (Unit.Last).Elaborate_Body := False;
         Unit.Table (Unit.Last).Version        := "00000000";
         Unit.Table (Unit.Last).First_With     := Withs.Last + 1;

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

            --  NE parameter (no elaboration)

            elsif C = 'N' then
               Checkc ('E');
               Check_At_End_Of_Field;
               Unit.Table (Unit.Last).No_Elab := True;

            --  PR/PU/PK parameters

            elsif C = 'P' then
               C := Getc;

               --  PR parameter (preelaborate) (also allow PRE for back
               --  compatibility with versions 2.03 and earlier)

               if C = 'R' then
                  if not At_End_Of_Field then
                     Checkc ('E');
                     Check_At_End_Of_Field;
                  end if;

                  Unit.Table (Unit.Last).Preelab := True;

               --  PU parameter (pure)

               elsif C = 'U' then
                  Check_At_End_Of_Field;
                  Unit.Table (Unit.Last).Pure := True;

               --  PK indicates unit is package

               elsif C = 'K' then
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
                  Check_At_End_Of_Field;

               else
                  Fatal_Error;
               end if;

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
            Withs.Table (Withs.Last).Uname         := Get_Name;
            Withs.Table (Withs.Last).Elaborate     := False;
            Withs.Table (Withs.Last).Elaborate_All := False;

            --  Generic case

            if At_Eol then
               Withs.Table (Withs.Last).Sfile := No_File;
               Withs.Table (Withs.Last).Afile := No_File;

            --  Normal case

            else
               Withs.Table (Withs.Last).Sfile := Get_Name;
               Withs.Table (Withs.Last).Afile := Get_Name;

               --  Scan out possible E and EA parameters

               while not At_Eol loop
                  Skip_Space;

                  if Getc = 'E' then
                     if At_End_Of_Field then
                        Withs.Table (Withs.Last).Elaborate := True;
                     else
                        Checkc ('A');
                        Check_At_End_Of_Field;
                        Withs.Table (Withs.Last).Elaborate_All := True;
                     end if;
                  end if;
               end loop;
            end if;

            Skip_Eol;
            C := Getc;

         end loop With_Loop;

         Unit.Table (Unit.Last).Last_With  := Withs.Last;

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

         declare
            Lbuf : String (1 .. 200);
            Llen : Natural := 0;
            Lptr : Natural;
            HC   : Natural;
            Dup  : Boolean;
            Tptr : Natural;

         begin
            loop
               C := Getc;

               if C < ' ' then
                  Fatal_Error;
               end if;

               exit when C = '"';
               Llen := Llen + 1;
               Lbuf (Llen) := C;
            end loop;

            Llen := Llen + 1;
            Lbuf (Llen) := Ascii.NUL;

            Skip_Eol;
            C := Getc;

            --  Now see if we already have this string stored

            Dup := False;
            Lptr := 0;
            Tptr := 1;

            while Tptr <= Linker_Options.Last loop
               Lptr := Lptr + 1;

               if Linker_Options.Table (Tptr) = Lbuf (Lptr) then
                  if Lptr = Llen then
                     Dup := True;
                     exit;
                  end if;

               elsif Linker_Options.Table (Tptr) = Ascii.Nul then
                  Lptr := 0;

               else
                  loop
                     Tptr := Tptr + 1;
                     exit when Linker_Options.Table (Tptr) = Ascii.NUL;
                  end loop;

                  Lptr := 0;
               end if;

               Tptr := Tptr + 1;
            end loop;

            --  If not a duplicate, add new string to table

            if not Dup then
               for J in 1 .. Llen loop
                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) := Lbuf (J);
               end loop;
            end if;
         end;
      end loop;

      --  Scan out source dependency lines for this ALI file

      ALIs.Table (Id).First_Sdep := Sdep.Last + 1;

      while C = 'D' loop
         Checkc (' ');
         Sdep.Increment_Last;
         Sdep.Table (Sdep.Last).Sfile := Get_Name;
         Sdep.Table (Sdep.Last).Stamp := Get_Stamp;

         --  Skip comments after stamp

         while not At_Eol loop
            P := P + 1;
         end loop;

         Skip_Eol;
         C := Getc;
      end loop;

      ALIs.Table (Id).Last_Sdep := Sdep.Last;

      if C /= EOF then
         Fatal_Error;
      end if;

      return Id;
   end Scan_ALI;

   ----------------------
   -- Set_Source_Table --
   ----------------------

   procedure Set_Source_Table (A : ALI_Id) is
      F : File_Name_Type;
      S : Source_Id;
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

            --  In check source files mode, try to get stamp from file

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

            --  If stamp was set from source file don't touch it. Otherwise
            --  update the stamp if the current reference in the Sdep entry
            --  is later than the current entry in the source table unless
            --  we find the corresponding source file and its time stamp
            --  matches the earlier one.

            if not Source.Table (S).Source_Found
              and then Sdep.Table (D).Stamp /= Source.Table (S).Stamp
            then
               Stamp := Source_File_Stamp (F);

               if Stamp = Source.Table (S).Stamp then
                  null;
               elsif Stamp = Sdep.Table (D).Stamp
                 or else Sdep.Table (D).Stamp > Source.Table (S).Stamp
               then
                  Source.Table (S).Stamp := Sdep.Table (D).Stamp;
               end if;

               if Stamp = Source.Table (S).Stamp then
                  Source.Table (S).Source_Found := True;
               end if;
            end if;
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

         if not Source.Table (Src).Source_Found
           or else Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
         then
            return Source.Table (Src).Sfile;
         end if;
      end loop;

      return No_File;

   end Time_Stamp_Mismatch;

end ALI;
