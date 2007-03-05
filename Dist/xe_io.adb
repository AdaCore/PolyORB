------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                                X E _ I O                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2007, Free Software Foundation, Inc.          --
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
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with XE_Names;                   use XE_Names;
with XE_Flags;                   use XE_Flags;
with XE_Utils;                   use XE_Utils;

package body XE_IO is

   Program_Name : Name_Id := No_Name;

   Current_FD : File_Descriptor := Standout;
   --  File descriptor for current output

   ------------------------------
   -- Local Directory Routines --
   ------------------------------

   function Is_Directory_Separator (C : Character) return Boolean;

   function Format_Pathname
     (Path  : File_Name_Type;
      Style : Path_Style)
      return  File_Name_Type;
   --  See GNAT.Directory_Operations.Format_Pathname

   ----------------
   -- Temp Files --
   ----------------

   type Temp_File_Entry is record
      Fname : File_Name_Type;
      File  : File_Descriptor;
   end record;

   Null_Temp_File_Entry : constant Temp_File_Entry :=
     (No_File_Name, Invalid_FD);

   Temp_Files : array (1 .. 16) of Temp_File_Entry :=
     (others => Null_Temp_File_Entry);

   -------------------------
   -- Line Buffer Control --
   -------------------------

   --  Note: the following buffer and column position are maintained by
   --  the subprograms defined in this package, and are not normally
   --  directly modified or accessed by a client. However, a client is
   --  permitted to modify these values, using the knowledge that only
   --  Write_Eol actually generates any output.

   Buffer_Max : constant := 8192;
   Buffer     : String (1 .. Buffer_Max + 1);
   --  Buffer used to build output line. We do line buffering because it
   --  is needed for the support of the debug-generated-code option (-gnatD).
   --  Historically it was first added because on VMS, line buffering is
   --  needed with certain file formats. So in any case line buffering must
   --  be retained for this purpose, even if other reasons disappear. Note
   --  any attempt to write more output to a line than can fit in the buffer
   --  will be silently ignored.

   Next_Column : Pos range 1 .. Buffer'Length + 1 := 1;
   --  Column about to be written.

   procedure Flush_Buffer;
   --  Flush buffer if non-empty and reset column counter

   ---------------------------
   -- Predefined File Names --
   ---------------------------

   subtype Str8 is String (1 .. 8);

   Predefined_Names : constant array (1 .. 12) of Str8 :=
     ("ada     ",       -- Ada
      "calendar",       -- Calendar
      "gnat    ",       -- GNAT
      "interfac",       -- Interfaces
      "system  ",       -- System
      "machcode",       -- Machine_Code
      "unchconv",       -- Unchecked_Conversion
      "unchdeal",       -- Unchecked_Deallocation
      "directio",       -- Direct_IO
      "ioexcept",       -- IO_Exceptions
      "sequenio",       -- Sequential_IO
      "text_io ");      -- Text_IO

   ----------------
   -- Time Stamp --
   ----------------

   function Image (T : OS_Time) return Time_Stamp_Type;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (Source, Target : File_Name_Type) is
      S, T : String_Access;
      OK   : Boolean;

   begin
      Get_Name_String (Source);
      S := new String'(Name_Buffer (1 .. Name_Len));
      Get_Name_String (Target);
      T := new String'(Name_Buffer (1 .. Name_Len));
      Copy_File (S.all, T.all, OK, Mode => Overwrite);
      if not OK then
         Message ("cannot copy file " & S.all & " to " & T.all);
         raise Fatal_Error;
      end if;
      Free (S);
      Free (T);
   end Copy_File;

   ----------------
   -- Create_Dir --
   ----------------

   procedure Create_Dir (Dname : Directory_Name_Type) is
   begin
      if Dname = No_Directory_Name then
         return;
      end if;

      Get_Name_String (Dname);

      if Is_Directory (Name_Buffer (1 .. Name_Len)) then
         return;
      end if;

      for J in 2 .. Name_Len loop
         if Is_Directory_Separator (Name_Buffer (J))
           and then not Is_Directory (Name_Buffer (1 .. J - 1))
         then
            Make_Dir (Name_Buffer (1 .. J - 1));
         end if;
      end loop;
      Make_Dir (Name_Buffer (1 .. Name_Len));

   exception when others =>
      Message ("cannot create directory", Dname);
      raise;
   end Create_Dir;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File
     (File  : out File_Descriptor;
      Fname : File_Name_Type;
      Exec  : Boolean := False) is
   begin
      if Debug_Mode then
         Message ("creating file", Fname);
      end if;

      Get_Name_String (Fname);
      Name_Buffer (Name_Len + 1) := ASCII.NUL;
      File := Create_File (Name_Buffer'Address, Binary);

      if File = Invalid_FD then
         Message ("cannot create file", Fname);
         raise Fatal_Error;
      end if;

      if Exec then
         Set_Executable (Name_Buffer (1 .. Name_Len));
      end if;
   end Create_File;

   ---------------------------
   -- Decrement_Indentation --
   ---------------------------

   procedure Decrement_Indentation is
   begin
      N_Space := N_Space - Space_Increment;
   end Decrement_Indentation;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Fname : File_Name_Type) is
      Success : Boolean;

   begin
      if Debug_Mode then
         Message ("deleting", Fname);
      end if;

      Get_Name_String (Fname);
      if Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
         Delete_File (Name_Buffer (1 .. Name_Len), Success);
         if not Success then
            Message ("cannot delete file", Fname);
            raise Fatal_Error;
         end if;
      end if;
   end Delete_File;

   ---------
   -- Dir --
   ---------

   function Dir
     (D1 : File_Name_Type;
      D2 : File_Name_Type)
      return File_Name_Type is
   begin
      pragma Assert (D1 /= No_File_Name);

      if D2 = No_File_Name then
         return Format_Pathname (D1, UNIX);
      end if;

      Get_Name_String (D1);
      if not Is_Directory_Separator (Name_Buffer (Name_Len)) then
         Add_Char_To_Name_Buffer ('/');
      end if;
      Get_Name_String_And_Append (D2);

      return Format_Pathname (Name_Find, UNIX);
   end Dir;

   ---------
   -- Dir --
   ---------

   function Dir
     (D1 : String_Access;
      D2 : File_Name_Type)
      return File_Name_Type is
   begin
      pragma Assert (D1 /= null);

      Name_Len := 0;
      Add_Str_To_Name_Buffer (D1.all);

      if D2 = No_File_Name then
         return Format_Pathname (Name_Find, UNIX);
      end if;

      Add_Char_To_Name_Buffer ('/');
      Get_Name_String_And_Append (D2);

      return Format_Pathname (Name_Find, UNIX);
   end Dir;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp (Fname : File_Name_Type) return Time_Stamp_Type is
   begin
      Get_Name_String (Fname);
      return Image (File_Time_Stamp (Name_Buffer (1 .. Name_Len)));
   end File_Time_Stamp;

   ------------------
   -- Flush_Buffer --
   ------------------

   procedure Flush_Buffer is
      Len : constant Natural := Natural (Next_Column - 1);

   begin
      if Len /= 0 then

         if Len /= Write (Current_FD, Buffer'Address, Len) then

            --  If there are errors with standard error, just quit

            if Current_FD = Standerr then
               OS_Exit (2);

            --  Otherwise, set the output to standard error before
            --  reporting a failure and quitting.

            else
               Current_FD := Standerr;
               Next_Column := 1;
               Write_Line ("fatal error: disk full");
               OS_Exit (2);
            end if;
         end if;

         --  Buffer is now empty

         Next_Column := 1;
      end if;
   end Flush_Buffer;

   ---------------------
   -- Format_Pathname --
   ---------------------

   function Format_Pathname
     (Path  : File_Name_Type;
      Style : Path_Style)
     return  File_Name_Type is
   begin
      Get_Name_String (Path);
      Set_Str_To_Name_Buffer
        (Format_Pathname (Name_Buffer (1 .. Name_Len), Style));
      return Name_Find;
   end Format_Pathname;

   -----------
   -- Image --
   -----------

   function Image (T : OS_Time) return Time_Stamp_Type is
      Img      : Time_Stamp_Type;
      Year     : Natural;
      Month    : Natural;
      Day      : Natural;
      Hour     : Natural;
      Minutes  : Natural;
      Seconds  : Natural;
      Zero_Pos : constant := Character'Pos ('0');

   begin
      GM_Split
        (T,
         Natural (Year),
         Natural (Month),
         Natural (Day),
         Natural (Hour),
         Natural (Minutes),
         Natural (Seconds));
      Img (01) := Character'Val (Zero_Pos + Year / 1000);
      Img (02) := Character'Val (Zero_Pos + (Year / 100) mod 10);
      Img (03) := Character'Val (Zero_Pos + (Year / 10) mod 10);
      Img (04) := Character'Val (Zero_Pos + Year mod 10);
      Img (05) := Character'Val (Zero_Pos + Month / 10);
      Img (06) := Character'Val (Zero_Pos + Month mod 10);
      Img (07) := Character'Val (Zero_Pos + Day / 10);
      Img (08) := Character'Val (Zero_Pos + Day mod 10);
      Img (09) := Character'Val (Zero_Pos + Hour / 10);
      Img (10) := Character'Val (Zero_Pos + Hour mod 10);
      Img (11) := Character'Val (Zero_Pos + Minutes / 10);
      Img (12) := Character'Val (Zero_Pos + Minutes mod 10);
      Img (13) := Character'Val (Zero_Pos + Seconds / 10);
      Img (14) := Character'Val (Zero_Pos + Seconds mod 10);
      return Img;
   end Image;

   ---------------------------
   -- Increment_Indentation --
   ---------------------------

   procedure Increment_Indentation is
   begin
      N_Space := N_Space + Space_Increment;
   end Increment_Indentation;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Fname : File_Name_Type) return Boolean is
   begin
      Get_Name_String (Fname);
      return Is_Directory (Name_Buffer (1 .. Name_Len));
   end Is_Directory;

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      return C = Directory_Separator
        or else C = '/';
   end Is_Directory_Separator;

   ------------------------
   -- Is_Predefined_File --
   ------------------------

   function Is_Predefined_File (Fname : File_Name_Type) return Boolean is
   begin
      Get_Name_String (Strip_Suffix (Strip_Directory (Fname)));

      --  Definitely false if longer than 12 characters (8.3)

      if Name_Len > 8 then
         return False;

      --  Definitely predefined if prefix is a- i- or s- followed by letter

      elsif Name_Len >=  3
        and then Name_Buffer (2) = '-'
        and then (Name_Buffer (1) = 'a'
                  or else Name_Buffer (1) = 'g'
                  or else Name_Buffer (1) = 'i'
                  or else Name_Buffer (1) = 's')
        and then (Name_Buffer (3) in 'a' .. 'z'
                  or else Name_Buffer (3) in 'A' .. 'Z')
      then
         return True;
      end if;

      --  Otherwise check against special list, first padding to 8 characters

      while Name_Len < 8 loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ' ';
      end loop;

      for J in Predefined_Names'Range loop
         if Name_Buffer (1 .. 8) = Predefined_Names (J) then
            return True;
         end if;
      end loop;

      return False;
   end Is_Predefined_File;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Fname : File_Name_Type) return Boolean is
   begin
      Get_Name_String (Fname);
      return GNAT.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len));
   end Is_Regular_File;

   -------------
   -- Message --
   -------------

   procedure Message
     (S1 : String  := No_Str;
      S2 : Name_Id := No_Name;
      S3 : String  := No_Str;
      S4 : Name_Id := No_Name;
      S5 : String  := No_Str) is
   begin
      Write_Program_Name;
      Write_Str (":");
      if S1 /= "" then
         Write_Char (' ');
         Write_Str  (S1);
      end if;
      if Present (S2) then
         Write_Char (' ');
         Write_Name (S2);
      end if;
      if S3 /= "" then
         Write_Char (' ');
         Write_Str  (S3);
      end if;
      if Present (S4) then
         Write_Char (' ');
         Write_Name (S4);
      end if;
      if S5 /= "" then
         Write_Char (' ');
         Write_Str  (S5);
      end if;
      Write_Eol;
   end Message;

   -------------------
   -- Normalize_CWD --
   -------------------

   function Normalize_CWD (F : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (F);
      if Name_Buffer (1) = '.'
        and then Is_Directory_Separator (Name_Buffer (2))
      then
         return Strip_Directory (F);
      end if;
      return F;
   end Normalize_CWD;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (Fname  : File_Name_Type;
      First  : out Text_Ptr;
      Last   : out Text_Ptr;
      Buffer : out Text_Buffer_Ptr)
   is
      File   : File_Descriptor;
      Length : Text_Ptr;
      Result : Text_Ptr;
      Ptr    : Text_Ptr := 1;

   begin
      Get_Name_String (Fname);
      Name_Buffer (Name_Len + 1) := ASCII.NUL;
      File := Open_Read (Name_Buffer'Address, Binary);
      if File = Invalid_FD then
         Buffer := null;
         return;
      end if;

      Length := Text_Ptr (File_Length (File));
      Buffer := new Text_Buffer (1 .. Length + 1);
      First  := 1;
      Last   := Length + 1;

      --  Force the last character to be EOF

      Buffer (Length + 1) := EOF;

      loop
         Result :=
           Text_Ptr (Read (File, Buffer (Ptr)'Address, Integer (Length)));
         exit when Result = Length;
         if Result <= 0 then
            Free (Buffer);
            return;
         end if;
         Ptr    := Ptr + Result;
         Length := Length - Result;
      end loop;
      Close (File);
   end Read_File;

   ------------------------
   -- Register_Temp_File --
   ------------------------

   procedure Register_Temp_File (Fname : File_Name_Type) is
   begin
      for J in Temp_Files'Range loop
         if Temp_Files (J).Fname = No_File_Name then
            Temp_Files (J) := (Fname, Invalid_FD);
            return;
         end if;
      end loop;
      raise Fatal_Error;
   end Register_Temp_File;

   procedure Register_Temp_File
     (File  : out File_Descriptor;
      Fname : in out File_Name_Type)
   is
      Buffer : Temp_File_Name;

   begin
      if Fname = No_File_Name then
         Create_Temp_File (File, Buffer);
         Set_Str_To_Name_Buffer (Buffer);
         Fname := Name_Find;

      else
         Get_Name_String (Fname);
         File := Create_File (Name_Buffer (1 .. Name_Len), Text);
      end if;

      for J in Temp_Files'Range loop
         if No (Temp_Files (J).Fname) then
            Temp_Files (J) := (Fname, File);
            return;
         end if;
      end loop;
      raise Fatal_Error;
   end Register_Temp_File;

   ---------------------------
   -- Remove_All_Temp_Files --
   ---------------------------

   procedure Remove_All_Temp_Files is
      Success : Boolean;
   begin
      for J in Temp_Files'Range loop
         if Present (Temp_Files (J).Fname) then
            Close (Temp_Files (J).File, Success);
            if not Keep_Tmp_Files then
               Delete_File (Temp_Files (J).Fname);
            end if;
            Temp_Files (J) := Null_Temp_File_Entry;
         end if;
      end loop;
   end Remove_All_Temp_Files;

   ----------------------
   -- Remove_Temp_File --
   ----------------------

   procedure Remove_Temp_File (Fname : File_Name_Type) is
      Success : Boolean;
   begin
      for J in Temp_Files'Range loop
         if Temp_Files (J).Fname = Fname then
            Close (Temp_Files (J).File, Success);
            if not Keep_Tmp_Files then
               Delete_File (Fname);
            end if;
            Temp_Files (J) := Null_Temp_File_Entry;
            exit;
         end if;
      end loop;
   end Remove_Temp_File;

   -----------------
   -- Rename_File --
   -----------------

   procedure Rename_File (Source, Target : File_Name_Type) is
      S, T : String_Access;
      OK   : Boolean;

   begin
      Get_Name_String (Source);
      S := new String'(Name_Buffer (1 .. Name_Len));
      Get_Name_String (Target);
      T := new String'(Name_Buffer (1 .. Name_Len));
      Rename_File (S.all, T.all, OK);
      if not OK then
         Message ("cannot rename file " & S.all & " to " & T.all);
         raise Fatal_Error;
      end if;
      Free (S);
      Free (T);
   end Rename_File;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (New_Output : File_Descriptor) is
   begin
      Flush_Buffer;
      Next_Column := 1;
      Current_FD := New_Output;
   end Set_Output;

   -------------------------
   -- Set_Space_Increment --
   -------------------------

   procedure Set_Space_Increment (Value : Natural) is
   begin
      Space_Increment := Value;
   end Set_Space_Increment;

   ------------------------
   -- Set_Standard_Error --
   ------------------------

   procedure Set_Standard_Error is
   begin
      Flush_Buffer;
      Next_Column := 1;
      Current_FD := Standerr;
   end Set_Standard_Error;

   -------------------------
   -- Set_Standard_Output --
   -------------------------

   procedure Set_Standard_Output is
   begin
      Flush_Buffer;
      Next_Column := 1;
      Current_FD := Standout;
   end Set_Standard_Output;

   ---------------------
   -- Strip_Directory --
   ---------------------

   function Strip_Directory (Fname : String) return String is
   begin
      return Base_Name (Fname);
   end Strip_Directory;

   ---------------------
   -- Strip_Directory --
   ---------------------

   function Strip_Directory (Fname : File_Name_Type) return File_Name_Type is
   begin
      return Id (Strip_Directory (Get_Name_String (Fname)));
   end Strip_Directory;

   -----------------------
   -- Strip_Exec_Suffix --
   -----------------------

   function Strip_Exec_Suffix (Fname : File_Name_Type) return File_Name_Type is
      Exe : constant String := Get_Executable_Suffix.all;
      Len : constant Natural := Exe'Length;

   begin
      Get_Name_String (Fname);
      if Len > 0
        and then Name_Len > Len
        and then Name_Buffer (Name_Len - Len + 1 .. Name_Len) = Exe
      then
         Name_Len := Name_Len - Len;
      end if;
      return Name_Find;
   end Strip_Exec_Suffix;

   ------------------
   -- Strip_Suffix --
   ------------------

   function Strip_Suffix (Fname : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (Fname);

      for J in reverse 2 .. Name_Len loop

         --  If we found the last '.', return part of Name that precedes it

         if Name_Buffer (J) = '.' then
            Name_Len := J - 1;
            return Name_Find;
         end if;
      end loop;

      return Fname;
   end Strip_Suffix;

   ----------------------
   -- To_Absolute_File --
   ----------------------

   function To_Absolute_File (Fname : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (Fname);
      if Is_Absolute_Path (Name_Buffer (1 .. Name_Len)) then
         return Fname;
      end if;
      Set_Str_To_Name_Buffer (Get_Current_Dir);
      if Name_Buffer (Name_Len) /= Directory_Separator then
         Add_Char_To_Name_Buffer (Directory_Separator);
      end if;
      Get_Name_String_And_Append (Fname);
      return Name_Find;
   end To_Absolute_File;

   --------------
   -- To_Afile --
   --------------

   function To_Afile (Fname : File_Name_Type) return File_Name_Type is
   begin
      --  The ALI file is not necessarily in the directory of the
      --  source file.

      return Strip_Suffix (Strip_Directory (Fname)) & ALI_Suffix_Id;
   end To_Afile;

   --------------
   -- To_Ofile --
   --------------

   function To_Ofile (Fname : File_Name_Type) return File_Name_Type is
   begin
      return Strip_Suffix (Fname) & Obj_Suffix_Id;
   end To_Ofile;

   ----------------
   -- Write_Char --
   ----------------

   procedure Write_Char (C : Character) is
   begin
      if Next_Column < Buffer'Length then
         Buffer (Natural (Next_Column)) := C;
         Next_Column := Next_Column + 1;
      end if;
   end Write_Char;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol (N : Natural := 1) is
   begin
      for I in 1 .. N loop
         Buffer (Natural (Next_Column)) := ASCII.LF;
         Next_Column := Next_Column + 1;
         Flush_Buffer;
      end loop;
   end Write_Eol;

   -----------------------
   -- Write_Indentation --
   -----------------------

   procedure Write_Indentation (Offset : Integer := 0) is
   begin
      for I in 1 .. N_Space + Offset loop
         Write_Char (' ');
      end loop;
   end Write_Indentation;

   ---------------
   -- Write_Int --
   ---------------

   procedure Write_Int (Val : Int) is
   begin
      if Val < 0 then
         Write_Char ('-');
         Write_Int (-Val);

      else
         if Val > 9 then
            Write_Int (Val / 10);
         end if;

         Write_Char (Character'Val ((Val mod 10) + Character'Pos ('0')));
      end if;
   end Write_Int;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (S : String) is
   begin
      Write_Str (S);
      Write_Eol;
   end Write_Line;

   ------------------------
   -- Write_Program_Name --
   ------------------------

   procedure Write_Program_Name is
      use Ada.Command_Line;

   begin
      if No (Program_Name) then
         Program_Name := Strip_Exec_Suffix (Id (Base_Name (Command_Name)));
      end if;

      Write_Name (Program_Name);
   end Write_Program_Name;

   -----------------
   -- Write_Space --
   -----------------

   procedure Write_Space is
   begin
      Write_Char (' ');
   end Write_Space;

   ----------------------------
   -- Write_Stamp_Comparison --
   ----------------------------

   procedure Write_Stamp_Comparison
     (Newer, Older : File_Name_Type) is
   begin
      Write_Program_Name;
      Write_Str (": ");
      Write_Name (Newer);
      if Debug_Mode then
         Write_Str (" (");
         Write_Str (String (File_Time_Stamp (Newer)));
         Write_Str (")");
      end if;
      Write_Eol;
      Write_Program_Name;
      Write_Str (":    is more recent than");
      Write_Eol;
      Write_Program_Name;
      Write_Str (": ");
      Write_Name (Older);
      if Debug_Mode then
         Write_Str (" (");
         Write_Str (String (File_Time_Stamp (Older)));
         Write_Str (")");
      end if;
      Write_Eol;
   end Write_Stamp_Comparison;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str (S : String) is
   begin
      for J in S'Range loop
         Write_Char (S (J));
      end loop;
   end Write_Str;

end XE_IO;
