------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ U T I L S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with System;
with Unchecked_Deallocation;

with ALI;            use ALI;
with GNAT.OS_Lib;    use GNAT.OS_Lib;
with Csets;          use Csets;
with Debug;          use Debug;
with Fname;          use Fname;
with Make;           use Make;
with Namet;          use Namet;
with Opt;
with Osint;          use Osint;
with Output;         use Output;
with Types;          use Types;
with XE;             use XE;
with XE_Defs;        use XE_Defs;

with Ada.Command_Line; use Ada.Command_Line;

pragma Elaborate_All (Csets, Debug, Make, Namet, Opt, Osint, Output);

package body XE_Utils is

   Path         : constant String_Access := GNAT.OS_Lib.Getenv ("PATH");

   GNAT_Verbose   : String_Access;
   Gcc            : String_Access;
   Mkdir          : String_Access;
   Copy           : String_Access;
   Link           : String_Access;
   Chmod          : String_Access;
   Rm             : String_Access;
   Gnatbind       : String_Access;
   Gnatlink       : String_Access;
   Gnatmake       : String_Access;

   Up_To_Low : constant := Character'Pos ('A') - Character'Pos ('a');

   EOL : constant String := (1 => Ascii.LF);

   Output_Flag           : constant String_Access := new String' ("-o");
   Preserve              : constant String_Access := new String' ("-lf");
   Symbolic              : constant String_Access := new String' ("-s");
   Force                 : constant String_Access := new String' ("-f");
   Compile_Flag          : constant String_Access := new String' ("-c");
   Exclude_File_Flag     : constant String_Access := new String' ("-x");
   Receiver_Compile_Flag : constant String_Access := new String' ("-gnatzr");
   Caller_Compile_Flag   : constant String_Access := new String' ("-gnatzc");

   Special_File_Flag     : constant String_Access := new String' ("-x");
   Ada_File_Flag         : constant String_Access := new String' ("ada");

   GARLIC                : String_Access;

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
     return String_Access;

   function Has_Standard_Extension (File : File_Name_Type) return Boolean;
   --  Check whether File has a standard extension for GCC and hence does
   --  not need the "-x ada" command line argument (typically ".ads" or
   --  ".adb" terminated file).

   ---------
   -- "&" --
   ---------

   function "&"
     (N1 : Types.File_Name_Type;
      N2 : Types.File_Name_Type)
      return Types.File_Name_Type is
   begin
      pragma Assert (N1 /= No_File);
      if N2 = No_File then
         return N1;
      end if;
      Get_Name_String (N1);
      Get_Name_String_And_Append (N2);
      return Name_Find;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (N1 : Types.File_Name_Type;
      N2 : String)
      return Types.File_Name_Type is
   begin
      pragma Assert (N1 /= No_File);
      Get_Name_String (N1);
      Add_Str_To_Name_Buffer (N2);
      return Name_Find;
   end "&";

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (To : in File_Name_Type) is

      C_Path : String (1 .. Strlen (To) + 1);

      function Chdir (Path : System.Address) return Int;
      pragma Import (C, Chdir, "chdir");

   begin

      if Debug_Mode then
         Message ("change to dir", To);
      end if;

      Get_Name_String (To);
      C_Path (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
      C_Path (Name_Len + 1) := Ascii.Nul;
      if Chdir (C_Path'Address) /= 0 then
         Message ("cannot change dir to", To);
         raise Fatal_Error;
      end if;

      if Building_Script then
         if Name_Len < 3 or else Name_Buffer (1 .. 3) /= "../" then
            Write_Str  (Standout, "if test ! -d ");
            Write_Name (Standout, To);
            Write_Str  (Standout, "; then mkdir -p ");
            Write_Name (Standout, To);
            Write_Str  (Standout, "; fi");
            Write_Eol  (Standout);
         end if;
         Write_Str  (Standout, "cd ");
         Write_Name (Standout, To);
         Write_Eol  (Standout);
      end if;

   end Change_Dir;

   -----------------------
   -- Compilation_Error --
   -----------------------

   procedure Compilation_Error (File : File_Name_Type) is
   begin
      Message ("", File, "compilation error");
      raise Compilation_Failed;
   end Compilation_Error;

   ------------------------
   -- Compile_RCI_Caller --
   ------------------------

   procedure Compile_RCI_Caller (Source, Object : in File_Name_Type) is
   begin
      Execute_Gcc
        (Source,
         Object,
         (Caller_Compile_Flag,
          I_GARLIC_Dir)
         );
   end Compile_RCI_Caller;

   --------------------------
   -- Compile_RCI_Receiver --
   --------------------------

   procedure Compile_RCI_Receiver (Source, Object : in File_Name_Type) is
   begin
      Execute_Gcc
        (Source,
         Object,
         (Receiver_Compile_Flag,
          I_GARLIC_Dir)
         );
   end Compile_RCI_Receiver;

   --------------------------
   -- Copy_With_File_Stamp --
   --------------------------

   procedure Copy_With_File_Stamp
     (Source, Target : in File_Name_Type;
      Maybe_Symbolic : in Boolean := False) is
      S : String_Access := new String (1 .. Strlen (Source));
      T : String_Access := new String (1 .. Strlen (Target));

   begin
      Get_Name_String (Source);
      S.all := Name_Buffer (1 .. Name_Len);
      Get_Name_String (Target);
      T.all := Name_Buffer (1 .. Name_Len);
      if Link = null then
         Execute (Copy, (Preserve, S, T));
      else
         Execute (Rm, (Force, T));
         if Maybe_Symbolic then
            Execute (Link, (Symbolic, S, T));
         else
            Execute (Link, (S, T));
         end if;
      end if;
      Free (S);
      Free (T);
   end Copy_With_File_Stamp;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Descriptor;
      Name : in File_Name_Type;
      Exec : in Boolean := False) is
      File_Name_Len : Natural := Strlen (Name);
      File_Name     : String (1 .. File_Name_Len + 1);
   begin
      Get_Name_String (Name);
      File_Name (1 .. File_Name_Len) := Name_Buffer (1 .. Name_Len);
      File_Name (File_Name_Len + 1) := Ascii.Nul;

      if Verbose_Mode then
         Message ("creating file", Name);
      end if;

      File := Create_File (File_Name'Address, Text);

      if File = Invalid_FD then
         Message ("cannot create file", Name);
         raise Fatal_Error;
      end if;

      if Exec then
         Execute
           (Chmod,
            (1 => new String'("u+x"),
             2 => new String'(File_Name (1 .. File_Name_Len))));
      end if;

   end Create;

   ----------------
   -- Create_Dir --
   ----------------

   procedure Create_Dir (To : in File_Name_Type) is
      Dir_Name_Len : Natural := Strlen (To);
      Dir_Name     : String (1 .. Dir_Name_Len);
   begin
      Get_Name_String (To);
      Dir_Name := Name_Buffer (1 .. Name_Len);
      for Index in Dir_Name'Range loop

         --  ???
         if Dir_Name (Index) = Directory_Separator and then Index > 1 and then
            not Is_Directory (Dir_Name (1 .. Index - 1)) then
            Execute (Mkdir, (1 => new String'(Dir_Name (1 .. Index - 1))));
         elsif Index = Dir_Name'Last then
            Execute (Mkdir, (1 => new String'(Dir_Name)));
         end if;
      end loop;

   end Create_Dir;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in File_Name_Type) is
      Error : Boolean;
   begin
      if Verbose_Mode then
         Message ("deleting", File);
      end if;
      Get_Name_String (File);
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := Ascii.Nul;
      Delete_File (Name_Buffer'Address, Error);
   end Delete;

   ---------
   -- Dir --
   ---------

   function Dir
     (D1 : Types.File_Name_Type;
      D2 : Types.File_Name_Type := No_File;
      D3 : Types.File_Name_Type := No_File;
      D4 : Types.File_Name_Type := No_File)
      return Types.File_Name_Type is
   begin
      pragma Assert (D1 /= No_File);

      Get_Name_String (D1);
      if D2 = No_File then
         return Name_Find;
      end if;
      Add_Char_To_Name_Buffer (Directory_Separator);
      Get_Name_String_And_Append (D2);
      if D3 = No_File then
         return Name_Find;
      end if;
      Add_Char_To_Name_Buffer (Directory_Separator);
      Get_Name_String_And_Append (D3);
      if D4 = No_File then
         return Name_Find;
      end if;
      Add_Char_To_Name_Buffer (Directory_Separator);
      Get_Name_String_And_Append (D4);
      return Name_Find;
   end Dir;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Prog  : in String_Access;
      Args  : in Argument_List;
      Fatal : in Boolean := True)
   is
      Success : Boolean := False;
   begin

      if Verbose_Mode or else Building_Script then
         if Building_Script then
            Write_Str (Standout, Prog.all);
         else
            Write_Str (Prog.all);
         end if;
         for Index in Args'Range loop
            if Args (Index) /= null then
               if Building_Script then
                  Write_Str (Standout, " ");
                  Write_Str (Standout, Args (Index).all);
               else
                  Write_Str (" ");
                  Write_Str (Args (Index).all);
               end if;
            end if;
         end loop;
         if Building_Script then
            Write_Eol (Standout);
         else
            Write_Eol;
         end if;
      end if;

      Spawn (Prog.all, Args, Success);

      if Fatal and then not Success then
         Message (Prog.all, No_Name, "failed");
         raise Fatal_Error;
      end if;

   end Execute;

   ------------------
   -- Execute_Bind --
   ------------------

   procedure Execute_Bind
     (Lib   : in File_Name_Type;
      Args  : in Argument_List;
      Fatal : in Boolean := True)
   is
      Length : constant Positive :=
        Args'Length + Binder_Switches.Last - Binder_Switches.First + 3;

      Bind_Flags   : Argument_List (1 .. Length);

      Lib_Name     : String (1 .. Strlen (Lib));

      N_Bind_Flags : Natural range 0 .. Length := 0;
   begin

      N_Bind_Flags := N_Bind_Flags + 1;
      Bind_Flags (N_Bind_Flags) := Exclude_File_Flag;

      --  various arguments

      for I in Args'Range loop
         N_Bind_Flags := N_Bind_Flags + 1;
         Bind_Flags (N_Bind_Flags) := Args (I);
      end loop;

      for I in Binder_Switches.First .. Binder_Switches.Last loop
         N_Bind_Flags := N_Bind_Flags + 1;
         Bind_Flags (N_Bind_Flags) := Binder_Switches.Table (I);
      end loop;

      --  <unit name>

      Get_Name_String (Lib);
      Lib_Name := Name_Buffer (1 .. Name_Len);
      N_Bind_Flags := N_Bind_Flags + 1;
      Bind_Flags (N_Bind_Flags) := new String'(Lib_Name);

      --  Call gnatbind

      Execute (Gnatbind, Bind_Flags (1 .. N_Bind_Flags), Fatal);

   end Execute_Bind;

   -----------------
   -- Execute_Gcc --
   -----------------

   procedure Execute_Gcc
     (File   : in File_Name_Type;
      Object : in File_Name_Type;
      Args   : in Argument_List;
      Fatal  : in Boolean := True)
   is
      Max_Length  : constant Natural
        := Gcc_Switches.Last - Gcc_Switches.First + 6 + Args'Length + 2;

      File_Name   : String_Access;
      Object_Name : String_Access;

      Gcc_Flags   : Argument_List (1 .. Max_Length);

      N_Gcc_Flags : Natural range 0 .. Max_Length := 0;
   begin
      if not Has_Standard_Extension (File) then
         N_Gcc_Flags := N_Gcc_Flags + 1;
         Gcc_Flags (N_Gcc_Flags) := Special_File_Flag;

         N_Gcc_Flags := N_Gcc_Flags + 1;
         Gcc_Flags (N_Gcc_Flags) := Ada_File_Flag;
      end if;

      N_Gcc_Flags := N_Gcc_Flags + 1;
      Gcc_Flags (N_Gcc_Flags) := Compile_Flag;

      N_Gcc_Flags := N_Gcc_Flags + 1;
      Get_Name_String (File);
      File_Name := new String'(Name_Buffer (1 .. Name_Len));
      Gcc_Flags (N_Gcc_Flags) := File_Name;

      if Object /= No_File then
         N_Gcc_Flags := N_Gcc_Flags + 1;
         Gcc_Flags (N_Gcc_Flags) := Output_Flag;

         N_Gcc_Flags := N_Gcc_Flags + 1;
         Get_Name_String (Object);
         Object_Name := new String'(Name_Buffer (1 .. Name_Len));
         Gcc_Flags (N_Gcc_Flags) := Object_Name;
      end if;

      for I in Args'Range loop
         N_Gcc_Flags := N_Gcc_Flags + 1;
         Gcc_Flags (N_Gcc_Flags) := Args (I);
      end loop;

      for I in Gcc_Switches.First .. Gcc_Switches.Last loop
         N_Gcc_Flags := N_Gcc_Flags + 1;
         Gcc_Flags (N_Gcc_Flags) := Gcc_Switches.Table (I);
      end loop;

      N_Gcc_Flags := N_Gcc_Flags + 1;
      Gcc_Flags (N_Gcc_Flags) := I_Current_Dir;

      Execute (Gcc, Gcc_Flags (1 .. N_Gcc_Flags), Fatal);

      Free (File_Name);
      Free (Object_Name);

   end Execute_Gcc;

   ------------------
   -- Execute_Link --
   ------------------

   procedure Execute_Link
     (Lib   : in File_Name_Type;
      Exec  : in File_Name_Type;
      Args  : in Argument_List;
      Fatal : in Boolean := True)
   is
      Length : constant Positive :=
        2 +            --  -o <executable name>
        1 +            --  <unit_name>
        Args'Length +
        Linker_Switches.Last - Linker_Switches.First + 1;

      Link_Flags   : Argument_List (1 .. Length);

      Lib_Name     : String (1 .. Strlen (Lib));
      Exec_Name    : String (1 .. Strlen (Exec));

      N_Link_Flags : Natural range 0 .. Length := 0;
   begin
      --  -o <executable name>

      Get_Name_String (Exec);
      Exec_Name := Name_Buffer (1 .. Name_Len);
      N_Link_Flags := N_Link_Flags + 1;
      Link_Flags (N_Link_Flags) := Output_Flag;
      N_Link_Flags := N_Link_Flags + 1;
      Link_Flags (N_Link_Flags) := new String'(Exec_Name);

      --  various arguments

      for I in Args'Range loop
         N_Link_Flags := N_Link_Flags + 1;
         Link_Flags (N_Link_Flags) := Args (I);
      end loop;

      --  <unit name>

      Get_Name_String (Lib);
      Lib_Name := Name_Buffer (1 .. Name_Len);
      N_Link_Flags := N_Link_Flags + 1;
      Link_Flags (N_Link_Flags) := new String'(Lib_Name);

      for I in Linker_Switches.First .. Linker_Switches.Last loop
         N_Link_Flags := N_Link_Flags + 1;
         Link_Flags (N_Link_Flags) := Linker_Switches.Table (I);
      end loop;

      --  Call gnatmake

      Execute (Gnatlink, Link_Flags (1 .. N_Link_Flags), Fatal);

   end Execute_Link;

   ----------------
   -- GNAT_Style --
   ----------------

   function GNAT_Style (N : Name_Id) return Name_Id is
      Capitalized : Boolean := True;
   begin
      Get_Name_String (N);
      for I in 1 .. Name_Len loop
         if Name_Buffer (I) in 'a' .. 'z' then
            if Capitalized then
               Name_Buffer (I) :=
                 Character'Val (Character'Pos (Name_Buffer (I)) + Up_To_Low);
            end if;
            Capitalized := False;
         elsif Name_Buffer (I) in 'A' .. 'Z' then
            if not Capitalized then
               Name_Buffer (I) :=
                 Character'Val (Character'Pos (Name_Buffer (I)) - Up_To_Low);
            end if;
            Capitalized := False;
         elsif Name_Buffer (I) = '_' or else Name_Buffer (I) = '.' then
            Capitalized := True;
         end if;
      end loop;
      return Name_Find;
   end GNAT_Style;

   ----------------------------
   -- Has_Standard_Extension --
   ----------------------------

   function Has_Standard_Extension (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return
        Name_Len > 3
          and then
        Name_Buffer (Name_Len - 3 .. Name_Len - 1) = ".ad"
          and then
        (Name_Buffer (Name_Len) = 's' or else Name_Buffer (Name_Len) = 'b');
   end Has_Standard_Extension;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      GARLIC_Included : Boolean := False;
   begin
      --  Default initialization of the flags affecting gnatdist

      Opt.Check_Readonly_Files     := False;
      Opt.Check_Object_Consistency := True;
      Opt.Compile_Only             := False;
      Opt.Do_Not_Execute           := False;
      Opt.Force_Compilations       := False;
      Opt.Quiet_Output             := False;
      Opt.Minimal_Recompilation    := False;
      Opt.Verbose_Mode             := False;

      --  Package initializations. The order of calls is important here.

      Output.Set_Standard_Error;
      Osint.Initialize (Osint.Make); --  reads gnatmake switches

      Gcc_Switches.Init;
      Binder_Switches.Init;
      Linker_Switches.Init;

      Csets.Initialize;
      Namet.Initialize;

      GNATLib_Compile_Flag := new String'("-gnatg");
      Obj_Suffix           := Str_To_Id (Get_Object_Suffix.all);
      Exe_Suffix           := Str_To_Id (Get_Executable_Suffix.all);

      Gcc             := Locate ("gcc");
      Mkdir           := Locate ("mkdir");
      Copy            := Locate ("cp");
      Link            := Locate ("ln", False);
      Chmod           := Locate ("chmod");
      Rm              := Locate ("rm");
      Gnatbind        := Locate ("gnatbind");
      Gnatlink        := Locate ("gnatlink");
      Gnatmake        := Locate ("gnatmake");

      ALI_Suffix     := Str_To_Id (".ali");
      ADS_Suffix     := Str_To_Id (".ads");
      ADB_Suffix     := Str_To_Id (".adb");

      Spec_Suffix    := Str_To_Id ("%s");
      Body_Suffix    := Str_To_Id ("%b");

      DSA_Dir        := Str_To_Id ("dsa");

      Name_Len := 18;
      Name_Buffer (1 .. 18) := "dsa/private/caller";
      Name_Buffer (4)  := Directory_Separator;
      Name_Buffer (12) := Directory_Separator;
      Caller_Dir := Name_Find;

      Name_Len := 20;
      Name_Buffer (13 .. 20) := "receiver";
      Receiver_Dir := Name_Find;

      PWD_Id         := Dir (Str_To_Id ("`pwd`"), No_File);

      Build_Stamp_File    := Str_To_Id ("glade.sta");
      Elaboration_File    := Str_To_Id ("s-garela");
      Elaboration_Name    := Str_To_Id ("System.Garlic.Elaboration");
      Partition_Main_File := Str_To_Id ("partition");
      Partition_Main_Name := Str_To_Id ("Partition");

      GARLIC := Getenv ("GLADE_LIBRARY_DIR");
      if GARLIC = null
        or else GARLIC'Length = 0
      then
         GARLIC := Get_GARLIC_Dir;
      end if;

      Name_Len := 2;
      Name_Buffer (1) := '-';
      Name_Buffer (2) := 'I';
      Add_Str_To_Name_Buffer (GARLIC.all);
      I_GARLIC_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name_Buffer (2) := 'L';
      L_GARLIC_Dir := new String'(Name_Buffer (1 .. Name_Len));

      I_Current_Dir := new String'("-I.");
      L_Current_Dir := new String'("-L.");

      Name_Len := 20;
      Name_Buffer (1 .. 20) := "-Idsa/private/caller";
      Name_Buffer (6)  := Directory_Separator;
      Name_Buffer (14) := Directory_Separator;
      I_Caller_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name_Buffer (2) := 'L';
      L_Caller_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Optimization_Mode := True;
      for I in 1 .. Argument_Count loop
         if Argument (I)(1) = Switch_Character
           or else Argument (I)(1) = '-'
         then
            if Argument (I)(2 .. Argument (I)'Last) = "cargs"
              or else Argument (I)(2 .. Argument (I)'Last) = "bargs"
              or else Argument (I)(2 .. Argument (I)'Last) = "largs"
            then
               Scan_Make_Arg (I_GARLIC_Dir.all);
               GARLIC_Included := True;
            elsif Argument (I)'Length = 2
              and then Argument (I)(2) = 'g' then
               Optimization_Mode := False;
            elsif Argument (I)'Length > 2
              and then Argument (I)(2) = 'O' then
               Optimization_Mode := False;
            end if;
         end if;
         Scan_Make_Arg (Argument (I));
      end loop;

      if Optimization_Mode then
         Scan_Make_Arg ("-O2");
      end if;

      if not GARLIC_Included then
         Scan_Make_Arg (I_GARLIC_Dir.all);
      end if;

      Osint.Add_Default_Search_Dirs;

      --  Source file lookups should be cached for efficiency.
      --  Source files are not supposed to change.

      --  Osint.Source_File_Data (Cache => True);

      Linker_Switches.Increment_Last;
      Linker_Switches.Table (Linker_Switches.Last) := new String'("-lgarlic");

      --  Use Gnatmake already defined switches.
      Verbose_Mode       := Opt.Verbose_Mode;
      Debug_Mode         := Debug.Debug_Flag_Q;
      Optimization_Mode  := Optimization_Mode and then Debug.Debug_Flag_S;
      Quiet_Output       := Opt.Quiet_Output;
      No_Recompilation   := Opt.Do_Not_Execute;
      Building_Script    := Opt.List_Dependencies;

      --  Use -dq and -ds for Gnatdist internal debugging.
      Debug.Debug_Flag_Q := False;
      Debug.Debug_Flag_S := False;

      --  Don't want log messages that would corrupt scripts.
      if Building_Script then
         Verbose_Mode := False;
         Quiet_Output := True;
      end if;

      Opt.Check_Source_Files := False;
      Opt.All_Sources        := False;

      if Verbose_Mode then
         GNAT_Verbose := new String' ("-v");
      else
         GNAT_Verbose := new String' ("-q");
      end if;

      if Debug_Mode then
         for I in Gcc_Switches.First .. Gcc_Switches.Last loop
            Write_Str ("compiler [");
            Write_Int (Int (I));
            Write_Str ("] = ");
            Write_Str (Gcc_Switches.Table (I).all);
            Write_Eol;
         end loop;
         for I in Binder_Switches.First .. Binder_Switches.Last loop
            Write_Str ("binder [");
            Write_Int (Int (I));
            Write_Str ("] = ");
            Write_Str (Binder_Switches.Table (I).all);
            Write_Eol;
         end loop;
         for I in Linker_Switches.First .. Linker_Switches.Last loop
            Write_Str ("linker [");
            Write_Int (Int (I));
            Write_Str ("] = ");
            Write_Str (Linker_Switches.Table (I).all);
            Write_Eol;
         end loop;
      end if;
   end Initialize;

   ------------------
   -- Is_Body_Name --
   ------------------

   function Is_Body_Name (U : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (U);
      return Name_Len > 2
        and then Name_Buffer (Name_Len - 1) = '%'
        and then Name_Buffer (Name_Len) = 'b';
   end Is_Body_Name;

   ------------------
   -- Is_Spec_Name --
   ------------------

   function Is_Spec_Name (U : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (U);
      return Name_Len > 2
        and then Name_Buffer (Name_Len - 1) = '%'
        and then Name_Buffer (Name_Len) = 's';
   end Is_Spec_Name;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return Is_Directory (Name_Buffer (1 .. Name_Len));
   end Is_Directory;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return GNAT.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len));
   end Is_Regular_File;

   ---------------------
   -- Is_Relative_Dir --
   ---------------------

   function Is_Relative_Dir (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);
      return Name_Len = 0 or else
        (Name_Buffer (1) /= Directory_Separator
         and then Name_Buffer (1) /= '/');
   end Is_Relative_Dir;

   -----------------
   -- Find_Source --
   -----------------

   function Find_Source
     (Uname : Unit_Name_Type;
      Fatal : Boolean := False)
      return File_Name_Type is
      Info : Int;
      File : File_Name_Type;
      Name : Unit_Name_Type;
      Spec : Boolean;

   begin
      Get_Name_String (Uname);
      if Name_Len < 3 or else Name_Buffer (Name_Len - 1) /= '%' then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := '%';
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := 'b';
      end if;

      --  If Info /= 0, then this unit is already in table Units. Info
      --  corresponds to the index in this table. Check body first.
      --  When the unit source file does not follow GNAT convention,
      --  we shall find it anyway as long as this unit is already loaded.

      Info := Get_Name_Table_Info (Name_Find);
      if Info /= 0 then
         return Units.Table (Unit_Id (Info)).Sfile;
      else
         --  If Info /= 0, then this unit is already in table Units. Info
         --  corresponds to the index in this table. Then check spec.

         Name_Buffer (Name_Len) := 's';
         Info := Get_Name_Table_Info (Name_Find);
         if Info /= 0 then
            return Units.Table (Unit_Id (Info)).Sfile;
         end if;

         Name := U_To_N (Uname);

         --  Find a regular source file. If Uname is a spec name,
         --  then try to find the spec source first. Otherwise,
         --  try to find the body source, then the spec one.

         Spec := Is_Spec_Name (Uname);

         if Spec then
            File := File_Name_Of_Spec (Name);
            if Full_Source_Name (File) /= No_File then
               return File;
            end if;
         end if;

         File := File_Name_Of_Body (Name);
         if Full_Source_Name (File) /= No_File then
            return File;
         end if;

         if not Spec then
            File := File_Name_Of_Spec (Name);
            if Full_Source_Name (File) /= No_File then
               return File;
            end if;
         end if;

         if Fatal then
            Source_File_Error (Uname);
         end if;

         return No_File;
      end if;
   end Find_Source;

   ------------
   -- Locate --
   ------------

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
     return String_Access is
      Loc : String_Access;
   begin
      Name_Len := Exec_Name'Length;
      Name_Buffer (1 .. Name_Len) := Exec_Name;
      declare
         Exe : constant String := Name_Buffer (1 .. Name_Len);
      begin
         Loc := GNAT.OS_Lib.Locate_Exec_On_Path (Exe);
         if Loc = null and then Show_Error then
            Message (Exe, No_Name, "is not in your path");
            raise Fatal_Error;
         end if;
      end;
      return Loc;
   end Locate;

   -------------
   -- Message --
   -------------

   procedure Message
     (S1 : in String  := "";
      S2 : in Name_Id := No_Name;
      S3 : in String  := "";
      S4 : in Name_Id := No_Name;
      S5 : in String  := "") is
   begin
      Write_Program_Name;
      Write_Str (":");
      if S1 /= "" then
         Write_Char (' ');
         Write_Str  (S1);
      end if;
      if S2 /= No_Name then
         Write_Char (' ');
         Write_Name (S2);
      end if;
      if S3 /= "" then
         Write_Char (' ');
         Write_Str  (S3);
      end if;
      if S4 /= No_Name then
         Write_Char (' ');
         Write_Name (S4);
      end if;
      if S5 /= "" then
         Write_Char (' ');
         Write_Str  (S5);
      end if;
      Write_Eol;
   end Message;

   -----------------------
   -- Source_File_Error --
   -----------------------

   procedure Source_File_Error (Uname : Unit_Name_Type) is
   begin
      Message ("no source file for unit", To_String (U_To_N (Uname)));
      raise Compilation_Failed;
   end Source_File_Error;

   -----------
   -- Stamp --
   -----------

   function Stamp (F : File_Name_Type) return String is
   begin
      return String (Source_File_Stamp (F));
   end Stamp;

   ---------------
   -- Str_To_Id --
   ---------------

   function Str_To_Id (S : String) return Name_Id is
   begin
      if S'Length = 0 then
         return No_Name;
      end if;
      Name_Buffer (1 .. S'Length) := S;
      Name_Len := S'Length;
      return Name_Find;
   end Str_To_Id;

   ------------
   -- Strlen --
   ------------

   function Strlen (Name : in Name_Id) return Natural is
   begin
      Get_Name_String (Name);
      return Name_Len;
   end Strlen;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in out String) is
   begin
      for I in S'Range loop
         if S (I) in 'A' .. 'Z' then
            S (I) := Character'Val (Character'Pos (S (I)) - Up_To_Low);
         end if;
      end loop;
   end To_Lower;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (N : in out Name_Id) is
   begin
      Get_Name_String (N);
      To_Lower (Name_Buffer (1 .. Name_Len));
      N := Name_Find;
   end To_Lower;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (N : Name_Id) return Name_Id is
   begin
      Get_Name_String (N);
      To_Lower (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end To_Lower;

   -------------
   -- To_Spec --
   -------------

   function To_Spec (U : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (U);
      if Name_Len > 2
        and then Name_Buffer (Name_Len - 1) = '%'
      then
         Name_Buffer (Name_Len) := 's';
      else
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := '%';
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := 's';
      end if;
      return Name_Find;
   end To_Spec;

   ---------------
   -- To_String --
   ---------------

   function To_String (N : Name_Id) return Name_Id is
   begin
      Name_Len := 0;
      Add_Char_To_Name_Buffer ('"');
      Get_Name_String_And_Append (N);
      Add_Char_To_Name_Buffer ('"');
      return Name_Find;
   end To_String;

   ------------
   -- U_To_N --
   ------------

   function U_To_N (U : in Unit_Name_Type) return Name_Id is
   begin
      if U /= No_Name then
         Get_Name_String (U);
         if Name_Buffer (Name_Len - 1) = '%' then --  %
            Name_Len := Name_Len - 2;
         end if;
         return Name_Find;
      else
         return U;
      end if;
   end U_To_N;

   -----------------
   -- Unlink_File --
   -----------------

   procedure Unlink_File (File : in File_Name_Type) is
      File_Name : String_Access := new String (1 .. Strlen (File));
      --  procedure Free is new Unchecked_Deallocation (String, String_Access);
   begin
      Get_Name_String (File);
      File_Name.all := Name_Buffer (1 .. Name_Len);
      Execute (Rm, (Force, File_Name));
      Free (File_Name);
   end Unlink_File;

   ---------------------------
   -- Write_Compile_Command --
   ---------------------------

   procedure Write_Compile_Command (Name : in File_Name_Type) is
   begin
      Write_Str  (Standout, Gnatmake.all);
      Write_Str  (Standout, " -c ");
      for I in Gcc_Switches.First .. Gcc_Switches.Last loop
         Write_Str (Standout, Gcc_Switches.Table (I).all);
         Write_Str (Standout, " ");
      end loop;
      Write_Name (Standout, Name);
      Write_Eol  (Standout);
   end Write_Compile_Command;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol
     (File   : in File_Descriptor;
      Stdout : in Boolean := False) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      if EOL'Length /= Write (File, EOL'Address, EOL'Length) then
         Write_Str ("error : disk full");
         Write_Eol;
         raise Fatal_Error;
      end if;

      if Stdout then
         Write_Eol (Standout);
      end if;

   end Write_Eol;

   ----------------------
   -- Write_File_Stamp --
   ----------------------

   procedure Write_File_Stamp
     (File : in File_Name_Type) is
   begin
      Write_Str (" (");
      Write_Str (Stamp (File));
      Write_Str (")");
   end Write_File_Stamp;

   ------------------------
   -- Write_Missing_File --
   ------------------------

   procedure Write_Missing_File
     (File  : in File_Name_Type) is
   begin
      Message ("", File, "does not exist");
   end Write_Missing_File;

   -------------------
   -- Uname_To_Name --
   -------------------

   procedure Write_Name
     (File   : in File_Descriptor;
      Name   : in Name_Id;
      Stdout : in Boolean := False) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      if Name /= No_Name then
         Get_Name_String (Name);
         if Name_Buffer (Name_Len - 1) = '%' then --  %
            Name_Len := Name_Len - 2;
         end if;
         if Write (File, Name_Buffer'Address, Name_Len) /= Name_Len then
            Write_Str ("error : disk full");
            Write_Eol;
            raise Fatal_Error;
         end if;

         if Stdout then
            Write_Name (Standout, Name);
         end if;

      end if;

   end Write_Name;

   ----------------------------
   -- Write_Stamp_Comparison --
   ----------------------------

   procedure Write_Stamp_Comparison
     (Newer, Older   : in File_Name_Type) is
   begin
      Write_Program_Name;
      Write_Str (": ");
      Write_Name (Newer);
      if Debug_Mode then
         Write_File_Stamp (Newer);
      end if;
      Write_Eol;
      Write_Program_Name;
      Write_Str (":    is more recent than");
      Write_Eol;
      Write_Program_Name;
      Write_Str (": ");
      Write_Name (Older);
      if Debug_Mode then
         Write_File_Stamp (Older);
      end if;
      Write_Eol;
   end Write_Stamp_Comparison;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str
     (File   : in File_Descriptor;
      Line   : in String;
      Stdout : in Boolean := False) is
   begin

      if File = Invalid_FD then
         raise Usage_Error;
      end if;

      if Write (File, Line'Address, Line'Length) /= Line'Length then
         Write_Str ("error : disk full");
         Write_Eol;
         raise Fatal_Error;
      end if;

      if Stdout then
         Write_Str (Standout, Line);
      end if;

   end Write_Str;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (U : in Unit_Name_Type) is
   begin
      Get_Decoded_Name_String (U);
      for J in 1 .. Name_Len loop
         if Name_Buffer (J) = '-' then
            Name_Buffer (J) := '.';
         end if;
      end loop;
      Name_Len := Name_Len - 2;
      Write_Str (Name_Buffer (1 .. Name_Len));
   end Write_Unit_Name;

end XE_Utils;
