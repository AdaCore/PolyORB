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
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with Unchecked_Deallocation;

with ALI;                        use ALI;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with Csets;                      use Csets;
with Debug;                      use Debug;
with Fname;                      use Fname;
with Fname.SF;
with Fname.UF;
with Hostparm;                   use Hostparm;
with Make;                       use Make;
with Namet;                      use Namet;
with Opt;
with Osint;                      use Osint;
with Output;                     use Output;
with Prj;
with SFN_Scan;
with Snames;
with Types;                      use Types;
with XE;                         use XE;
with XE_Defs;                    use XE_Defs;
with XE_Sysdep;                  use XE_Sysdep;

with Ada.Command_Line;           use Ada.Command_Line;

pragma Elaborate_All (Csets, Debug, Make, Namet, Opt, Osint, Output);

package body XE_Utils is

   Dir_Sep : Character renames GNAT.OS_Lib.Directory_Separator;

   GNAT_Verbose   : String_Access;
   Gcc            : String_Access;
   Link           : String_Access;
   Gnatbind       : String_Access;
   Gnatlink       : String_Access;
   Gnatmake       : String_Access;

   Up_To_Low : constant := Character'Pos ('A') - Character'Pos ('a');

   EOL : constant String := (1 => ASCII.LF);

   Output_Flag           : constant String_Access := new String' ("-o");
   Symbolic              : constant String_Access := new String' ("-s");
   Compile_Flag          : constant String_Access := new String' ("-c");
   Exclude_File_Flag     : constant String_Access := new String' ("-x");
   Receiver_Compile_Flag : constant String_Access := new String' ("-gnatzr");
   Caller_Compile_Flag   : constant String_Access := new String' ("-gnatzc");

   Special_File_Flag     : constant String_Access := new String' ("-x");
   Ada_File_Flag         : constant String_Access := new String' ("ada");

   System_Tasking        : constant String  := "system.tasking";
   System_Tasking_Length : constant Natural := System_Tasking'Length;

   GARLIC                : String_Access;

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
     return String_Access;
   --  look for Exec_Name on the path. If Exec_Name is found then the full
   --  pathname for Exec_Name is returned. If Exec_Name is not found and
   --  Show_Error is set to False then null is returned. If Exec_Name is not
   --  found and Show_Error is set to True then Fatal_Error is raised.

   function Has_Standard_Extension (File : File_Name_Type) return Boolean;
   --  Check whether File has a standard extension for GCC and hence does
   --  not need the "-x ada" command line argument (typically ".ads" or
   --  ".adb" terminated file).

   procedure Add_Default_Optimization;
   --  Add the default optimization flag if Optimization_Mode is True. Turns
   --  if off afterwise.

   procedure Print_Flags;

   ---------
   -- "&" --
   ---------

   function "&"
     (N1 : Types.File_Name_Type;
      N2 : Types.File_Name_Type)
      return Types.File_Name_Type is
   begin
      Name_Len := 0;
      if N1 /= No_Name then
         Get_Name_String_And_Append (N1);
      end if;
      if N2 /= No_Name then
         Get_Name_String_And_Append (N2);
      end if;
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
      Name_Len := 0;
      if N1 /= No_Name then
         Get_Name_String_And_Append (N1);
      end if;
      Add_Str_To_Name_Buffer (N2);
      return Name_Find;
   end "&";

   ------------------------------
   -- Add_Default_Optimization --
   ------------------------------

   procedure Add_Default_Optimization is
      Opt : constant String := Get_Default_Optimization;
   begin
      if Optimization_Mode and then Opt /= "O0" then
         Scan_Make_Arg ("-" & Opt, And_Save => False);
      end if;
      Optimization_Mode := False;
   end Add_Default_Optimization;

   -------
   -- C --
   -------

   function C (S : String) return Name_Id is
   begin
      for F in S'Range loop
         if S (F) /= ' ' then
            return C (Str_To_Id (S (F .. S'Last)));
         end if;
      end loop;
      return No_Name;
   end C;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (To : in File_Name_Type) is
   begin

      if Debug_Mode then
         Message ("change to dir", To);
      end if;

      Get_Name_String (To);
      Change_Dir (Name_Buffer (1 .. Name_Len));

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

   exception
      when GNAT.Directory_Operations.Directory_Error =>
         Message ("cannot change dir to", To);
         raise Fatal_Error;
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
         (1 => Caller_Compile_Flag)
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
         (1 => Receiver_Compile_Flag)
         );
   end Compile_RCI_Receiver;

   --------------------------
   -- Copy_With_File_Stamp --
   --------------------------

   procedure Copy_With_File_Stamp
     (Source, Target : in File_Name_Type;
      Maybe_Symbolic : in Boolean := False)
   is

      S : String_Access := new String (1 .. Strlen (Source));
      T : String_Access := new String (1 .. Strlen (Target));

   begin
      Get_Name_String (Source);
      S.all := Name_Buffer (1 .. Name_Len);
      Get_Name_String (Target);
      T.all := Name_Buffer (1 .. Name_Len);

      if Link = null then
         Copy_File (S.all, T.all);

      else
         Force_Remove (T.all);

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
      Exec : in Boolean := False)
   is
      File_Name_Len : Natural := Strlen (Name);
      File_Name     : String (1 .. File_Name_Len + 1);
   begin
      Get_Name_String (Name);
      File_Name (1 .. File_Name_Len) := Name_Buffer (1 .. Name_Len);
      File_Name (File_Name_Len + 1) := ASCII.NUL;

      if Verbose_Mode then
         Message ("creating file", Name);
      end if;

      File := Create_File (File_Name'Address, Text);

      if File = Invalid_FD then
         Message ("cannot create file", Name);
         raise Fatal_Error;
      end if;

      if Exec then
         Set_Executable_Attribute (File_Name (1 .. File_Name_Len));
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
         if Dir_Name (Index) = Dir_Sep and then Index > 1 and then
            not Is_Directory (Dir_Name (1 .. Index - 1))
         then
            Make_Dir (Dir_Name (1 .. Index - 1));
         elsif Index = Dir_Name'Last then
            Make_Dir (Dir_Name);
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
      Name_Buffer (Name_Len) := ASCII.NUL;
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

      Add_Char_To_Name_Buffer (Dir_Sep);
      Get_Name_String_And_Append (D2);

      if D3 = No_File then
         return Name_Find;
      end if;

      Add_Char_To_Name_Buffer (Dir_Sep);
      Get_Name_String_And_Append (D3);

      if D4 = No_File then
         return Name_Find;
      end if;

      Add_Char_To_Name_Buffer (Dir_Sep);
      Get_Name_String_And_Append (D4);
      return Name_Find;
   end Dir;

   -----------------
   -- Dwrite_Call --
   -----------------

   procedure Dwrite_Call
     (File   : in File_Descriptor;
      Ind    : in Int;
      S1     : in String;
      N1     : in Name_Id := No_Name;
      S2     : in String  := No_Str;
      N2     : in Name_Id := No_Name;
      S3     : in String  := No_Str;
      N3     : in Name_Id := No_Name)
   is
      Id   : Integer := 0;

      procedure Dwrite_Separator (New_Id : Boolean);
      procedure Dwrite_Separator (New_Id : Boolean) is
      begin
         if New_Id then
            Id := Id + 1;
            if Id = 2 then
               Dwrite_Str (File, " (");
            elsif Id > 2 then
               Dwrite_Str (File, ", ");
            end if;
         end if;
      end Dwrite_Separator;

   begin
      for I in 1 .. Ind loop
         Dwrite_Str (File, "   ");
      end loop;
      Dwrite_Separator (S1 /= No_Str);
      Dwrite_Str  (File, S1);
      Dwrite_Separator (N1 /= No_Name);
      Dwrite_Name (File, N1);
      Dwrite_Separator (S2 /= No_Str);
      Dwrite_Str  (File, S2);
      Dwrite_Separator (N2 /= No_Name);
      Dwrite_Name (File, N2);
      Dwrite_Separator (S3 /= No_Str);
      Dwrite_Str  (File, S3);
      Dwrite_Separator (N3 /= No_Name);
      Dwrite_Name (File, N3);
      pragma Assert (Id /= 0);
      if Id /= 1 then
         Dwrite_Str (File, ")");
      end if;
      Dwrite_Str (File, ";");
      Dwrite_Eol (File);
   end Dwrite_Call;

   -----------------
   -- Dwrite_Line --
   -----------------

   procedure Dwrite_Line
     (File  : in File_Descriptor;
      Ind : in Int;
      S1  : in String;
      N1  : in Name_Id := No_Name;
      S2  : in String  := No_Str;
      N2  : in Name_Id := No_Name;
      S3  : in String  := No_Str;
      N3  : in Name_Id := No_Name) is
   begin
      for I in 1 .. Ind loop
         Dwrite_Str (File, "   ");
      end loop;
      Dwrite_Str  (File, S1);
      Dwrite_Name (File, N1);
      Dwrite_Str  (File, S2);
      Dwrite_Name (File, N2);
      Dwrite_Str  (File, S3);
      Dwrite_Name (File, N3);
      Dwrite_Eol  (File);
   end Dwrite_Line;

   ------------------------
   -- Dwrite_With_Clause --
   ------------------------

   procedure Dwrite_With_Clause
     (File : in File_Descriptor;
      Used : in Boolean;
      Unit : in Name_Id) is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("with ");
      Get_Name_String_And_Append (Unit);
      Add_Char_To_Name_Buffer (';');
      Dwrite_Str (File, Name_Buffer (1 .. Name_Len));
      Dwrite_Eol (File);

      if Used then
         Name_Buffer (1 .. 5) := "use  ";
         Dwrite_Str (File, Name_Buffer (1 .. Name_Len));
         Dwrite_Eol (File);
      end if;
   end Dwrite_With_Clause;

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

   -----------------
   -- Find_Source --
   -----------------

   function Find_Source
     (Uname : Unit_Name_Type;
      Fatal : Boolean := False)
      return File_Name_Type
   is
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
            File := UF.File_Name_Of_Spec (Name);
            if Full_Source_Name (File) /= No_File then
               return File;
            end if;
         end if;

         File := UF.File_Name_Of_Body (Name);
         if Full_Source_Name (File) /= No_File then
            return File;
         end if;

         if not Spec then
            File := UF.File_Name_Of_Spec (Name);
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

      Gcc_Switches.Init;
      Binder_Switches.Init;
      Linker_Switches.Init;

      Csets.Initialize;
      Namet.Initialize;

      Snames.Initialize;

      Prj.Initialize;

      GNATLib_Compile_Flag := new String'("-gnatg");
      Cfg_Suffix           := Str_To_Id (Get_Conf_Suffix);
      Obj_Suffix           := Str_To_Id (Get_Object_Suffix.all);
      Exe_Suffix           := Str_To_Id (Get_Executable_Suffix.all);

      Gcc             := Locate ("gcc");
      Link            := Locate ("ln", False);
      Gnatbind        := Locate ("gnatbind");
      Gnatlink        := Locate ("gnatlink");
      Gnatmake        := Locate ("gnatmake");

      ALI_Suffix     := Str_To_Id (".ali");
      ADS_Suffix     := Str_To_Id (".ads");
      ADB_Suffix     := Str_To_Id (".adb");

      Spec_Suffix    := Str_To_Id ("%s");
      Body_Suffix    := Str_To_Id ("%b");

      DSA_Dir        := Str_To_Id ("dsa");

      declare
         Private_Dir : Name_Id := Dir (DSA_Dir, Str_To_Id ("private"));
      begin
         Caller_Dir   := Dir (Private_Dir, Str_To_Id ("caller"));
         Receiver_Dir := Dir (Private_Dir, Str_To_Id ("receiver"));
      end;

      PWD_Id         := Dir (Str_To_Id ("`pwd`"), No_File);

      Build_Stamp_File     := Str_To_Id ("glade.sta");
      Elaboration_File     := Str_To_Id ("s-garela");
      Elaboration_Name     := Str_To_Id ("System.Garlic.Elaboration");
      Partition_Main_File  := Str_To_Id ("partition");
      Partition_Main_Name  := Str_To_Id ("Partition");
      Protocol_Config_File := Str_To_Id ("s-gaprco");
      Protocol_Config_Name := Str_To_Id ("System.Garlic.Protocols.Config");
      Storage_Config_File  := Str_To_Id ("s-gastco");
      Storage_Config_Name  := Str_To_Id ("System.Garlic.Storages.Config");

      GARLIC := Getenv ("GLADE_LIBRARY_DIR");
      if GARLIC = null
        or else GARLIC'Length = 0
      then
         GARLIC := Get_GARLIC_Dir;
      end if;

      I_Current_Dir := new String'("-I" & Normalized_CWD);
      L_Current_Dir := new String'("-L" & Normalized_CWD);

      Name_Len := 2;
      Name_Buffer (1 .. 2) := "-I";
      Get_Name_String_And_Append (Caller_Dir);
      I_Caller_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name_Buffer (2) := 'L';
      L_Caller_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Name_Len := 2;
      Name_Buffer (1 .. 2) := "-I";
      Add_Str_To_Name_Buffer (GARLIC.all);
      I_GARLIC_Dir := new String'(Name_Buffer (1 .. Name_Len));

      --  gnatmake has its own set of flags. gnatdist parses them
      --  first and then passes them to gnatmake. gnatmake keeps also
      --  a global variable Program_Args indicating the kind of flags
      --  it is currently parsing (compiler, binder, linker,
      --  none). Once a flag -[cbl]args is parsed, this global
      --  variable is not set to none any more. To add garlic dir in
      --  the include flags for the compiler and the binder we have to
      --  force the parsing of -[cb]args because the user may have
      --  pass those flags in the command line changing Program_Args
      --  to something different from none.

      Optimization_Mode := True;
      for I in 1 .. Argument_Count loop
         declare
            Argv : String := Argument (I);
         begin
            if Argv (1) = '-' then
               if Argv'Length >= 2
                 and then Argv (2) = 'g'
                 and then (Argv'Length < 5
                           or else Argv (2 .. 5) /= "gnat")
               then
                  Optimization_Mode := False;
               elsif Argv'Length >= 2
                 and then Argv (2) = 'O'
               then
                  Optimization_Mode := False;
               end if;
            end if;
            Scan_Make_Arg (Argv, And_Save => False);
         end;
      end loop;

      Add_Default_Optimization;

      declare
         Primary_Dir : String_Ptr;
      begin

         --  We must add GARLIC in the src and lib search dirs. But
         --  when we want to use an internal GARLIC file from the
         --  current directory (or somewhere else), we must add -I- to
         --  remove the primary directory (GARLIC directory here).

         if Opt.Look_In_Primary_Dir then
            Primary_Dir :=
              Normalize_Directory_Name (Get_Primary_Src_Search_Directory.all);
         end if;
         if Number_Of_Files > 0 then
            Scan_Make_Arg ("-cargs", And_Save => False);
            if Primary_Dir /= null then
               Scan_Make_Arg ("-I" & Primary_Dir.all, And_Save => False);
               Scan_Make_Arg ("-I-", And_Save => False);
            end if;
            Scan_Make_Arg (I_GARLIC_Dir.all, And_Save => False);
            Scan_Make_Arg ("-bargs", And_Save => False);
         end if;
         if Primary_Dir /= null then
            Scan_Make_Arg ("-I" & Primary_Dir.all, And_Save => False);
            Scan_Make_Arg ("-I-", And_Save => False);
         end if;
         Scan_Make_Arg (I_GARLIC_Dir.all, And_Save => False);
      end;

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
      Quiet_Mode         := Opt.Quiet_Output;
      No_Recompilation   := Opt.Do_Not_Execute;
      Building_Script    := Opt.List_Dependencies;

      --  Use -dq and -ds for Gnatdist internal debugging.
      Debug.Debug_Flag_Q := False;
      Debug.Debug_Flag_S := False;

      --  Don't want log messages that would corrupt scripts.
      if Building_Script then
         Verbose_Mode := False;
         Quiet_Mode := True;
      end if;

      Opt.Check_Source_Files := False;
      Opt.All_Sources        := False;

      if Verbose_Mode then
         GNAT_Verbose := new String' ("-v");
      else
         GNAT_Verbose := new String' ("-q");
      end if;

      --  Read gnat.adc file to initialize Fname.UF

      Fname.UF.Initialize;
      begin
         Fname.SF.Read_Source_File_Name_Pragmas;
      exception
         when SFN_Scan.Syntax_Error_In_GNAT_ADC =>
            Osint.Fail ("syntax error in gnat.adc.");
      end;

      if Debug_Mode then
         Print_Flags;
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
        (Name_Buffer (1) /= Dir_Sep
         and then Name_Buffer (1) /= '/');
   end Is_Relative_Dir;

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

   ------------
   -- Locate --
   ------------

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
      return String_Access
   is
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

   -----------------
   -- Print_Flags --
   -----------------

   procedure Print_Flags is
   begin
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
   end Print_Flags;

   -----------
   -- Quote --
   -----------

   function Quote
     (N : Name_Id;
      I : Natural  := 0;
      S : Positive := 48)
     return Name_Id
   is
      F : Natural;
      L : Natural;

   begin
      Name_Len := 0;
      Add_Char_To_Name_Buffer ('"'); -- "
      if N /= No_Name then
         Get_Name_String_And_Append (N);
         F := 2;
         L := Name_Len;
         for T in 1 .. (Name_Len - 1) / S loop
            F := F + S;
            Name_Buffer (F + 5 + 3 * I .. L + 5 + 3 * I)
              := Name_Buffer (F .. L);
            Name_Buffer (F)     := '"'; -- "
            Name_Buffer (F + 1) := ' ';
            Name_Buffer (F + 2) := '&';
            Name_Buffer (F + 3) := ASCII.LF;
            for J in 0 .. I - 1 loop
               Name_Buffer (F + 4 + 3 * J .. F + 4 + 3 * (J + 1)) := "   ";
            end loop;
            Name_Buffer (F + 4 + 3 * I) := '"'; -- "
            F := F + 5 + 3 * I;
            L := L + 5 + 3 * I;
         end loop;
         Name_Len := L;
      end if;
      Add_Char_To_Name_Buffer ('"'); -- "
      return Name_Find;
   end Quote;

   ----------------------
   -- Remove_GNAT_Flag --
   ----------------------

   procedure Remove_GNAT_Flag (Flag : in String) is
      L : constant Natural := 5 + Flag'Length;
      F : String (1 .. L);
      O : Natural := 0;
      --  Occurrences of GNAT Flag in Gcc_Switches.
   begin
      F (1 .. 5) := "-gnat";
      F (6 .. L) := Flag;
      for I in Gcc_Switches.First .. Gcc_Switches.Last loop
         declare
            S : String_Access renames Gcc_Switches.Table (I);
         begin
            if L <= S'Length
              and then S (S'First .. S'First + L - 1) = F
            then
               O := O + 1;
            else
               Gcc_Switches.Table (I - O) := Gcc_Switches.Table (I);
            end if;
         end;
      end loop;
      for I in 1 .. O loop
         Gcc_Switches.Decrement_Last;
      end loop;
   end Remove_GNAT_Flag;

   -------
   -- S --
   -------

   function S (X : String) return Name_Id is
   begin
      return S (Str_To_Id (X));
   end S;

   -------
   -- S --
   -------

   function S (N : Name_Id) return Name_Id is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("System.");
      Get_Name_String_And_Append (N);
      return Name_Find;
   end S;

   --------
   -- SG --
   --------

   function SG (X : String) return Name_Id is
   begin
      return SG (Str_To_Id (X));
   end SG;

   --------
   -- SG --
   --------

   function SG (N : Name_Id) return Name_Id is
      CN : Name_Id := C (N);
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("Garlic.");
      Get_Name_String_And_Append (CN);
      return S (Name_Find);
   end SG;

   ---------
   -- SGF --
   ---------

   function SGF (X : String) return Name_Id is
   begin
      return SGF (Str_To_Id (X));
   end SGF;

   ---------
   -- SGP --
   ---------

   function SGP (X : String) return Name_Id is
   begin
      return SGP (Str_To_Id (X));
   end SGP;

   ---------
   -- SGP --
   ---------

   function SGP (N : Name_Id) return Name_Id is
      CN : Name_Id := C (N);
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("Protocols.");
      Get_Name_String_And_Append (CN);
      return SG (Name_Find);
   end SGP;

   ---------
   -- SGF --
   ---------

   function SGF (N : Name_Id) return Name_Id is
      CN : Name_Id := C (N);
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("Filters.");
      Get_Name_String_And_Append (CN);
      return SG (Name_Find);
   end SGF;

   ---------
   -- SGS --
   ---------

   function SGS (X : String) return Name_Id is
   begin
      return SGS (Str_To_Id (X));
   end SGS;

   ---------
   -- SGS --
   ---------

   function SGS (N : Name_Id) return Name_Id is
      CN : Name_Id := C (N);
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("Storages.");
      Get_Name_String_And_Append (CN);
      return SG (Name_Find);
   end SGS;

   -----------------------
   -- Source_File_Error --
   -----------------------

   procedure Source_File_Error (Uname : Unit_Name_Type) is
   begin
      Message ("no source file for unit", Quote (U_To_N (Uname)));
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

   -------------------------
   -- System_Tasking_Child --
   -------------------------

   function System_Tasking_Child (N : Types.Name_Id) return Boolean is
   begin
      Get_Name_String (N);
      return System_Tasking_Length <= Name_Len
        and then System_Tasking = Name_Buffer (1 .. System_Tasking_Length);
   end System_Tasking_Child;

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
   begin
      Get_Name_String (File);
      Force_Remove (Name_Buffer (1 .. Name_Len));
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

   ----------------
   -- Write_Name --
   ----------------

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
