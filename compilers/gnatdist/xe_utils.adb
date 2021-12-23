------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             X E _ U T I L S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2021, Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Directories;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Platform;
with Utils;    use Utils;

with XE_Defs;          use XE_Defs;
with XE_Flags;         use XE_Flags;
with XE_IO;            use XE_IO;
with XE_Names;         use XE_Names;

package body XE_Utils is

   type Name_Array is array (Natural range <>) of Name_Id;
   type Name_Array_Ptr is access Name_Array;

   Main_Sources        : Name_Array_Ptr;
   Current_Main_Source : Natural := 1;
   Last_Main_Source    : Natural := 0;

   type Make_Program_Type is (None, Compiler, Binder, Linker);

   Program_Args : Make_Program_Type := None;
   --  Used to indicate if we are scanning gnatmake, gcc, gnatbind, or
   --  gnatbind options within the gnatmake command line.

   procedure Ensure_Make_Args;
   --  Reset Program_Args to None, adding "-margs" to make switches if needed

   Project_File_Name_Expected : Boolean := False;
   --  Used to keep state between invocations of Scan_Dist_Arg. True when
   --  previous argument was "-P".

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup);

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);
   pragma Import (C, Dup2);

   GNAT_Driver : GNAT.OS_Lib.String_Access;
   GPRBuild    : GNAT.OS_Lib.String_Access;

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True) return GNAT.OS_Lib.String_Access;
   --  look for Exec_Name on the path. If Exec_Name is found then the full
   --  pathname for Exec_Name is returned. If Exec_Name is not found and
   --  Show_Error is set to False then null is returned. If Exec_Name is not
   --  found and Show_Error is set to True then Fatal_Error is raised.

   procedure Add_Make_Switch (Argv : Unbounded_String);
   procedure Add_Make_Switch (Argv : String);

   procedure Add_List_Switch (Argv : Unbounded_String);
   procedure Add_List_Switch (Argv : String);

   procedure Add_Main_Source (Source : String);
   procedure Add_Source_Directory (Argv : String);

   procedure Fail
     (S1 : String;
      S2 : String := No_Str;
      S3 : String := No_Str);

   type Sigint_Handler is access procedure;
   pragma Convention (C, Sigint_Handler);

   procedure Install_Int_Handler (Handler : Sigint_Handler);
   pragma Import (C, Install_Int_Handler, "__gnat_install_int_handler");
   --  Called by Gnatmake to install the SIGINT handler below

   procedure Sigint_Intercepted;
   pragma Convention (C, Sigint_Intercepted);
   --  Called when the program is interrupted by Ctrl-C to delete the
   --  temporary mapping files and configuration pragmas files.

   procedure Check_User_Provided_S_RPC (Dir : String);
   --  Check whether the given directory contains a user-provided version of
   --  s-rpc.adb, and if so set the global flag User_Provided_S_RPC to True.

   function Is_Project_Switch (S : Unbounded_String) return Boolean;
   --  True if S is a builder command line switch specifying a project file

   procedure Check_Section_Argument
     (Arg        : String;
      Is_Section : out Boolean;
      Program    : in out Make_Program_Type);
   --  Determine whether Arg is a section switch (-Xarg), and if so
   --  set Is_Section to True, and Program to the corresponding value.
   --  If not, leave Program unchanged.

   procedure Copy_Build_Arguments
     (From     : Argument_Vec;
      To       : in out Argument_Vec;
      Has_Prj  : out Boolean);
   --  Copy command line arguments from From to To.
   --  Upon exit, "-margs" is also appended to To if From changed the section.
   --  Has_Prj is set True if From contains a -P flag.

   ---------
   -- "&" --
   ---------

   function "&"
     (L : File_Name_Type;
      R : File_Name_Type)
     return File_Name_Type is
   begin
      Name_Len := 0;
      if Present (L) then
         Get_Name_String_And_Append (L);
      end if;
      if Present (R) then
         Get_Name_String_And_Append (R);
      end if;
      return Name_Find;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (L : File_Name_Type;
      R : String) return File_Name_Type
   is
   begin
      Name_Len := 0;
      if Present (L) then
         Get_Name_String_And_Append (L);
      end if;
      Add_Str_To_Name_Buffer (R);
      return Name_Find;
   end "&";

   ---------------------
   -- Add_List_Switch --
   ---------------------

   procedure Add_List_Switch (Argv : Unbounded_String) is
   begin
      Push (List_Switches, Argv);
   end Add_List_Switch;

   procedure Add_List_Switch (Argv : String) is
   begin
      Push (List_Switches, Argv);
   end Add_List_Switch;

   ---------------------
   -- Add_Main_Source --
   ---------------------

   procedure Add_Main_Source (Source : String) is
   begin
      if Main_Sources = null then
         Main_Sources := new Name_Array (1 .. Argument_Count);
      end if;
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Source);
      Last_Main_Source := Last_Main_Source + 1;
      Main_Sources (Last_Main_Source) := Name_Find;
   end Add_Main_Source;

   ---------------------
   -- Add_Make_Switch --
   ---------------------

   procedure Add_Make_Switch (Argv : Unbounded_String) is
   begin
      Push (Make_Switches, Argv);
   end Add_Make_Switch;

   ---------------------
   -- Add_Make_Switch --
   ---------------------

   procedure Add_Make_Switch (Argv : String) is
   begin
      Push (Make_Switches, Argv);
   end Add_Make_Switch;

   --------------------------
   -- Add_Source_Directory --
   --------------------------

   procedure Add_Source_Directory (Argv : String) is
   begin
      Check_User_Provided_S_RPC (Argv);
      Push (Source_Directories, Argv);
   end Add_Source_Directory;

   -----------
   -- Build --
   -----------

   procedure Build
     (Library   : File_Name_Type;
      Arguments : Argument_Vec;
      Fatal     : Boolean := True;
      Progress  : Boolean := False)
   is
      Flags      : Argument_Vec;
      Success    : Boolean;
      Has_Prj    : Boolean := False;
      Skip       : Boolean;
      Builder    : GNAT.OS_Lib.String_Access;

   begin
      if Use_GPRBuild then
         Builder := GPRBuild;

      else
         Builder := GNAT_Driver;

         --  gnat make

         Push (Flags, "make");
      end if;

      if Quiet_Mode then
         --  Pass -q to gnatmake

         Push (Flags, Quiet_Flag);

      elsif Verbose_Mode then
         --  Pass -v to gnatmake

         Push (Flags, Verbose_Flag);
      end if;

      if Progress then
         --  Pass -d to gnatmake

         Push (Flags, Progress_Flag);
      end if;

      --  Library file name (free'd at exit of Compile, must record position
      --  in Flags array).

      Push (Flags, Library);

      --  Copy additional arguments

      Copy_Build_Arguments
        (From => Arguments, To => Flags, Has_Prj => Has_Prj);

      Skip := False;
      for Make_Switch of Make_Switches loop
         if Skip then
            Skip := False;

         --  If there is a project file among the arguments then any project
         --  file from the Make switches is ignored.

         elsif Has_Prj
           and then Is_Project_Switch (Make_Switch)
         then
            if Make_Switch = Project_File_Flag then
               --  Case of "-P" followed by project file name in a separate
               --  argument.

               Skip := True;
            end if;

         else
            Push (Flags, Make_Switch);
         end if;
      end loop;

      --  Call gnat make

      Execute (Builder.all, Flags, Success);

      if not Success and then Fatal then
         raise Compilation_Error;
      end if;
   end Build;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (N : Name_Id) return Name_Id is
   begin
      Get_Name_String (N);
      Capitalize (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end Capitalize;

   function Capitalize (S : String) return String is
      R : String := S;
   begin
      Capitalize (R);
      return R;
   end Capitalize;

   procedure Capitalize (S : in out String) is
      Capitalized : Boolean := True;

   begin
      for J in S'Range loop
         if S (J) in 'a' .. 'z' then
            if Capitalized then
               S (J) := To_Upper (S (J));
            end if;
            Capitalized := False;

         elsif S (J) in 'A' .. 'Z' then
            if not Capitalized then
               S (J) := To_Lower (S (J));
            end if;
            Capitalized := False;

         elsif S (J) = '_' or else S (J) = '.' then
            Capitalized := True;
         end if;
      end loop;
   end Capitalize;

   ----------------------------
   -- Check_Section_Argument --
   ----------------------------

   procedure Check_Section_Argument
     (Arg        : String;
      Is_Section : out Boolean;
      Program    : in out Make_Program_Type)
   is
   begin
      if Arg'Length = 6
        and then Arg (Arg'First) = '-'
        and then Arg (Arg'First + 2 .. Arg'Last) = "args"
      then
         case Arg (Arg'First + 1) is
            when 'c' => Program := Compiler;
            when 'b' => Program := Binder;
            when 'l' => Program := Linker;
            when 'm' => Program := None;
            when others =>
               raise Program_Error
                 with "unexpected switch " & Arg;
         end case;
         Is_Section := True;
      else
         Is_Section := False;
      end if;
   end Check_Section_Argument;

   -------------------------------
   -- Check_User_Provided_S_RPC --
   -------------------------------

   procedure Check_User_Provided_S_RPC (Dir : String) is
   begin
      --  Special kludge: if the user provides his own version of s-rpc, the
      --  PCS should not provide it.

      if Is_Readable_File (Dir & Dir_Separator & "s-rpc.adb") then
         XE_Flags.User_Provided_S_RPC := True;
      end if;
   end Check_User_Provided_S_RPC;

   --------------------------
   -- Copy_Build_Arguments --
   --------------------------

   procedure Copy_Build_Arguments
     (From     : Argument_Vec;
      To       : in out Argument_Vec;
      Has_Prj  : out Boolean)
   is
      Is_Section : Boolean;

      Prog : Make_Program_Type := None;

   begin
      for Arg of From loop
         Push (To, Arg);

         --  Detect any project file

         if Is_Project_Switch (Arg) then
            Has_Prj := True;
         end if;

         --  Record section changes

         Check_Section_Argument (To_String (Arg), Is_Section, Prog);
      end loop;

      if Prog /= None then
         Push (To, "-margs");
      end if;
   end Copy_Build_Arguments;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Source    : File_Name_Type;
      Arguments : Argument_Vec;
      Fatal     : Boolean := True)
   is
      Flags   : Argument_Vec;
      Success : Boolean;
      Has_Prj : Boolean := False;
      Skip    : Boolean;

   begin
      --  gnat compile

      Push (Flags, "compile");

      --  Source file name (as provided by "gnat list")

      Push (Flags, Get_Name_String (Source));

      if Quiet_Mode then
         --  Pass -q to gnatmake

         Push (Flags, Quiet_Flag);

      elsif Verbose_Mode then
         --  Pass -v to gnatmake

         Push (Flags, Verbose_Flag);
      end if;

      --  Copy additional arguments

      Copy_Build_Arguments
        (From => Arguments, To => Flags, Has_Prj => Has_Prj);

      Skip := False;
      for Make_Switch of Make_Switches loop
         --  If there is a project file among the arguments then any project
         --  file from the Make switches is ignored.

         if Skip then
            Skip := False;

         elsif Has_Prj
           and then Is_Project_Switch (Make_Switch)
         then
            if Make_Switch = Project_File_Flag then

               --  Case of "-P" followed by project file name in a separate
               --  argument.

               Skip := True;
            end if;

         else
            Push (Flags, Make_Switch);
         end if;
      end loop;

      Execute (GNAT_Driver.all, Flags, Success);

      if not Success and then Fatal then
         raise Compilation_Error;
      end if;
   end Compile;

   ----------------------
   -- Ensure_Make_Args --
   ----------------------

   procedure Ensure_Make_Args is
   begin
      if Program_Args /= None then
         Add_Make_Switch ("-margs");
         Program_Args := None;
      end if;
   end Ensure_Make_Args;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Command   : String;
      Arguments : Argument_Vec;
      Success   : out Boolean)
   is
      Arg_List : Argument_List (1 .. Integer (Arguments.Length));
   begin
      for J in Arg_List'Range loop
         Arg_List (J) := new String'(To_String (Arguments.Element (J)));
      end loop;

      if not Quiet_Mode then
         Set_Standard_Error;
         Write_Str (Command);
         for J in Arg_List'Range loop
            Write_Str (" ");
            Write_Str (Arg_List (J).all);
         end loop;
         Write_Eol;
         Set_Standard_Output;
      end if;

      Spawn (Command, Arg_List, Success);
      for Arg of Arg_List loop
         Free (Arg);
      end loop;
   end Execute;

   ------------------
   -- Exit_Program --
   ------------------

   procedure Exit_Program (Code : Exit_Code_Type) is
      Status : Integer := 0;

   begin
      Remove_All_Temp_Files;
      if Code /= E_Success then
         Status := 1;
      end if;
      OS_Exit (Status);
   end Exit_Program;

   ----------
   -- Fail --
   ----------

   procedure Fail
     (S1 : String;
      S2 : String := No_Str;
      S3 : String := No_Str) is
   begin
      Write_Program_Name;
      Write_Str (": ");
      Write_Str (S1);
      Write_Str (S2);
      Write_Str (S3);
      Write_Eol;
      raise Usage_Error;
   end Fail;

   --------
   -- Id --
   --------

   function Id (S : String) return Name_Id is
   begin
      if S'Length = 0 then
         return No_Name;
      end if;
      Name_Buffer (1 .. S'Length) := S;
      Name_Len := S'Length;

      return Name_Find;
   end Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      XE_Names.Initialize;
      Set_Space_Increment (3);

      Cfg_Suffix_Id  := Id (Cfg_Suffix);
      Obj_Suffix_Id  := Id (Obj_Suffix);
      Exe_Suffix_Id  := Id (Exe_Suffix);
      ALI_Suffix_Id  := Id (ALI_Suffix);
      ADB_Suffix_Id  := Id (ADB_Suffix);
      ADS_Suffix_Id  := Id (ADS_Suffix);
      Set_Str_To_Name_Buffer (Ada.Directories.Current_Directory);
      Curdir_Id      := Name_Find;
      Root_Id        := Dir (Curdir_Id, Id (Root));
      Root_Id        := Dir (Root_Id, Id (Platform.Target));
      Part_Dir_Name  := Dir (Root_Id, Id ("partitions"));
      Stub_Dir_Name  := Dir (Root_Id, Id ("stubs"));
      Stub_Dir       := +Name_Buffer (1 .. Name_Len);
      PWD_Id         := Dir (Id ("`pwd`"), No_File_Name);
      I_Current_Dir  := +"-I.";
      E_Current_Dir  := +"-I-";

      Monolithic_Obj_Dir := Dir (Root_Id, Id ("obj"));
      Hidden_Stubs_Dir   := Dir (Monolithic_Obj_Dir, Id ("monolithic"));

      PCS_Project        := Id ("pcs_project");
      Set_Corresponding_Project_File_Name (PCS_Project_File);

      Part_Main_Spec_Name := Id ("partition" & ADS_Suffix);
      Part_Main_Body_Name := Id ("partition" & ADB_Suffix);
      Part_Main_ALI_Name := To_Afile (Part_Main_Body_Name);
      Part_Main_Obj_Name := To_Ofile (Part_Main_Body_Name);

      Part_Prj_File_Name := Id ("partition.gpr");

      Overridden_PCS_Units := Id ("pcs_excluded.lst");

      Set_Str_To_Name_Buffer ("-aO");
      Get_Name_String_And_Append (Stub_Dir_Name);
      A_Stub_Dir := +Name_Buffer (1 .. Name_Len);

      for J in 1 .. Argument_Count loop
         Scan_Dist_Arg (Argument (J), Implicit => False);
      end loop;

      if Project_File_Name_Expected then
         Fail ("project file name missing after -P");
      end if;

      if Check_Readonly_Files
        and then Project_File_Name = Null_Unbounded_String
      then
         --  If the user asks for recompilation of files with read-only ALIs
         --  (in practice recompilation of the GNAT runtime), and no project
         --  has been provided, then assume that additional files to be
         --  recompiled won't be covered by the generated project, and
         --  pass extra flag to gnatmake to allow compiling them anyway.

         Ensure_Make_Args;
         Add_Make_Switch (To_String (External_Units_Flag));
      end if;

      XE_Defs.Initialize;

      Install_Int_Handler (Sigint_Intercepted'Access);

      Create_Dir (Monolithic_Obj_Dir);
      Create_Dir (Hidden_Stubs_Dir);
      Create_Dir (Stub_Dir_Name);
      Create_Dir (Part_Dir_Name);

      if Platform.Is_Cross then
         GNAT_Driver := Locate (Platform.Target & "-gnat");
      else
         GNAT_Driver := Locate ("gnat");
      end if;

      --  Use gprbuild by default if available. This is consistent
      --  with the behaviour of the "gnat" driver. Note that the
      --  default can be overridden using debugging switch -dM.

      GPRBuild := Locate ("gprbuild", Show_Error => False);
      Use_GPRBuild := GPRBuild /= null;

      Check_User_Provided_S_RPC (".");
   end Initialize;

   -----------------------
   -- Is_Project_Switch --
   -----------------------

   function Is_Project_Switch (S : Unbounded_String) return Boolean is
      St : String renames To_String (S);
      Fl : String renames To_String (Project_File_Flag);
   begin
      return St'Length >= Fl'Length
               and then St (St'First .. St'First + Fl'Length - 1) = Fl;
   end Is_Project_Switch;

   ----------
   -- List --
   ----------

   procedure List
     (Sources   : File_Name_List;
      Arguments : Argument_Vec;
      Output    : out File_Name_Type;
      Fatal     : Boolean := True)
   is
      Flags   : Argument_Vec;
      File    : GNAT.OS_Lib.File_Descriptor;
      Success : Boolean;
      Result  : File_Name_Type := No_File_Name;
      Has_Prj : Boolean := False;
      Skip    : Boolean;

      Saved_Standout : File_Descriptor;

   begin
      --  gnat list

      Push (Flags, "list");

      --  Source file names

      for J in Sources'Range loop
         Push (Flags, Sources (J));
      end loop;

      for Arg of Arguments loop
         Push (Flags, Arg);

         --  Detect any project file

         if Is_Project_Switch (Arg) then
            Has_Prj := True;
         end if;
      end loop;

      Skip := False;
      for List_Switch of List_Switches loop
         if Skip then
            Skip := False;

         --  If there is a project file among the arguments then any
         --  project file from the List switches is ignored.

         elsif Has_Prj and then Is_Project_Switch (List_Switch) then
            if List_Switch = Project_File_Flag then

               --  Case of "-P" followed by project file name in a separate
               --  argument.

               Skip := True;
            end if;

         else
            Push (Flags, List_Switch);
         end if;
      end loop;

      Register_Temp_File (File, Result);
      Saved_Standout := Dup (Standout);
      Dup2 (File, Standout);

      Execute (GNAT_Driver.all, Flags, Success);

      Dup2 (Saved_Standout, Standout);
      Close (Saved_Standout);

      if not Success then
         if Fatal then
            raise Program_Error;
         end if;
         Remove_Temp_File (Result);
      end if;

      Output := Result;
   end List;

   ------------
   -- Locate --
   ------------

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
      return GNAT.OS_Lib.String_Access
   is
      Loc : constant GNAT.OS_Lib.String_Access :=
              GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
   begin
      if Loc = null and then Show_Error then
         raise Fatal_Error with Exec_Name & " not found on PATH";
      end if;
      return Loc;
   end Locate;

   -----------------------
   -- More_Source_Files --
   -----------------------

   function More_Source_Files return Boolean is
   begin
      return Current_Main_Source <= Last_Main_Source;
   end More_Source_Files;

   ----------
   -- Name --
   ----------

   function Name (N : Name_Id) return Name_Id is
   begin
      Get_Name_String (N);
      if Name_Len > 1
        and then Name_Buffer (Name_Len - 1) = '%'
      then
         Name_Len := Name_Len - 2;
         return Name_Find;
      end if;
      return N;
   end Name;

   ----------------------
   -- Next_Main_Source --
   ----------------------

   function Next_Main_Source return Name_Id is
      Source : Name_Id := No_Name;

   begin
      if Current_Main_Source <= Last_Main_Source then
         Source := Main_Sources (Current_Main_Source);
         Current_Main_Source := Current_Main_Source + 1;
      end if;
      return Source;
   end Next_Main_Source;

   --------
   -- No --
   --------

   function No (N : Name_Id) return Boolean is
   begin
      return N = No_Name;
   end No;

   ---------------------
   -- Number_Of_Files --
   ---------------------

   function Number_Of_Files return Natural is
   begin
      return Last_Main_Source;
   end Number_Of_Files;

   -------------
   -- Present --
   -------------

   function Present (N : Name_Id) return Boolean is
   begin
      return N /= No_Name;
   end Present;

   ----------
   -- Push --
   ----------

   procedure Push (AV : in out Argument_Vec; U : Unbounded_String) is
   begin
      AV.Append (U);
   end Push;

   procedure Push (AV : in out Argument_Vec; S : String) is
   begin
      AV.Append (To_Unbounded_String (S));
   end Push;

   procedure Push (AV : in out Argument_Vec; N : Name_Id) is
   begin
      AV.Append (To_Unbounded_String (Get_Name_String (N)));
   end Push;

   -----------
   -- Quote --
   -----------

   function Quote (N : Name_Id) return Name_Id is
   begin
      Name_Len := 0;
      Add_Char_To_Name_Buffer ('"'); -- "
      if Present (N) then
         Get_Name_String_And_Append (N);
      end if;
      Add_Char_To_Name_Buffer ('"'); -- "
      return Name_Find;
   end Quote;

   -------------------
   -- Scan_Dist_Arg --
   -------------------

   procedure Scan_Dist_Arg (Argv : String; Implicit : Boolean := True) is
      Is_Section : Boolean;
   begin
      if Argv'Length = 0 then
         return;
      end if;

      Check_Section_Argument (Argv, Is_Section, Program_Args);
      if Is_Section then
         Add_Make_Switch (Argv);
         return;
      end if;

      case Program_Args is
         when Compiler | Binder | Linker =>
            Add_Make_Switch (Argv);
            return;
         when others =>
            null;
      end case;

      if Project_File_Name_Expected then
         Project_File_Name :=
           +Normalize_Pathname (Argv,
                                Resolve_Links => Resolve_Links);
         Add_List_Switch (Project_File_Name);
         Add_Make_Switch (Project_File_Name);
         Project_File_Name_Expected := False;

      elsif Argv (Argv'First) = '-' then

         if Argv'Length = 1 then
            Fail ("switch character cannot be followed by a blank");

         --  Processing for -I-

         elsif Argv = "-I-" then
            if not Use_GPRBuild then
               Add_List_Switch (Argv);
               Add_Make_Switch (Argv);
            end if;

         --  Forbid -?- or -??- where ? is any character

         elsif Argv'Length in 3 .. 4 and then Argv (Argv'Last) = '-' then
            Fail ("Trailing ""-"" at the end of ", Argv, " forbidden.");

         --  Processing for -Adir, -Idir and -Ldir

         elsif Argv (Argv'First + 1) = 'A'
           or else Argv (Argv'First + 1) = 'I'
           or else Argv (Argv'First + 1) = 'L'
         then
            if Use_GPRBuild then
               Add_List_Switch (Argv);
               Add_Make_Switch (Argv);
            end if;

            if Argv (Argv'First + 1) = 'I' and then not Implicit then
               Add_Source_Directory (Argv (Argv'First + 2 .. Argv'Last));
            end if;

         --  Processing for -aIdir, -aLdir, -aOdir, -aPdir

         elsif Argv'Length >= 3
           and then Argv (Argv'First + 1) = 'a'
           and then (Argv (Argv'First + 2) = 'I'
             or else Argv (Argv'First + 2) = 'L'
             or else Argv (Argv'First + 2) = 'O'
             or else Argv (Argv'First + 2) = 'P')
         then
            Add_List_Switch (Argv);
            Add_Make_Switch (Argv);

            if Argv (Argv'First + 2) = 'I' and then not Implicit then
               Add_Source_Directory (Argv (Argv'First + 3 .. Argv'Last));
            end if;

         elsif Argv (Argv'First + 1) = 'P' then

            if Project_File_Name_Expected
              or else Project_File_Name /= Null_Unbounded_String
            then
               Fail ("cannot have several project files specified");
            end if;

            if Argv'Length > 2 then
               Project_File_Name :=
                 +Normalize_Pathname
                    (Argv (Argv'First + 2 .. Argv'Last),
                     Resolve_Links => Resolve_Links);
               Add_List_Switch (Project_File_Flag);
               Add_List_Switch (Project_File_Name);

               Add_Make_Switch (Project_File_Flag);
               Add_Make_Switch (Project_File_Name);

            else
               Project_File_Name_Expected := True;
               Add_List_Switch (Project_File_Flag);
               Add_Make_Switch (Project_File_Flag);
            end if;

         elsif Argv (Argv'First + 1) = 'e' then
            Add_List_Switch (Argv);
            Add_Make_Switch (Argv);

            if Argv'Length = 3 and then Argv (Argv'Last) = 'L' then
               Resolve_Links := True;
            end if;

         --  Debugging switches

         elsif Argv (Argv'First + 1) = 'd' then

            --  -d: debugging traces

            if Argv'Length = 2 then
               Display_Compilation_Progress := True;

            else
               case Argv (Argv'First + 2) is
                  --  -dd: debug mode

                  when 'd' =>
                     Debug_Mode := True;

                  --  -df: output base names only in error messages (to ensure
                  --       constant output for testsuites).

                  when 'f' =>
                     Add_Make_Switch ("-df");

                  --  -dM: revert to using gnatmake even if gprbuild is present

                  when 'M' =>
                     Use_GPRBuild := False;

                  when others =>
                     --  Pass other debugging flags to the builder untouched

                     Add_Make_Switch (Argv);
               end case;
            end if;

         --  Processing for one character switches

         elsif Argv'Length = 2 then
            case Argv (Argv'First + 1) is
               when 'a' =>
                  Check_Readonly_Files := True;
                  Add_List_Switch (Argv);
                  Add_Make_Switch (Argv);

               when 'k' =>
                  Keep_Going := True;
                  Add_Make_Switch (Argv);

               when 't' =>
                  Keep_Tmp_Files := True;
                  Add_Make_Switch ("-dn");

               when 'r' =>
                  Relocatable_Starter := True;

               when 'q' =>
                  Quiet_Mode := True;
                  --  Switch is passed to gnatmake later on

               when 'v' =>
                  Verbose_Mode := True;
                  --  Switch is passed to gnatmake later on

               when others =>

                  --  Pass unrecognized one character switches to gnat make

                  Add_Make_Switch (Argv);
            end case;

         --  Processing for --PCS=

         elsif Starts_With (Argv, "--PCS=") then
            Set_PCS_Name (Argv (Argv'First + 6 .. Argv'Last));

         --  Switches to be passed to builder and lister

         elsif Starts_With (Argv, "--RTS=")
                 or else
               Argv = "--unchecked-shared-lib-imports"
         then
            Add_List_Switch (Argv);
            Add_Make_Switch (Argv);

         --  Pass all other unrecognized switches to gnat make

         else
            Add_Make_Switch (Argv);
         end if;

      else
         Add_Main_Source (Argv);
      end if;
   end Scan_Dist_Arg;

   --------------------
   -- Scan_Dist_Args --
   --------------------

   procedure Scan_Dist_Args (Args : String) is
      Argv : Argument_List_Access := Argument_String_To_List (Args);
   begin
      --  We have already processed the user command line: we might be in the
      --  -cargs or -largs section. If so, switch back to -margs now.

      Ensure_Make_Args;
      for J in Argv'Range loop
         if Argv (J)'Length > 0 then
            Scan_Dist_Arg (Argv (J).all);
         end if;
      end loop;
      Free (Argv);
   end Scan_Dist_Args;

   -----------------------------------------
   -- Set_Corresponding_Project_File_Name --
   -----------------------------------------

   procedure Set_Corresponding_Project_File_Name (N : out File_Name_Type) is
   begin
      Add_Str_To_Name_Buffer (".gpr");
      N := Name_Find;
   end Set_Corresponding_Project_File_Name;

   --------------------
   -- Show_Dist_Args --
   --------------------

   procedure Show_Dist_Args is
   begin
      for Sw of Make_Switches loop
         Message ("make = " & To_String (Sw));
      end loop;

      for Sw of List_Switches loop
         Message ("list = " & To_String (Sw));
      end loop;
   end Show_Dist_Args;

   ------------------------
   -- Sigint_Intercepted --
   ------------------------

   procedure Sigint_Intercepted is
   begin
      Exit_Program (E_Fatal);
   end Sigint_Intercepted;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in out String) is
   begin
      for J in S'Range loop
         S (J) := To_Lower (S (J));
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

   ---------------------------
   -- Set_Application_Names --
   ---------------------------

   procedure Set_Application_Names (Configuration_Name : Name_Id) is
   begin
      Get_Name_String (Configuration_Name);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Add_Str_To_Name_Buffer ("_monolithic_app");

      Monolithic_App_Unit_Name := Name_Find;

      Add_Str_To_Name_Buffer (ADB_Suffix);
      Monolithic_Src_Base_Name := Name_Find;

      Monolithic_Src_Name := Dir (Root_Id, Monolithic_Src_Base_Name);
      Monolithic_ALI_Name := To_Afile (Monolithic_Src_Name);
      Monolithic_Obj_Name := To_Ofile (Monolithic_Src_Name);

      Get_Name_String (Configuration_Name);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Add_Str_To_Name_Buffer ("_dist_app");
      Dist_App_Project := Name_Find;

      Set_Corresponding_Project_File_Name (Dist_App_Project_File);
   end Set_Application_Names;

   ------------------------
   -- Write_Missing_File --
   ------------------------

   procedure Write_Missing_File (Fname : File_Name_Type) is
   begin
      Message ("file", Fname, "does not exist");
   end Write_Missing_File;

   ----------------------------
   -- Write_Warnings_Pragmas --
   ----------------------------

   procedure Write_Warnings_Pragmas is
   begin
      --  Turn off warnings

      Write_Line ("pragma Warnings (Off);");

      --  Turn off style checks and set maximum line length to the largest
      --  supported value.

      Write_Line ("pragma Style_Checks (""NM32766"");");
   end Write_Warnings_Pragmas;

end XE_Utils;
