------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ U T I L S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2005 Free Software Foundation, Inc.           --
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

with Ada.Command_Line; use Ada.Command_Line;
with XE_Defs;          use XE_Defs;
with XE_Flags;         use XE_Flags;
with XE_IO;            use XE_IO;
with XE_Names;         use XE_Names;
with XE_Usage;

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

   Usage_Needed : Boolean := False;

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup);

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);
   pragma Import (C, Dup2);

   GNAT_Driver : String_Access;

   List_Command    : constant String_Access := new String'("list");
   Build_Command   : constant String_Access := new String'("make");
   Compile_Command : constant String_Access := new String'("compile");

   Up_To_Low : constant := Character'Pos ('A') - Character'Pos ('a');

   function Locate
     (Exec_Name  : String;
      Show_Error : Boolean := True)
     return String_Access;
   --  look for Exec_Name on the path. If Exec_Name is found then the full
   --  pathname for Exec_Name is returned. If Exec_Name is not found and
   --  Show_Error is set to False then null is returned. If Exec_Name is not
   --  found and Show_Error is set to True then Fatal_Error is raised.

   procedure Add_Make_Switch (Argv : String);
   procedure Add_List_Switch (Argv : String);
   procedure Add_Main_Source (Source : String);

   procedure Fail
     (S1 : String;
      S2 : String := No_Str;
      S3 : String := No_Str);

   type Sigint_Handler is access procedure;

   procedure Install_Int_Handler (Handler : Sigint_Handler);
   pragma Import (C, Install_Int_Handler, "__gnat_install_int_handler");
   --  Called by Gnatmake to install the SIGINT handler below

   procedure Sigint_Intercepted;
   --  Called when the program is interrupted by Ctrl-C to delete the
   --  temporary mapping files and configuration pragmas files.

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
      R : String)
      return File_Name_Type is
   begin
      Name_Len := 0;
      if Present (L) then
         Get_Name_String_And_Append (L);
      end if;
      Add_Str_To_Name_Buffer (R);
      return Name_Find;
   end "&";

   -----------------------
   -- Add_List_Switch --
   -----------------------

   procedure Add_List_Switch (Argv : String) is
   begin
      List_Switches.Append (new String'(Argv));
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

   -----------------------
   -- Add_Make_Switch --
   -----------------------

   procedure Add_Make_Switch (Argv : String) is
   begin
      Make_Switches.Append (new String'(Argv));
   end Add_Make_Switch;

   -----------
   -- Build --
   -----------

   procedure Build
     (Library    : File_Name_Type;
      Arguments  : Argument_List;
      Fatal      : Boolean := True;
      Silent     : Boolean := True)
   is
      Length : constant Positive :=
        Arguments'Length + 4
        + Make_Switches.Last
        - Make_Switches.First;
      Flags   : Argument_List (1 .. Length);
      N_Flags : Natural := 0;
      Success : Boolean;
      Has_Prj : Boolean := False;
      Index   : Natural;

      --  Files to deal with error messages

      File           : File_Descriptor;
      Output         : File_Name_Type := No_File_Name;
      Saved_Standout : File_Descriptor;
      Saved_Standerr : File_Descriptor;

   begin
      --  gnat make

      N_Flags := N_Flags + 1;
      Flags (N_Flags) := Build_Command;

      --  library filename

      N_Flags := N_Flags + 1;
      Get_Name_String (Library);
      Flags (N_Flags) := new String'(Name_Buffer (1 .. Name_Len));

      --  -q (because gnatmake is verbose instead of gcc)

      N_Flags := N_Flags + 1;
      Flags (N_Flags) := Quiet_Flag;

      for I in Arguments'Range loop
         N_Flags := N_Flags + 1;
         Flags (N_Flags) := Arguments (I);

         --  Detect any project file

         if Arguments (I).all = Project_File_Flag.all then
            Has_Prj := True;
         end if;
      end loop;

      Index := Make_Switches.First;
      while Index <= Make_Switches.Last loop

         --  If there is a project file among the arguments then any
         --  project file from the Make switches is ignored.

         if Has_Prj
           and then Make_Switches.Table (Index).all = Project_File_Flag.all
         then
            Index := Index + 1;

         else
            N_Flags := N_Flags + 1;
            Flags (N_Flags) := Make_Switches.Table (Index);
         end if;

         Index := Index + 1;
      end loop;

      if not Verbose_Mode and then Silent then
         Register_Temp_File (File, Output);
         Saved_Standout := Dup (Standout);
         Saved_Standerr := Dup (Standerr);
         Dup2 (File, Standout);
         Dup2 (File, Standerr);
      end if;

      --  Call gnat make

      Execute (GNAT_Driver, Flags (1 .. N_Flags), Success);

      if Present (Output) then
         Dup2 (Saved_Standout, Standout);
         Dup2 (Saved_Standerr, Standerr);
         Close (Saved_Standout);
         Close (Saved_Standerr);
         Remove_Temp_File (Output);
      end if;

      --  Free library filename argument

      Free (Flags (2));

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

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (S : String) return String is
      R : String := S;
   begin
      Capitalize (R);
      return R;
   end Capitalize;

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
      Capitalized : Boolean := True;

   begin
      for J in S'Range loop
         if S (J) in 'a' .. 'z' then
            if Capitalized then
               S (J) := Character'Val (Character'Pos (S (J)) + Up_To_Low);
            end if;
            Capitalized := False;

         elsif S (J) in 'A' .. 'Z' then
            if not Capitalized then
               S (J) := Character'Val (Character'Pos (S (J)) - Up_To_Low);
            end if;
            Capitalized := False;

         elsif S (J) = '_' or else S (J) = '.' then
            Capitalized := True;
         end if;
      end loop;
   end Capitalize;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Source    : File_Name_Type;
      Arguments : Argument_List;
      Fatal     : Boolean := True;
      Silent    : Boolean := True)
   is
      Length  : constant Natural :=
        Arguments'Length + 5
        + Make_Switches.Last
        - Make_Switches.First;
      Flags   : Argument_List (1 .. Length);
      N_Flags : Natural := 0;
      Success : Boolean;
      Has_Prj : Boolean := False;
      Index   : Natural;

      --  Files to deal with error messages

      File           : File_Descriptor;
      Output         : File_Name_Type := No_File_Name;
      Saved_Standout : File_Descriptor;
      Saved_Standerr : File_Descriptor;

   begin
      --  gnat compile

      N_Flags := N_Flags + 1;
      Flags (N_Flags) := Compile_Command;

      --  source filename

      N_Flags := N_Flags + 1;
      Get_Name_String (Source);
      Flags (N_Flags) := new String'(Name_Buffer (1 .. Name_Len));

      --  Check whether we have a predefined unit

      Name_Len := 0;
      Add_Str_To_Name_Buffer (Strip_Directory (Flags (N_Flags).all));
      if Name_Len > 2
        and then Name_Buffer (2) = '-'
        and then (Name_Buffer (1) = 'a'
                  or else Name_Buffer (1) = 'g'
                  or else Name_Buffer (1) = 's')
      then
         N_Flags := N_Flags + 1;
         Flags (N_Flags) := Readonly_Flag;
      end if;

      --  -q (because gnatmake is verbose instead of gcc)

      N_Flags := N_Flags + 1;
      Flags (N_Flags) := Quiet_Flag;

      for I in Arguments'Range loop
         N_Flags := N_Flags + 1;
         Flags (N_Flags) := Arguments (I);

         --  Detect any project file

         if Arguments (I).all = Project_File_Flag.all then
            Has_Prj := True;
         end if;
      end loop;

      Index := Make_Switches.First;
      while Index <= Make_Switches.Last loop

         --  If there is a project file among the arguments then any
         --  project file from the Make switches is ignored.

         if Has_Prj
           and then Make_Switches.Table (Index).all = Project_File_Flag.all
         then
            Index := Index + 1;

         else
            N_Flags := N_Flags + 1;
            Flags (N_Flags) := Make_Switches.Table (Index);
         end if;

         Index := Index + 1;
      end loop;

      if not Verbose_Mode and then Silent then
         Register_Temp_File (File, Output);
         Saved_Standout := Dup (Standout);
         Saved_Standerr := Dup (Standerr);
         Dup2 (File, Standout);
         Dup2 (File, Standerr);
      end if;

      Execute (GNAT_Driver, Flags (1 .. N_Flags), Success);

      if Present (Output) then
         Dup2 (Saved_Standout, Standout);
         Dup2 (Saved_Standerr, Standerr);
         Close (Saved_Standout);
         Close (Saved_Standerr);
         Remove_Temp_File (Output);
      end if;

      --  Free source filename argument

      Free (Flags (2));

      if not Success and then Fatal then
         raise Compilation_Error;
      end if;
   end Compile;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Command   : String_Access;
      Arguments : Argument_List;
      Success   : out Boolean) is
   begin
      if Verbose_Mode then
         Write_Str (Command.all);
         for J in Arguments'Range loop
            if Arguments (J) /= null then
               Write_Str (" ");
               Write_Str (Arguments (J).all);
            end if;
         end loop;
         Write_Eol;
      end if;

      Spawn (Command.all, Arguments, Success);
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
      Stub_Dir_Name  := Dir (Id (Root), Id ("private"));
      Stub_Dir_Name  := Dir (Stub_Dir_Name, Id ("stub"));
      Stub_Dir       := new String'(Name_Buffer (1 .. Name_Len));
      PWD_Id         := Dir (Id ("`pwd`"), No_File_Name);
      I_Current_Dir  := new String'("-I.");
      E_Current_Dir  := new String'("-I-");

      Part_Main_Src_Name := Id ("partition" & ADB_Suffix);
      Part_Main_ALI_Name := To_Afile (Part_Main_Src_Name);
      Part_Main_Obj_Name := To_Ofile (Part_Main_Src_Name);

      Part_Prj_File_Name := Id ("partition.gpr");

      Name_Len := 2;
      Name_Buffer (1 .. 2) := "-I";
      Get_Name_String_And_Append (Stub_Dir_Name);
      I_Stub_Dir := new String'(Name_Buffer (1 .. Name_Len));

      for J in 1 .. Argument_Count loop
         Scan_Dist_Arg (Argument (J));
      end loop;

      if Project_File_Name_Present
        and then Project_File_Name /= null
      then
         Fail ("project file name missing after -P");
      end if;

      XE_Defs.Initialize;

      if Usage_Needed then
         XE_Usage;
         raise Usage_Error;
      end if;

      Install_Int_Handler (Sigint_Intercepted'Access);

      Create_Dir (Stub_Dir_Name);

      GNAT_Driver := Locate ("gnat");
   end Initialize;

   ----------
   -- List --
   ----------

   procedure List
     (Sources   : File_Name_List;
      Arguments : Argument_List;
      Output    : out File_Name_Type;
      Fatal     : Boolean := True)
   is
      Length  : constant Natural :=
        Sources'Length + 4
        + Arguments'Length
        + List_Switches.Last
        - List_Switches.First;
      Flags   : Argument_List (1 .. Length);
      N_Flags : Natural := 0;
      File    : GNAT.OS_Lib.File_Descriptor;
      Success : Boolean;
      Result  : File_Name_Type := No_File_Name;
      Has_Prj : Boolean := False;
      Index   : Natural;
      Predef  : Boolean := False;

      Saved_Standout : File_Descriptor;
      Saved_Standerr : File_Descriptor;

   begin
      --  gnat list

      N_Flags := N_Flags + 1;
      Flags (N_Flags) := List_Command;

      --  source filenames

      for J in Sources'Range loop
         N_Flags := N_Flags + 1;
         Get_Name_String (Sources (J));
         Flags (N_Flags) := new String'(Name_Buffer (1 .. Name_Len));

         Predef := Predef or Is_Predefined_File (Sources (J));
      end loop;

      if Predef then
         N_Flags := N_Flags + 1;
         Flags (N_Flags) := Readonly_Flag;
      end if;

      --  -q (because gnatmake is verbose instead of gcc)

      N_Flags := N_Flags + 1;
      Flags (N_Flags) := Quiet_Flag;

      for I in Arguments'Range loop
         N_Flags := N_Flags + 1;
         Flags (N_Flags) := Arguments (I);

         --  Detect any project file

         if Arguments (I).all = Project_File_Flag.all then
            Has_Prj := True;
         end if;
      end loop;

      Index := List_Switches.First;
      while Index <= List_Switches.Last loop

         --  If there is a project file among the arguments then any
         --  project file from the List switches is ignored.

         if Has_Prj
           and then List_Switches.Table (Index).all = Project_File_Flag.all
         then
            Index := Index + 1;

         else
            N_Flags := N_Flags + 1;
            Flags (N_Flags) := List_Switches.Table (Index);
         end if;

         Index := Index + 1;
      end loop;

      if Verbose_Mode then
         Write_Str (GNAT_Driver.all);
         for J in Flags'Range loop
            if Flags (J) /= null then
               Write_Str (" ");
               Write_Str (Flags (J).all);
            end if;
         end loop;
         Write_Eol;
      end if;

      Register_Temp_File (File, Result);
      Saved_Standout := Dup (Standout);
      Saved_Standerr := Dup (Standerr);
      Dup2 (File, Standout);
      Dup2 (File, Standerr);

      Spawn (GNAT_Driver.all, Flags (1 .. N_Flags), Success);

      Dup2 (Saved_Standout, Standout);
      Dup2 (Saved_Standerr, Standerr);
      Close (Saved_Standout);
      Close (Saved_Standerr);

      if not Success then
         if Fatal then
            raise Program_Error;
         end if;
         Remove_Temp_File (Result);
      end if;

      --  Free source filename argument

      N_Flags := 1;
      for J in Sources'Range loop
         N_Flags := N_Flags + 1;
         Free (Flags (N_Flags));
      end loop;

      Output := Result;
   end List;

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

   procedure Scan_Dist_Arg (Argv : String) is
   begin
      if Argv'Length = 0 then
         return;
      end if;

      if Argv = "-cargs" then
         Program_Args := Compiler;
         Add_Make_Switch (Argv);
         return;

      elsif Argv = "-bargs" then
         Program_Args := Binder;
         Add_Make_Switch (Argv);
         return;

      elsif Argv = "-largs" then
         Program_Args := Linker;
         Add_Make_Switch (Argv);
         return;

      elsif Argv = "-margs" then
         Program_Args := None;
         Add_Make_Switch (Argv);
         return;
      end if;

      if Program_Args = Binder
        or else Program_Args = Linker
      then
         Add_Make_Switch (Argv);
         return;
      end if;

      if Project_File_Name_Present then
         Project_File_Name :=
           new String'(Normalize_Pathname (Argv));
         Project_File_Name_Present := False;

      elsif Argv (Argv'First) = '-' then

         if Argv'Length = 1 then
            Fail ("switch character cannot be followed by a blank");

         --  Processing for -I-

         elsif Argv = "-I-" then
            Add_List_Switch (Argv);
            Add_Make_Switch (Argv);

         --  Forbid -?- or -??- where ? is any character

         elsif Argv'Length in 3 .. 4 and then Argv (Argv'Last) = '-' then
            Fail ("Trailing ""-"" at the end of ", Argv, " forbidden.");

         --  Processing for -Adir, -Idir and -Ldir

         elsif Argv (Argv'First + 1) = 'A'
           or else Argv (Argv'First + 1) = 'I'
           or else Argv (Argv'First + 1) = 'L'
         then
            Add_List_Switch (Argv);
            Add_Make_Switch (Argv);

         --  Processing for -aIdir, -aLdir and -aOdir

         elsif Argv'Length >= 3
           and then Argv (Argv'First + 1) = 'a'
           and then (Argv (Argv'First + 2) = 'I'
             or else Argv (Argv'First + 2) = 'L'
             or else Argv (Argv'First + 2) = 'O')
         then
            Add_List_Switch (Argv);
            Add_Make_Switch (Argv);

         elsif Argv (Argv'First + 1) = 'P' then

            if Project_File_Name_Present
              or else Project_File_Name /= null
            then
               Fail ("cannot have several project files specified");
            end if;

            if Argv'Length > 2 then
               Project_File_Name :=
                 new String'(Normalize_Pathname
                              (Argv (Argv'First + 2 .. Argv'Last)));
               Add_List_Switch (Project_File_Flag.all);
               Add_List_Switch (Project_File_Name.all);
               Add_Make_Switch (Project_File_Flag.all);
               Add_Make_Switch (Project_File_Name.all);

            else
               Project_File_Name_Present := True;
               Add_List_Switch (Project_File_Flag.all);
               Add_Make_Switch (Project_File_Flag.all);
            end if;

         elsif Argv (Argv'First + 1) = 'X' then
            Add_List_Switch (Argv);
            Add_Make_Switch (Argv);

         --  Processing for one character switches

         elsif Argv'Length = 2 then
            case Argv (Argv'First + 1) is
               when 'a' =>
                  Add_List_Switch (Argv);
                  Add_Make_Switch (Argv);

               when 'f'
                 |  'g'
                 |  'O' =>
                  Add_Make_Switch (Argv);

               when 't' =>
                  Keep_Tmp_Files := True;

               when 'd' =>
                  Debug_Mode   := True;

               when 'q' =>
                  Quiet_Mode   := True;

               when 'v' =>
                  Verbose_Mode := True;

               when others =>
                  Usage_Needed := True;
            end case;

         --  Processing for -O0, -O1, -O2 and -O3

         elsif Argv'Length = 3
           and then Argv (Argv'First + 1) = 'O'
           and then Argv (Argv'First + 2) in '0' .. '3'
         then
            Add_Make_Switch (Argv);

         --  Processing for -gnat flags

         elsif Argv'Length > 5
           and then Argv (Argv'First + 1 .. Argv'First + 4) = "gnat"
         then
            Add_Make_Switch (Argv);

         --  Processing for --PCS=

         elsif Argv'Length > 6
           and then Argv (Argv'First + 1 .. Argv'First + 5) = "-PCS="
         then
            Set_PCS_Name (Argv (Argv'First + 6 .. Argv'Last));

         --  Processing for --RTS=

         elsif Argv'Length > 6
           and then Argv (Argv'First + 1 .. Argv'First + 5) = "-RTS="
         then
            Add_Make_Switch (Argv);
            Add_List_Switch (Argv);

         else
            Usage_Needed := True;
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
      --  We have already processed the user command line: we might be
      --  in the -cargs or -largs section.

      Scan_Dist_Arg ("-margs");

      for J in Argv'Range loop
         if Argv (J)'Length > 0 then
            Scan_Dist_Arg (Argv (J).all);
         end if;
      end loop;
      Free (Argv);
   end Scan_Dist_Args;

   --------------------
   -- Show_Dist_Args --
   --------------------

   procedure Show_Dist_Args is
   begin
      for J in Make_Switches.First .. Make_Switches.Last loop
         Message ("make = " & Make_Switches.Table (J).all);
      end loop;
      for J in List_Switches.First .. List_Switches.Last loop
         Message ("list = " & List_Switches.Table (J).all);
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

   function To_Lower (C : Character) return Character is
   begin
      if C in 'A' .. 'Z' then
         return Character'Val (Character'Pos (C) - Up_To_Low);
      end if;
      return C;
   end To_Lower;

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

   ------------------------
   -- Write_Missing_File --
   ------------------------

   procedure Write_Missing_File (Fname : File_Name_Type) is
   begin
      Message ("file", Fname, "does not exist");
   end Write_Missing_File;

end XE_Utils;
