------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2015, Free Software Foundation, Inc.          --
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

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.HTable;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Platform;
with XE;       use XE;
with XE_Flags; use XE_Flags;
with XE_Front; use XE_Front;
with XE_IO;    use XE_IO;
with XE_Names; use XE_Names;
with XE_Utils; use XE_Utils;

package body XE_Back is

   -------------------------
   -- Backend Registering --
   -------------------------

   type String_Ptr is access all String;
   type Header_Num is range 1 .. 7;
   function Hash (S : String_Ptr) return Header_Num;

   function Eq (S1, S2 : String_Ptr) return Boolean;
   --  Test equality of designated strings

   package All_Backends is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Backend_Access,
      No_Element => null,
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Eq);

   ------------------
   -- Casing Rules --
   ------------------

   type Casing_Rule is record
      Size : Natural;
      From : GNAT.OS_Lib.String_Access;
      Into : GNAT.OS_Lib.String_Access;
   end record;

   Rules : array (1 .. 64) of Casing_Rule;
   Rules_Last : Natural := 0;

   function Get_Absolute_Command return String;
   --  Get the absolute path of the command being executed

   function Needs_Stub (A : ALI_Id) return Boolean;
   --  True if A is the LI Id for a unit requiring stubs (i.e. a non-generic
   --  RCI or Shared passive unit).

   procedure Do_Rename (Source, Target : File_Name_Type);
   --  Call Rename_File (Source, Target), and show a message if in debug mode

   -----------
   -- "and" --
   -----------

   function "and" (N : Name_Id; S : String) return Name_Id is
   begin
      Get_Name_String (N);
      Add_Char_To_Name_Buffer ('.');
      Add_Str_To_Name_Buffer (S);
      return Name_Find;
   end "and";

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Name_Id) return Name_Id is
   begin
      return L and Get_Name_String (R);
   end "and";

   ------------------------
   -- Apply_Casing_Rules --
   ------------------------

   procedure Apply_Casing_Rules (S : in out String) is
      New_Word : Boolean := True;
      Length   : Natural := S'Length;
      C        : String  := S;

   begin
      Capitalize (S);
      To_Lower (C);
      for I in S'Range loop
         if New_Word then
            New_Word := False;
            for J in 1 .. Rules_Last loop
               if Rules (J).Size <= Length
                 and then C (I .. I + Rules (J).Size - 1) = Rules (J).From.all
               then
                  S (I .. I + Rules (J).Size - 1) := Rules (J).Into.all;
               end if;
            end loop;
         end if;
         if S (I) = '_' then
            New_Word := True;
         end if;
         Length := Length - 1;
      end loop;
   end Apply_Casing_Rules;

   ---------------
   -- Do_Rename --
   ---------------

   procedure Do_Rename (Source, Target : File_Name_Type) is
   begin
      if Debug_Mode then
         Message ("renaming", Source, "to", Target);
      end if;
      Delete_File (Target);
      Rename_File (Source, Target);
   end Do_Rename;

   --------
   -- Eq --
   --------

   function Eq (S1, S2 : String_Ptr) return Boolean is
   begin
      if S1 = null or else S2 = null then
         return S1 = S2;
      end if;
      return S1.all = S2.all;
   end Eq;

   ------------------
   -- Find_Backend --
   ------------------

   function Find_Backend (PCS_Name : String) return Backend_Access is
      S       : aliased String := PCS_Name;
      Backend : Backend_Access;
   begin
      Backend := All_Backends.Get (S'Unchecked_Access);
      if Backend = null then
         Write_Line ("'" & PCS_Name & "' is not a valid PCS.");
         raise Usage_Error;
      end if;

      return Backend;
   end Find_Backend;

   ----------------------------------
   -- Generate_All_Stubs_And_Skels --
   ----------------------------------

   procedure Generate_All_Stubs_And_Skels is
      PID   : Partition_Id;
      Unit  : Unit_Id;
      Uname : Unit_Name_Type;

   begin
      for J in ALIs.First .. ALIs.Last loop
         Unit := ALIs.Table (J).Last_Unit;

         --  Create stub files. Create skel files as well when we have to build
         --  the partition on which this unit is mapped.

         if Needs_Stub (J) then
            Uname := Name (Units.Table (Unit).Uname);
            PID   := Get_Partition_Id (Uname);

            if PID /= No_Partition_Id
              and then Partitions.Table (PID).To_Build
              and then Partitions.Table (PID).Passive /= BTrue
            then
               if Debug_Mode then
                  Message ("create caller and receiver stubs for", Uname);
               end if;

               Generate_Stub (J);
               Generate_Skel (J, PID);

            else
               if Debug_Mode then
                  Message ("create caller stubs for", Uname);
               end if;

               Generate_Stub (J);
            end if;
         end if;

         if Display_Compilation_Progress then
            Write_Str ("completed ");
            Write_Int (Int (J));
            Write_Str (" out of ");
            Write_Int (Int (ALIs.Last));
            Write_Str (" (");
            Write_Int (Int ((J * 100) / (ALIs.Last - ALIs.First + 1)));
            Write_Str ("%)...");
            Write_Eol;
         end if;
      end loop;
   end Generate_All_Stubs_And_Skels;

   ----------------------------------------
   -- Generate_Application_Project_Files --
   ----------------------------------------

   procedure Generate_Application_Project_Files is
      Prj_Fname : File_Name_Type;
      Prj_File  : File_Descriptor;
   begin
      --  Create application-wide project, extending user project file if
      --  provided.

      Prj_Fname := Dir (Root_Id, Dist_App_Project_File);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);

      --  Dependency on PCS

      Write_Str  ("with """);
      Write_Name (PCS_Project);
      Write_Line (""";");

      --  Dependency on user project, if any

      Write_Str  ("project ");
      Write_Name (Dist_App_Project);

      if Project_File_Name /= Null_Unbounded_String then
         Write_Str (" extends all """ & To_String (Project_File_Name) & """");
      end if;

      Write_Line (" is");
      Write_Line ("   for Object_Dir use ""obj"";");

      --  If no user project file is provided, add any source directory
      --  specified on the command line as source directories, in addition to
      --  the main application directory. The generated main subprogram
      --  (monolithic_app.adb) and all RCI units must be sources of the
      --  project (so that they can be individually recompiled).

      Write_Str  ("   for Source_Dirs use ("".""");
      if Project_File_Name = Null_Unbounded_String then
         Write_Line (",");
         Write_Line ("     ""../..""");
         for D of Source_Directories loop
            declare
               Normalized_Dir : constant String :=
                                  Normalize_Pathname
                                    (To_String (D),
                                     Resolve_Links => Resolve_Links);
            begin
               if Is_Directory (Normalized_Dir) then
                  Write_Line (",");
                  Write_Str ("     """ & Normalized_Dir & """");
               end if;
            end;
         end loop;
      end if;
      Write_Line (");");

      --  If a user project file is provided, explicitly specify additional
      --  source file partition.adb (in addition to all other sources, which
      --  are sources of this project by virtue of "extends all").

      if Project_File_Name /= Null_Unbounded_String then
         Write_Str  ("   for Source_Files use (""");
         Write_Name (Monolithic_Src_Base_Name);
         Write_Line (""");");
      end if;

      Write_Str  ("end ");
      Write_Name (Dist_App_Project);
      Write_Line (";");
      Close (Prj_File);
      Set_Standard_Output;

      --  Distributed app project file extends user provided project, and
      --  includes the PCS as well.

      Project_File_Name :=
        +Normalize_Pathname (Get_Name_String (Prj_Fname),
                             Resolve_Links => Resolve_Links);
   end Generate_Application_Project_Files;

   -------------------------------------
   -- Generate_Partition_Project_File --
   -------------------------------------

   procedure Generate_Partition_Project_File
     (D : Directory_Name_Type;
      P : Partition_Id := No_Partition_Id)
   is
      Prj_Fname  : File_Name_Type;
      Prj_File   : File_Descriptor;

   begin
      Prj_Fname := Dir (D, Part_Prj_File_Name);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);

      --  Note that the main subprogram for each partition is always called
      --  partition.adb; the executable name is set using gnatmake command line
      --  switch "-o". We do not set it through the project to ensure that
      --  any settings inherited from the user's Builder package (in particular
      --  global configuration pragmas) are preserved.

      Write_Str  ("project Partition extends all """);
      Write_Str  (To_String (Project_File_Name));
      Write_Line (""" is");
      Write_Line ("   for Object_Dir use ""."";");

      if P /= No_Partition_Id then
         Write_Str  ("   for Exec_Dir use """);
         Name_Len := 0;
         if Present (Partitions.Table (P).Executable_Dir) then
            Get_Name_String (Partitions.Table (P).Executable_Dir);
         end if;

         declare
            Exec_Dir : constant String := Name_Buffer (1 .. Name_Len);
         begin
            if Exec_Dir'Length = 0
              or else not Is_Absolute_Path (Exec_Dir)
            then
               --  Reach up to main dir from
               --  dsa/<target>/partitions/<cfg>/<partition>

               Write_Str ("../../../../../");
            end if;
            Write_Str (Exec_Dir);
         end;
         Write_Line (""";");

      else
         Write_Line ("   --  Pseudo-partition project for RCI calling stubs");
      end if;

      Write_Line ("end Partition;");
      Close (Prj_File);
      Set_Standard_Output;
   end Generate_Partition_Project_File;

   -------------------
   -- Generate_Skel --
   -------------------

   procedure Generate_Skel (A : ALI_Id; P : Partition_Id) is
      Full_Unit_File : File_Name_Type;
      Full_ALI_File  : File_Name_Type;
      Skel_Object    : File_Name_Type;
      Skel_ALI       : File_Name_Type;
      Arguments      : Argument_Vec;
      Part_Prj_Fname : File_Name_Type;
      Directory      : Directory_Name_Type
        renames Partitions.Table (P).Partition_Dir;

   begin
      Full_Unit_File := Units.Table (ALIs.Table (A).First_Unit).Sfile;
      Full_ALI_File  := Dir (Monolithic_Obj_Dir, ALIs.Table (A).Afile);

      if not Is_Regular_File (Full_ALI_File) then

         --  No ALI in monolithic application: this must be the PCS_Conf_Unit.
         --  In this case the Full_ALI_File is deemed to be older than any
         --  existing stubs file, and so the stubs will be considered to be
         --  always up-to-date if present.

         pragma Assert
           (PCS_Conf_Unit /= No_Name
              and then ALIs.Table (A).Uname = PCS_Conf_Unit);
         null;
      end if;

      --  Determination of skel ALI file name

      Skel_ALI    := Dir (Directory, Strip_Directory (Full_ALI_File));
      Skel_Object := To_Ofile (Skel_ALI);

      --  Do we need to generate the skel files

      if not Is_Regular_File (Skel_Object) then
         if Verbose_Mode then
            Write_Missing_File (Skel_Object);
         end if;

      elsif not Is_Regular_File (Skel_ALI) then
         if Verbose_Mode then
            Write_Missing_File (Skel_ALI);
         end if;

      elsif File_Time_Stamp (Full_ALI_File) > File_Time_Stamp (Skel_ALI) then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_ALI_File, Skel_ALI);
         end if;

      else
         if not Quiet_Mode then
            Message ("  ", ALIs.Table (A).Uname,
                     "receiver stubs is up to date");
         end if;
         return;
      end if;

      --  Here if stubs need to be rebuilt

      if not Quiet_Mode then
         Message
           ("building", ALIs.Table (A).Uname,
            "receiver stubs from", Normalize_CWD (Full_Unit_File));
      end if;

      Push (Arguments, Skel_Flag);

      if Project_File_Name = Null_Unbounded_String then
         Push (Arguments, Object_Dir_Flag);
         Push (Arguments, Directory);

      else
         Push (Arguments, Project_File_Flag);
         Part_Prj_Fname := Dir (Directory, Part_Prj_File_Name);
         Push (Arguments, Part_Prj_Fname);
      end if;

      Push (Arguments, Partition_Dir_Flag (P));

      Compile (Full_Unit_File, Arguments, Fatal => False);
   end Generate_Skel;

   -------------------------
   -- Generate_Stamp_File --
   -------------------------

   procedure Generate_Stamp_File (P : Partition_Id) is
      File    : File_Descriptor;
      Current : Partition_Type renames Partitions.Table (P);

   begin
      Create_File (File, Dir (Current.Partition_Dir, Build_Stamp_File));
      Set_Output  (File);
      Write_Line (String (File_Time_Stamp (Configuration_File_Name)));
      Write_Line (String (File_Time_Stamp (Current.Executable_File)));
      Write_Line (String (File_Time_Stamp (Current.Most_Recent)));
      Close (File);
      Set_Standard_Output;
   end Generate_Stamp_File;

   ---------------------------
   -- Generate_Starter_File --
   ---------------------------

   procedure Generate_Starter_File (Backend : Backend_Access) is
      procedure Generate_Boot_Server_Evaluation (P : Partition_Id);
      procedure Generate_Host_Name_Evaluation   (P : Partition_Id);
      procedure Generate_Executable_Invocation  (P : Partition_Id);

      -------------------------------------
      -- Generate_Boot_Server_Evaluation --
      -------------------------------------

      procedure Generate_Boot_Server_Evaluation (P : Partition_Id) is
         L : Location_Id := Partitions.Table (P).First_Network_Loc;
      begin
         if L = No_Location_Id then
            L := Default_First_Boot_Location;
         end if;

         if L = No_Location_Id then
            Write_Str ("BOOT_LOCATION=tcp://`hostname`:");
            Write_Str ("`echo 000$$ | sed 's,^.*\(...\),5\1,'`");
            Write_Eol;

         else
            Write_Str ("BOOT_LOCATION='");
            loop
               Write_Name (Locations.Table (L).Major);
               Write_Str  ("://");
               Write_Name (Locations.Table (L).Minor);
               L := Locations.Table (L).Next_Location;
               exit when L = No_Location_Id;
               Write_Char (' ');
            end loop;
            Write_Str ("'");
            Write_Eol;
         end if;
      end Generate_Boot_Server_Evaluation;

      ------------------------------------
      -- Generate_Executable_Invocation --
      ------------------------------------

      procedure Generate_Executable_Invocation (P : Partition_Id) is
         Ext_Quote : constant Character := '"';  -- "
         Int_Quote : Character := ''';  -- '
         Current   : Partition_Type renames Partitions.Table (P);

      begin

         --  For the main partition, the command should be
         --    "<pn>" --boot_location "<bl>" <cline>
         --  For other partitions, it should be
         --    <rshcmd> <host> <rshopts> "<exported_Vars> '<pn>' --detach ...
         --         ... --boot_location '<bs>' <cline> &" ...
         --         ... < /dev/null > /dev/null 2>&1

         if P = Main_Partition then
            Int_Quote := '"';  -- "
         end if;

         if P /= Main_Partition then
            Write_Name (Get_Rsh_Command);
            Write_Str  (" ${");
            Write_Name (Current.Name);
            Write_Str  ("_HOST} ");
            Write_Name (Get_Rsh_Options);
            Write_Char (' ');
            Write_Char (Ext_Quote);
            Write_Str  (Get_Env_Vars (P, Q => Int_Quote, Names_Only => False));
         end if;

         --  Executable file name must be quoted because it may contain
         --  spaces, or (on Windows) backslashes that must not be interpreted
         --  by the shell.

         Write_Char (Int_Quote);
         Write_Name (To_Absolute_File (Current.Executable_File));
         Write_Char (Int_Quote);

         --  Boot_Location not currently supported with PolyORB, instead pass
         --  name service reference directly.

         --  Write_Str  (" --boot_location ");
         --  Write_Char (Int_Quote);
         --  Write_Str  ("${BOOT_LOCATION}");
         --  Write_Char (Int_Quote);

         Write_Str (" --polyorb-dsa-name_service=");
         Write_Char (Int_Quote);
         Write_Str  ("${POLYORB_DSA_NAME_SERVICE}");
         Write_Char (Int_Quote);

         Write_Name (Current.Command_Line);

         if P /= Main_Partition then
            Write_Str  (" ");
            Write_Name (Get_Detach_Flag (Backend));
            Write_Str  (" &");
            Write_Char (Ext_Quote);
            Write_Str  (" < /dev/null > /dev/null 2>&1");
         end if;

         Write_Eol;
      end Generate_Executable_Invocation;

      -----------------------------------
      -- Generate_Host_Name_Evaluation --
      -----------------------------------

      procedure Generate_Host_Name_Evaluation (P : Partition_Id) is
         H : Name_Id;
      begin
         Write_Image (H, Partitions.Table (P).Host, P);
         if No (H) then
            Write_Str  ("echo '");
            Write_Name (Partitions.Table (P).Name);
            Write_Str  (" host: '");
            Write_Eol;
            Write_Str  ("read ");
            Write_Name (Partitions.Table (P).Name);
            Write_Str  ("_HOST");
            Write_Eol;

         else
            Write_Name (Partitions.Table (P).Name);
            Write_Str  ("_HOST=");
            Write_Name (H);
            Write_Eol;
         end if;
      end Generate_Host_Name_Evaluation;

      File      : File_Descriptor;
      Exec_File : File_Name_Type;
      Success   : Boolean;

   --  Start of processing for Generate_Starter_File

   begin
      --  Do not build start unless also building all partitions

      if not Partitions.Table (Default_Partition_Id).To_Build then
         return;
      end if;

      if Default_Starter /= None_Import
        and then not Quiet_Mode
      then
         Message ("generating starter", Main_Subprogram);
      end if;

      case Default_Starter is
         when Shell_Import =>
            Delete_File (Main_Subprogram);
            Create_File (File, Main_Subprogram, True);
            Set_Output  (File);
            Write_Line  ("#! /bin/sh");

            for J in Partitions.First + 1 .. Partitions.Last loop
               if J /= Main_Partition then
                  Generate_Host_Name_Evaluation (J);
               end if;
            end loop;

            Generate_Boot_Server_Evaluation (Main_Partition);
            for J in Partitions.First + 1 .. Partitions.Last loop
               if J /= Main_Partition then
                  Generate_Executable_Invocation (J);
               end if;
            end loop;
            Generate_Executable_Invocation (Main_Partition);
            Close (File);
            Set_Standard_Output;

         when Ada_Import =>
            Exec_File := Partitions.Table (Main_Partition).Executable_File;
            Copy_File
              (Get_Name_String (Exec_File),
               Get_Name_String (Main_Subprogram & Exe_Suffix_Id),
               Success,
               Overwrite,
               Full);

         when None_Import =>
            null;
      end case;
   end Generate_Starter_File;

   -------------------
   -- Generate_Stub --
   -------------------

   procedure Generate_Stub (A : ALI_Id) is
      Full_Unit_File   : File_Name_Type;
      Full_ALI_File    : File_Name_Type;
      Full_ALI_Base    : File_Name_Type;
      Stub_Object      : File_Name_Type;
      Stub_ALI_Base    : File_Name_Type;
      Stub_ALI         : File_Name_Type;
      Unit             : Unit_Id := ALIs.Table (A).Last_Unit;
      Arguments        : Argument_Vec;

   begin
      if Units.Table (Unit).Shared_Passive then
         Unit := ALIs.Table (A).First_Unit;
      end if;

      Full_Unit_File := Units.Table (Unit).Sfile;
      Full_ALI_File  := Dir (Monolithic_Obj_Dir, ALIs.Table (A).Afile);
      Full_ALI_Base  := ALIs.Table (A).Afile;

      if not Is_Regular_File (Full_ALI_File) then

         --  No ALI in monolithic application: this must be the PCS_Conf_Unit.
         --  In this case the Full_ALI_File is deemed to be older than any
         --  existing stubs file, and so the stubs will be considered to be
         --  always up-to-date if present.

         pragma Assert
           (PCS_Conf_Unit /= No_Name
              and then ALIs.Table (A).Uname = PCS_Conf_Unit);
         null;
      end if;

      --  Determination of stub ALI file name

      --  Note that the base name the compiler will use for the stubs ALI
      --  and object files (which cannot be overridden) may be different from
      --  thos of the full application  (because under some non-standard
      --  naming convention, the base name of the spec might be different
      --  from the base name of the body, which is also the base name of the
      --  monolithic ALI). In that case, the output files are renamed after
      --  compilation.

      Stub_ALI_Base    := To_Afile (Strip_Directory (Full_Unit_File));
      Stub_ALI         := Dir (Stub_Dir_Name, Stub_ALI_Base);
      Stub_Object      := To_Ofile (Stub_ALI);

      --  Do we need to regenerate the caller stub and its ali?

      if not Is_Regular_File (Stub_Object) then
         if Verbose_Mode then
            Write_Missing_File (Stub_Object);
         end if;

      elsif not Is_Regular_File (Stub_ALI) then
         if Verbose_Mode then
            Write_Missing_File (Stub_ALI);
         end if;

      elsif File_Time_Stamp (Full_ALI_File) > File_Time_Stamp (Stub_ALI) then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_ALI_File, Stub_ALI);
         end if;

      else
         if not Quiet_Mode then
            Message ("  ", ALIs.Table (A).Uname, "caller stubs is up to date");
         end if;
         return;
      end if;

      --  Here if stubs need to be rebuilt

      if not Quiet_Mode then
         Message
           ("building", ALIs.Table (A).Uname,
            "caller stubs from", Normalize_CWD (Full_Unit_File));
      end if;

      Push (Arguments, Stub_Flag);

      if Project_File_Name = Null_Unbounded_String then
         Push (Arguments, Object_Dir_Flag);
         Push (Arguments, Stub_Dir);

      else
         Push (Arguments, Project_File_Flag);
         Push (Arguments, Dir (Stub_Dir, Part_Prj_File_Name));
      end if;

      Compile (Full_Unit_File, Arguments, Fatal => False);

      --  Now rename output files if required (see comments above)

      if Full_ALI_Base /= Stub_ALI_Base then
         declare
            Final_ALI : constant File_Name_Type :=
                          Dir (Stub_Dir_Name, Full_ALI_Base);
            Final_Object : constant File_Name_Type := To_Ofile (Final_ALI);

         begin
            Do_Rename (Stub_ALI,    Final_ALI);
            Do_Rename (Stub_Object, Final_Object);
         end;
      end if;
   end Generate_Stub;

   --------------------------
   -- Get_Absolute_Command --
   --------------------------

   function Get_Absolute_Command return String is
      Cmd : constant String := Command_Name;
   begin
      for J in Cmd'Range loop
         if Cmd (J) = Dir_Separator then
            return Normalize_Pathname (Cmd, Resolve_Links => Resolve_Links);
         end if;
      end loop;

      --  Case of command name containing no directory separator

      declare
         Abs_Command_Access : GNAT.OS_Lib.String_Access :=
           Locate_Exec_On_Path (Cmd);
         Abs_Command : constant String := Abs_Command_Access.all;
      begin
         Free (Abs_Command_Access);
         return Abs_Command;
      end;

   end Get_Absolute_Command;

   ------------------
   -- Get_Env_Vars --
   ------------------

   function Get_Env_Vars
     (P          : Partition_Id;
      Q          : Character := ' ';
      Names_Only : Boolean) return String
   is
      V : Env_Var_Id;
   begin
      --  Export environment vars for remote partitions

      Name_Len := 0;

      V := Partitions.Table (P).First_Env_Var;
      while V /= No_Env_Var_Id loop
         if V = Partitions.Table (P).First_Env_Var then
            Add_Str_To_Name_Buffer ("env ");
         end if;
         Get_Name_String_And_Append (Env_Vars.Table (V).Name);
         if not Names_Only then
            Add_Char_To_Name_Buffer ('=');
            Add_Char_To_Name_Buffer (Q);
            Add_Str_To_Name_Buffer ("${");
            Get_Name_String_And_Append (Env_Vars.Table (V).Name);
            Add_Char_To_Name_Buffer ('}');
            Add_Char_To_Name_Buffer (Q);
         end if;
         Add_Str_To_Name_Buffer (" ");
         V := Env_Vars.Table (V).Next_Env_Var;
      end loop;

      return Name_Buffer (1 .. Name_Len);
   end Get_Env_Vars;

   ----------
   -- Hash --
   ----------

   function Hash (S : String_Ptr) return Header_Num is
      function Hash is new GNAT.HTable.Hash (Header_Num);
   begin
      if S = null then
         return Header_Num'First;
      end if;
      return Hash (S.all);
   end Hash;

   ------------------------
   -- Hide_Stubbed_Units --
   ------------------------

   procedure Hide_Stubbed_Units (Hide : Boolean := True) is
      type What is (ALI, Object);
      Filenames : array (Boolean, What) of File_Name_Type;
   begin
      for J in ALIs.First .. ALIs.Last loop
         if Needs_Stub (J) then
            Filenames :=
              (False =>  --  Visible paths
                 (ALI    => Dir (Monolithic_Obj_Dir, ALIs.Table (J).Afile),
                  Object => ALIs.Table (J).Ofile),

               True =>   --  Hidden paths
                 (ALI    => Dir (Hidden_Stubs_Dir, ALIs.Table (J).Afile),
                  Object => Dir (Hidden_Stubs_Dir,
                                 Strip_Directory (ALIs.Table (J).Ofile))));

            for F in What loop
               if Is_Regular_File (Filenames (not Hide, F)) then
                  Do_Rename (Source => Filenames (not Hide, F),
                             Target => Filenames (Hide, F));
               end if;
            end loop;
         end if;
      end loop;
   end Hide_Stubbed_Units;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Build_Stamp_File    := Id ("glade.sta");
      Partition_Main_File := Id ("partition");
      Partition_Main_Name := Id ("Partition");
   end Initialize;

   -------------------------
   -- Location_List_Image --
   -------------------------

   function Location_List_Image (Location : Location_Id) return Name_Id is
      L : Location_Id := Location;

   begin
      Name_Len := 0;
      loop
         Get_Name_String_And_Append (Locations.Table (L).Major);
         if Present (Locations.Table (L).Minor) then
            Add_Str_To_Name_Buffer ("://");
            Get_Name_String_And_Append (Locations.Table (L).Minor);
         end if;
         L := Locations.Table (L).Next_Location;
         exit when L = No_Location_Id;
         Add_Char_To_Name_Buffer (' ');
      end loop;
      return Quote (Name_Find);
   end Location_List_Image;

   ----------------
   -- Needs_Stub --
   ----------------

   function Needs_Stub (A : ALI_Id) return Boolean is
      Unit : constant Unit_Id := ALIs.Table (A).Last_Unit;
   begin
      return (Units.Table (Unit).RCI
                or else
              Units.Table (Unit).Shared_Passive)
        and then not Units.Table (Unit).Is_Generic;
   end Needs_Stub;

   ------------------------
   -- Partition_Dir_Flag --
   ------------------------

   function Partition_Dir_Flag (P : Partition_Id) return String is
   begin
      Set_Str_To_Name_Buffer ("-XPARTITION_DIR=");
      Get_Name_String_And_Append (Partitions.Table (P).Partition_Dir);
      return Name_Buffer (1 .. Name_Len);
   end Partition_Dir_Flag;

   --  Local declarations for Prefix

   Exec_Abs_Name : constant String := Get_Absolute_Command;
   Exec_Abs_Dir  : constant String := Dir_Name (Exec_Abs_Name);

   --  Strip trailing separator and remove last component ("bin")

   Exec_Prefix   : constant String  :=
                     Dir_Name (Exec_Abs_Dir (Exec_Abs_Dir'First
                                          .. Exec_Abs_Dir'Last - 1));

   ------------
   -- Prefix --
   ------------

   function Prefix (Check_For : String) return String is
   begin
      if Is_Readable_File (Exec_Prefix & Check_For) then
         return Exec_Prefix;
      else
         return Platform.Prefix & Dir_Separator;
      end if;
   end Prefix;

   -------------------------
   -- Prepare_Directories --
   -------------------------

   procedure Prepare_Directories is
      Afile   : File_Name_Type;
      Unit    : Unit_Id;
      Uname   : Unit_Name_Type;
      Current : Partition_Type;

   begin
      if Project_File_Name /= Null_Unbounded_String then
         Generate_Partition_Project_File (Stub_Dir_Name);
      end if;

      for J in Partitions.First + 1 .. Partitions.Last loop
         Current := Partitions.Table (J);
         if Current.To_Build and then Current.Passive /= BTrue then

            --  Create directories in which resp skels, main partition
            --  unit, elaboration unit and executables are stored

            Create_Dir (Current.Partition_Dir);

            if Present (Current.Executable_Dir) then
               Get_Name_String (Current.Executable_Dir);
               Set_Str_To_Name_Buffer
                 (Normalize_Pathname
                    (Name_Buffer (1 .. Name_Len),
                     Resolve_Links => Resolve_Links));
               Current.Executable_Dir := Name_Find;
               Create_Dir (Current.Executable_Dir);
            end if;

            if Project_File_Name /= Null_Unbounded_String then
               Generate_Partition_Project_File (Current.Partition_Dir, J);
            end if;

            for K in ALIs.First .. ALIs.Last loop
               Afile := ALIs.Table (K).Afile;
               Unit  := ALIs.Table (K).Last_Unit;
               Uname := Units.Table (Unit).Uname;

               --  Remove possible copies of unit object

               if (not Units.Table (Unit).RCI
                   and then not Units.Table (Unit).Shared_Passive)
                 or else Get_Partition_Id (Uname) /= J
               then
                  Afile := Strip_Directory (Afile);
                  Afile := Dir (Current.Partition_Dir, Afile);
                  Delete_File (Afile);
                  Delete_File (To_Ofile (Afile));
               end if;
            end loop;
         end if;
      end loop;
   end Prepare_Directories;

   -----------------------
   -- Rebuild_Partition --
   -----------------------

   function Rebuild_Partition (P : Partition_Id) return Boolean is
      Current     : Partition_Type renames Partitions.Table (P);
      Executable  : File_Name_Type renames Current.Executable_File;
      Most_Recent : File_Name_Type renames Current.Most_Recent;
      Stamp_File  : File_Name_Type;

      First  : Text_Ptr;
      Last   : Text_Ptr;
      Buffer : Text_Buffer_Ptr;

      function Read_Time_Stamp return Time_Stamp_Type;

      ---------------------
      -- Read_Time_Stamp --
      ---------------------

      function Read_Time_Stamp return Time_Stamp_Type is
         S : Time_Stamp_Type;

      begin
         for I in S'Range loop
            if Buffer (First) not in '0' .. '9' then
               return Dummy_Time_Stamp;
            end if;
            S (I) := Buffer (First);
            First := First + 1;
         end loop;

         while Buffer (First) = ASCII.LF
           or else Buffer (First) = ASCII.CR
         loop
            First := First + 1;
         end loop;

         return S;
      end Read_Time_Stamp;

      Old_Stamp : Time_Stamp_Type;
      New_Stamp : Time_Stamp_Type;

   begin
      --  Check that executable exists and is up to date

      if not Is_Regular_File (Executable) then
         return True;
      end if;

      if File_Time_Stamp (Most_Recent) > File_Time_Stamp (Executable) then
         return True;
      end if;

      --  Check that stamp file exists

      Stamp_File := Dir (Partitions.Table (P).Partition_Dir, Build_Stamp_File);
      if not Is_Regular_File (Stamp_File) then
         return True;
      end if;

      --  Check stamps from stamp file corresponds to the current ones.

      Read_File (Stamp_File, First, Last, Buffer);

      --  Check Configuration File Stamp

      Old_Stamp := Read_Time_Stamp;
      New_Stamp := File_Time_Stamp (Configuration_File_Name);

      if Old_Stamp /= New_Stamp then
         return True;
      end if;

      --  Check Executable File Stamp

      Old_Stamp := Read_Time_Stamp;
      New_Stamp := File_Time_Stamp (Executable);

      if Old_Stamp /= New_Stamp then
         return True;
      end if;

      --  Check Most Recent Object File Stamp

      Old_Stamp := Read_Time_Stamp;
      New_Stamp := File_Time_Stamp (Most_Recent);

      if Old_Stamp /= New_Stamp then
         return True;
      end if;

      return False;
   end Rebuild_Partition;

   ----------------------
   -- Register_Backend --
   ----------------------

   procedure Register_Backend (PCS_Name : String; The_Backend : Backend_Access)
   is
   begin
      All_Backends.Set (new String'(PCS_Name), The_Backend);
   end Register_Backend;

   --------------------------
   -- Register_Casing_Rule --
   --------------------------

   procedure Register_Casing_Rule (S : String) is
   begin
      Rules_Last := Rules_Last + 1;
      Rules (Rules_Last).Size := S'Length;
      Rules (Rules_Last).Into := new String'(S);
      Rules (Rules_Last).From := new String'(S);
      To_Lower (Rules (Rules_Last).From.all);
   end Register_Casing_Rule;

   ----------------
   -- Write_Call --
   ----------------

   procedure Write_Call
     (SP         : Unit_Name_Type;
      N1         : Name_Id := No_Name;
      S1         : String  := No_Str;
      N2         : Name_Id := No_Name;
      S2         : String  := No_Str;
      N3         : Name_Id := No_Name;
      S3         : String  := No_Str;
      I1_Present : Boolean := False;
      I1         : Int     := -1)
   is
      Max_String_Length : constant := 64;
      N_Params          : Integer  := 0;

      procedure Write_Parameter (P : String);
      procedure Write_Separator;

      ---------------------
      -- Write_Parameter --
      ---------------------

      procedure Write_Parameter (P : String) is
         F : Natural := P'First;
         L : Natural := P'Last;

      begin
         if P (F) /= '"' then --  "
            Write_Str (P);
            return;
         end if;
         F := F + 1;
         for J in 1 .. (P'Length - 2) / Max_String_Length loop
            L := F + Max_String_Length - 1;
            Write_Char ('"'); --  "
            Write_Str  (P (F .. L));
            Write_Line (""" &");
            Write_Indentation;
            F := L + 1;
         end loop;
         Write_Char ('"'); --  "
         Write_Str  (P (F .. P'Last));
      end Write_Parameter;

      ---------------------
      -- Write_Separator --
      ---------------------

      procedure Write_Separator is
      begin
         N_Params := N_Params + 1;
         if N_Params = 1 then
            Write_Eol;
            Increment_Indentation;
            Write_Indentation (-1);
            Write_Str ("(");
         else
            Write_Str (",");
            Write_Eol;
            Write_Indentation;
         end if;
      end Write_Separator;

   begin
      Write_Indentation;
      Write_Name (SP);
      if Present (N1) then
         Write_Separator;
         Write_Parameter (Get_Name_String (N1));
      end if;
      if S1 /= No_Str then
         Write_Separator;
         Write_Parameter (S1);
      end if;
      if Present (N2) then
         Write_Separator;
         Write_Parameter (Get_Name_String (N2));
      end if;
      if S2 /= No_Str then
         Write_Separator;
         Write_Parameter (S2);
      end if;
      if Present (N3) then
         Write_Separator;
         Write_Parameter (Get_Name_String (N3));
      end if;
      if S3 /= No_Str then
         Write_Separator;
         Write_Parameter (S3);
      end if;
      if I1_Present then
         Write_Separator;
         Write_Int (I1);
      end if;
      if N_Params /= 0 then
         Write_Str (")");
         Decrement_Indentation;
      end if;
      Write_Str (";");
      Write_Eol;
   end Write_Call;

   -----------------
   -- Write_Image --
   -----------------

   procedure Write_Image (I : out Name_Id; H : Host_Id; P : Partition_Id) is
   begin
      if H /= No_Host_Id then
         if not Hosts.Table (H).Static then
            if Hosts.Table (H).Import = Shell_Import then
               Name_Len := 0;
               Add_Str_To_Name_Buffer ("""`");
               Get_Name_String_And_Append (Hosts.Table (H).External);
               Add_Char_To_Name_Buffer (' ');
               Get_Name_String_And_Append (Partitions.Table (P).Name);
               Add_Str_To_Name_Buffer ("`""");
               I := Name_Find;

            elsif Hosts.Table (H).Import = Ada_Import then
               Get_Name_String (Hosts.Table (H).External);
               Add_Char_To_Name_Buffer ('(');
               Get_Name_String_And_Append (Partitions.Table (P).Name);
               Add_Char_To_Name_Buffer (')');
               I := Name_Find;

            else
               raise Parsing_Error;
            end if;

         else
            Name_Len := 0;
            Add_Char_To_Name_Buffer ('"'); -- "
            Get_Name_String_And_Append (Hosts.Table (H).Name);
            Add_Char_To_Name_Buffer ('"'); -- "
            I := Name_Find;
         end if;

      else
         I := No_Name;
      end if;
   end Write_Image;

   ------------------------
   -- Write_With_Clause --
   ------------------------

   procedure Write_With_Clause
     (W : Name_Id;
      U : Boolean := False;
      E : Boolean := False) is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("with ");
      Get_Name_String_And_Append (W);
      Add_Char_To_Name_Buffer (';');
      Write_Str (Name_Buffer (1 .. Name_Len));
      Write_Eol;
      if U then
         Name_Buffer (1 .. 4) := "use ";
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_Eol;
      end if;
      if E then
         Write_Call (Id ("pragma Elaborate_All"), W);
      end if;
   end Write_With_Clause;

end XE_Back;
