------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1995-2004 Free Software Foundation, Inc.           --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with XE;          use XE;
with XE_Flags;    use XE_Flags;
with XE_Front;    use XE_Front;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Types;    use XE_Types;
with XE_Units;    use XE_Units;
with XE_Utils;    use XE_Utils;

package body XE_Back is

   Build_Stamp_File      : File_Name_Type;
   Partition_Main_File   : File_Name_Type;
   Partition_Main_Name   : Unit_Name_Type;

   type RU_Id is
     (RU_PolyORB,
      RU_PolyORB_Access_Points,
      RU_PolyORB_Access_Points_IIOP,
      RU_PolyORB_Binding_Data,
      RU_PolyORB_Binding_Data_IIOP,
      RU_PolyORB_Initialization,
      RU_PolyORB_ORB,
      RU_PolyORB_ORB_No_Tasking,
      RU_PolyORB_ORB_Thread_Pool,
      RU_PolyORB_POA_Config,
      RU_PolyORB_POA_Config_RACWs,
      RU_PolyORB_Setup,
      RU_PolyORB_Setup_IIOP,
      RU_PolyORB_Tasking,
      RU_PolyORB_Tasking_Full_Tasking,
      RU_PolyORB_Tasking_No_Tasking,
      RU_System_Partition_Interface,
      RU_System_RPC,
      RU_System_RPC_Server);

   RU : array (RU_Id) of Unit_Name_Type;

   type RE_Id is
     (RE_Initialize_World,
      RE_Run,
      RE_The_ORB);

   RE : array (RE_Id) of Unit_Name_Type;

   RE_Unit_Table : constant array (RE_Id) of RU_Id :=
     (RE_Initialize_World => RU_PolyORB_Initialization,
      RE_Run              => RU_PolyORB_ORB,
      RE_The_ORB          => RU_PolyORB_Setup);

   function "and" (N : Name_Id; S : String) return Name_Id;
   function "and" (L, R : Name_Id) return Name_Id;

   type Casing_Rule is record
      Size : Natural;
      From : String_Access;
      Into : String_Access;
   end record;

   Rules : array (1 .. 64) of Casing_Rule;
   Rules_Last : Natural := 0;

   procedure Apply_Casing_Rules (S : in out String);
   procedure Register_Casing_Rule (S : String);

   procedure Generate_Executable_File (P : Partition_Id);
   --  Compile main partition file and elaboration file.
   --  Bind and link partition to create executable.

   procedure Generate_Partition_Main_File (P : Partition_Id);
   --  Create a procedure which "withes" all the RCI or SP receivers
   --  of the partition and insert the main procedure if needed.

   procedure Generate_Partition_Project_File
     (D : Directory_Name_Type;
      P : Partition_Id := No_Partition_Id);

   procedure Generate_Starter_File;
   --  Create the starter file to launch the other partitions from
   --  main partition subprogram. This can be a shell script or an Ada
   --  program.

   procedure Generate_Stamp_File (P : Partition_Id);
   --  Create a stamp file in which the executable file stamp and the
   --  configuration file stamp are stored.

   procedure Generate_Stub (A : ALI_Id);
   --  Create stub and skel for a RCI or SP unit.

   procedure Generate_Skel (A : ALI_Id; P : Partition_Id);
   --  Create skel for a RCI or SP unit and store them in the
   --  directory of the partition on which the unit is assigned.

   procedure Write_Image (I : out Name_Id; H : Host_Id; P : Partition_Id);
   --  Write in I the text to get the partition hostname. This can be
   --  a shell script.

   function Rebuild_Partition (P : Partition_Id) return Boolean;
   --  Check various file stamps to decide whether the partition
   --  executable should be regenerated. Load the partition stamp file
   --  which contains the configuration file stamp, executable file
   --  stamp and the most recent object file stamp. If one of these
   --  stamps is not the same, rebuild the partition. Note that for
   --  instance we ensure that a partition executable coming from
   --  another configuration is detected as inconsistent.

   procedure Write_Call
     (SP : Unit_Name_Type;
      N1 : Name_Id := No_Name;
      S1 : String  := No_Str;
      N2 : Name_Id := No_Name;
      S2 : String  := No_Str;
      N3 : Name_Id := No_Name;
      S3 : String  := No_Str);
   --  Insert a procedure call. The first non-null parameter
   --  is supposed to be the procedure name. The next parameters
   --  are parameters for this procedure call.

   procedure Write_With_Clause
     (W : Name_Id;
      U : Boolean := False;
      E : Boolean := False);
   --  Add a with clause W, a use clause when U is true and an
   --  elaborate clause when E is true.

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

   -------------
   -- Backend --
   -------------

   procedure Backend is
      Afile   : File_Name_Type;
      PID     : Partition_Id;
      Unit    : Unit_Id;
      Uname   : Unit_Name_Type;
      Current : Partition_Type;

   begin
      if Project_File_Name /= null then
         Generate_Partition_Project_File (Stub_Dir_Name);
      end if;

      for J in Partitions.First + 1 .. Partitions.Last loop
         Current := Partitions.Table (J);
         if Current.To_Build and then Current.Passive /= BTrue then

            --  Create directories in which resp skels, main partition
            --  unit, elaboration unit and executables are stored

            Create_Dir (Current.Partition_Dir);
            Create_Dir (Current.Executable_Dir);

            if Present (Current.Executable_Dir) then
               Get_Name_String (Current.Executable_Dir);
               Set_Str_To_Name_Buffer
                 (Normalize_Pathname (Name_Buffer (1 .. Name_Len)));
               Partitions.Table (J).Executable_Dir := Name_Find;
            end if;

            Get_Name_String (Current.Executable_File);
            Set_Str_To_Name_Buffer
              (Normalize_Pathname (Name_Buffer (1 .. Name_Len)));
            Current.Executable_File := Name_Find;

            if Project_File_Name /= null then
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

      --  Create stub files. Create skel files as well when we have to
      --  build the partition on which this unit is mapped.

      for J in ALIs.First .. ALIs.Last loop
         Unit := ALIs.Table (J).Last_Unit;

         if not Units.Table (Unit).Is_Generic
           and then (Units.Table (Unit).RCI
                     or else Units.Table (Unit).Shared_Passive)
         then
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
      end loop;

      --  Create partition files and fill partition directories

      for J in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (J).To_Build then
            Current := Partitions.Table (J);

            if Current.To_Build and then Current.Passive /= BTrue then
               if Rebuild_Partition (J) then
                  if not Quiet_Mode then
                     Message ("building partition", Current.Name);
                  end if;

                  Generate_Partition_Main_File (J);
                  Generate_Executable_File (J);
                  Generate_Stamp_File (J);
               end if;

            elsif Verbose_Mode then
               Message ("no need to expand", Current.Name);
            end if;
         end if;
      end loop;

      Generate_Starter_File;
   end Backend;

   ------------------------------
   -- Generate_Executable_File --
   ------------------------------

   procedure Generate_Executable_File (P : Partition_Id) is
      Current    : Partition_Type renames Partitions.Table (P);
      Executable : File_Name_Type;
      Directory  : Directory_Name_Type;
      I_Part_Dir : String_Access;
      Comp_Args  : String_List (1 .. 8);
      Make_Args  : String_List (1 .. 8);
      Sfile      : File_Name_Type;
      Prj_Fname  : File_Name_Type;
      Length     : Natural := 6;

   begin
      Executable := Current.Executable_File;
      Directory  := Current.Partition_Dir;

      Name_Len := 2;
      Name_Buffer (1) := '-';
      Name_Buffer (2) := 'I';
      Get_Name_String_And_Append (Directory);
      I_Part_Dir := new String'(Name_Buffer (1 .. Name_Len));

      --  Give the priority to partition and stub directory against
      --  current directory.

      Comp_Args (1) := E_Current_Dir;
      Comp_Args (2) := I_Part_Dir;
      Comp_Args (3) := I_Stub_Dir;
      Comp_Args (4) := I_Current_Dir;

      --  If there is no project file, then save ali and object files
      --  in partition directory.

      if Project_File_Name = null then
         Comp_Args (5) := Object_Dir_Flag;
         Comp_Args (6) := new String'(Get_Name_String (Directory));

      else
         Comp_Args (5) := Project_File_Flag;
         Prj_Fname     := Dir (Directory, Part_Prj_File_Name);
         Comp_Args (6) := new String'(Get_Name_String (Prj_Fname));
      end if;

      --  We already checked the consistency of all the partition
      --  units. In case of an inconsistency of exception mode, we may
      --  have to rebuild some parts of garlic (units configured just
      --  for this partition). Note that some parts of Garlic may have
      --  been already recompiled when the monolithic application was
      --  initially build. Some bodies may be missing as they are
      --  assigned to partitions we do not want to build. So compile
      --  silently and do not exit on errors (keep going).

      Sfile := Partition_Main_File & ADB_Suffix_Id;
      if Project_File_Name = null then
         Sfile := Dir (Directory, Sfile);
         Comp_Args (7) := Compile_Only_Flag;
         Comp_Args (8) := Keep_Going_Flag;
         Length := 8;
      end if;
      Build (Sfile, Comp_Args (1 .. Length), Fatal => False, Silent => True);

      Free (Comp_Args (6));

      --  Now we just want to bind and link as the ALI files are now
      --  consistent.

      Make_Args (1) := E_Current_Dir;
      Make_Args (2) := I_Part_Dir;
      Make_Args (3) := I_Stub_Dir;
      Make_Args (4) := I_Current_Dir;
      Make_Args (5) := Bind_Only_Flag;
      Make_Args (6) := Link_Only_Flag;

      if Project_File_Name = null then
         Make_Args (7) := Output_Flag;
         Make_Args (8) := new String'(Get_Name_String (Executable));

      else
         Make_Args (7) := Project_File_Flag;
         Prj_Fname := Dir (Directory, Part_Prj_File_Name);
         Make_Args (8) := new String'(Get_Name_String (Prj_Fname));
      end if;

      Build (Sfile, Make_Args, Fatal => True, Silent => False);

      Free (Make_Args (2));
      Free (Make_Args (8));
   end Generate_Executable_File;

   ----------------------------------
   -- Generate_Partition_Main_File --
   ----------------------------------

   procedure Generate_Partition_Main_File (P : Partition_Id) is

      Filename  : File_Name_Type;
      File      : File_Descriptor;
      Current   : Partition_Type renames Partitions.Table (P);
      Conf_Unit : Conf_Unit_Id;

   begin
      Filename := Partition_Main_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);
      Write_Line  ("pragma Warnings (Off);");

      Write_With_Clause (RU (RU_PolyORB_ORB), False, True);
      Write_With_Clause (RU (RU_PolyORB_Initialization), False, True);
      Write_With_Clause (RU (RU_PolyORB_Setup), False, True);
      Write_With_Clause (RU (RU_PolyORB_Setup_IIOP), False, True);

      if Current.Tasking = 'N' then
         Write_With_Clause (RU (RU_PolyORB_Tasking_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_Binding_Data_IIOP));

      else
         Write_With_Clause (RU (RU_PolyORB_Tasking_Full_Tasking));

         if Current.Tasking = 'T' then
            Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
            Write_With_Clause (RU (RU_PolyORB_Binding_Data_IIOP));

         else
            Write_With_Clause (RU (RU_PolyORB_ORB_Thread_Pool));
            Write_With_Clause (RU (RU_PolyORB_Access_Points_IIOP));
            Write_With_Clause (RU (RU_PolyORB_POA_Config_RACWs));
         end if;
      end if;

      --  Assign RCI or SP skels on the partition

      Conf_Unit := Current.First_Unit;
      while Conf_Unit /= No_Conf_Unit_Id loop
         Write_With_Clause (Conf_Units.Table (Conf_Unit).Name);
         Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
      end loop;

      --  Assign the RCI or SP stubs to compare version with skels

      for J in Current.First_Stub .. Current.Last_Stub loop
         Write_With_Clause (Stubs.Table (J));
      end loop;

      --  Add termination package and locking mechanisms if needed

      Write_Str  ("procedure ");
      Write_Name (Partition_Main_Name);
      Write_Line (" is");
      Write_Line ("begin");

      Increment_Indentation;
      Write_Call (RE (RE_Initialize_World));

      if Current.Tasking = 'P' then
         Write_Call (RE (RE_Run), RE (RE_The_ORB), "May_Poll => True");
      end if;

      --   Invoke main subprogram through Run routine

      if Present (Current.Main_Subprogram) then
         Write_Call (Current.Main_Subprogram);
      end if;

      Decrement_Indentation;
      Write_Str  ("end ");
      Write_Name (Partition_Main_Name);
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Partition_Main_File;

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
      Write_Str  ("project Partition extends """);
      Write_Str  (Project_File_Name.all);
      Write_Line (""" is");
      Write_Line ("   for Object_Dir use ""."";");
      if P /= No_Partition_Id then
         Write_Str  ("   for Exec_Dir use """);
         Write_Name (Partitions.Table (P).Executable_Dir);
         Write_Line (""";");
         Write_Line ("   package Builder is");
         Write_Str  ("      for Executable (""partition.adb"") use """);
         Write_Name (Partitions.Table (P).Name);
         Write_Line (""";");
         Write_Line ("   end Builder;");
      end if;
      Write_Line ("end Partition;");
      Close (Prj_File);
      Set_Standard_Output;
   end Generate_Partition_Project_File;

   -------------------
   -- Generate_Skel --
   -------------------

   procedure Generate_Skel (A : ALI_Id; P : Partition_Id) is
      Obsolete       : Boolean;
      Full_Unit_File : File_Name_Type;
      Full_ALI_File  : File_Name_Type;
      Skel_Object    : File_Name_Type;
      Skel_ALI       : File_Name_Type;
      Arguments      : Argument_List (1 .. 3);
      Part_Prj_Fname : File_Name_Type;
      Directory      : Directory_Name_Type
        renames Partitions.Table (P).Partition_Dir;

   begin
      Full_Unit_File := Units.Table (ALIs.Table (A).First_Unit).Sfile;
      Full_ALI_File  := ALIs.Table (A).Afile;
      Skel_ALI       := To_Afile (Strip_Directory (Full_Unit_File));
      Skel_ALI       := Dir (Directory, Skel_ALI);
      Skel_Object    := To_Ofile (Skel_ALI);

      --  Do we need to generate the skel files

      Obsolete := False;
      if not Is_Regular_File (Skel_Object) then
         if Verbose_Mode then
            Write_Missing_File (Skel_Object);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete
        and then not Is_Regular_File (Skel_ALI)
      then
         if Verbose_Mode then
            Write_Missing_File (Skel_ALI);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete
        and then File_Time_Stamp (Full_ALI_File) > File_Time_Stamp (Skel_ALI)
      then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_ALI_File, Skel_ALI);
         end if;
         Obsolete := True;
      end if;

      if Obsolete then
         if not Quiet_Mode then
            Message
              ("building", ALIs.Table (A).Uname,
               "receiver stubs from", Normalize_CWD (Full_Unit_File));
         end if;

         Arguments (1) := Skel_Flag;

         if Project_File_Name = null then
            Arguments (2) := Object_Dir_Flag;
            Arguments (3) := new String'(Get_Name_String (Directory));

         else
            Arguments (2) := Project_File_Flag;
            Part_Prj_Fname := Dir (Directory, Part_Prj_File_Name);
            Get_Name_String (Part_Prj_Fname);
            Arguments (3) := new String'(Name_Buffer (1 .. Name_Len));
         end if;

         Compile (Full_Unit_File, Arguments, Fatal => False, Silent => True);

         Free (Arguments (3));
      elsif not Quiet_Mode then
         Message ("  ", ALIs.Table (A).Uname, "receiver stubs is up to date");
      end if;
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

   procedure Generate_Starter_File is

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
         --    <rshcmd> <host> <rshopts> "'<pn>' --detach ...
         --         ... --boot_location '<bs>' <cline> &" ...
         --         ... < /dev/null > /dev/null 2>&1

         if P = Main_Partition then
            Int_Quote := '"';  -- "
         end if;

         if P /= Main_Partition then
            Write_Name (Get_Rsh_Command);
            Write_Str  (" $");
            Write_Name (Current.Name);
            Write_Str  ("_HOST ");
            Write_Name (Get_Rsh_Options);
            Write_Char (' ');
            Write_Char (Ext_Quote);
         end if;

         Write_Name (To_Absolute_File (Current.Executable_File));
         Write_Str  (" --boot_location ");
         Write_Char (Int_Quote);
         Write_Str  ("$BOOT_LOCATION");
         Write_Char (Int_Quote);
         Write_Name (Current.Command_Line);

         if P /= Main_Partition then
            Write_Str  (" --detach &");
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

   begin
      --  If all the partitions are not to build then do not build the
      --  launcher partition.

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
            Write_Line  ("PATH=/usr/ucb:${PATH}");

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
      Obsolete       : Boolean;
      Full_Unit_File : File_Name_Type;
      Full_ALI_File  : File_Name_Type;
      Stub_Object    : File_Name_Type;
      Stub_ALI       : File_Name_Type;
      Unit           : Unit_Id := ALIs.Table (A).Last_Unit;
      Arguments      : Argument_List (1 .. 3);
      Part_Prj_Fname : File_Name_Type := No_File_Name;

   begin
      if Units.Table (Unit).Shared_Passive then
         Unit := ALIs.Table (A).First_Unit;
      end if;

      Full_Unit_File := Units.Table (Unit).Sfile;
      Full_ALI_File  := ALIs.Table (A).Afile;
      Stub_ALI       := To_Afile (Strip_Directory (Full_Unit_File));
      Stub_ALI       := Dir (Stub_Dir_Name, Stub_ALI);
      Stub_Object    := To_Ofile (Stub_ALI);

      --  Do we need to regenerate the caller stub and its ali

      Obsolete := False;
      if not Is_Regular_File (Stub_Object) then
         if Verbose_Mode then
            Write_Missing_File (Stub_Object);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete
        and then not Is_Regular_File (Stub_ALI)
      then
         if Verbose_Mode then
            Write_Missing_File (Stub_ALI);
         end if;
         Obsolete := True;
      end if;

      if not Obsolete
        and then File_Time_Stamp (Full_ALI_File) > File_Time_Stamp (Stub_ALI)
      then
         if Verbose_Mode then
            Write_Stamp_Comparison (Full_ALI_File, Stub_ALI);
         end if;
         Obsolete := True;
      end if;

      if Obsolete then
         if not Quiet_Mode then
            Message
              ("building", ALIs.Table (A).Uname,
               "caller stubs from", Normalize_CWD (Full_Unit_File));
         end if;

         Arguments (1) := Stub_Flag;

         if Project_File_Name = null then
            Arguments (2) := Object_Dir_Flag;
            Arguments (3) := Stub_Dir;

         else
            Arguments (2)  := Project_File_Flag;
            Part_Prj_Fname := Dir (Stub_Dir, Part_Prj_File_Name);
            Get_Name_String (Part_Prj_Fname);
            Arguments (3)  := new String'(Name_Buffer (1 .. Name_Len));
         end if;

         Compile (Full_Unit_File, Arguments, Fatal => False, Silent => True);

         if Present (Part_Prj_Fname) then
            Free (Arguments (3));
         end if;

      elsif not Quiet_Mode then
         Message ("  ", ALIs.Table (A).Uname, "caller stubs is up to date");
      end if;
   end Generate_Stub;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Position : Integer;
      Length   : Natural;

   begin
      Build_Stamp_File     := Id ("glade.sta");
      Partition_Main_File  := Id ("partition");
      Partition_Main_Name  := Id ("Partition");

      Register_Casing_Rule ("ORB");
      Register_Casing_Rule ("ORB");

      for U in RU_Id'First .. RU_Id'Last loop
         Set_Str_To_Name_Buffer (RU_Id'Image (U));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         Position := 0;
         RU (U)   := Name_Find;
         Length   := Name_Len;
         Set_Name_Table_Info (RU (U), RU_Id'Pos (U) + 1);

         while Name_Len > 0 loop
            if Name_Buffer (Name_Len) = '_' then
               Name_Len := Name_Len - 1;
               Position := Integer (Get_Name_Table_Info (Name_Find));
               exit when Position > 0;

            else
               Name_Len := Name_Len - 1;
            end if;
         end loop;

         --  When there is a parent, remove parent unit name from
         --  unit name to get real identifier.

         if Position > 0 then
            Set_Str_To_Name_Buffer (Name_Buffer (Name_Len + 2 .. Length));
            RU (U) := RU (RU_Id'Val (Position - 1)) and Name_Find;

         else
            Set_Str_To_Name_Buffer (Name_Buffer (1 .. Length));
            RU (U) := Name_Find;
         end if;

         if Debug_Mode then
            Message (U'Img & " = " & Get_Name_String (RU (U)));
         end if;
      end loop;

      for E in RE_Id loop
         Set_Str_To_Name_Buffer (RE_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         while Name_Buffer (Name_Len) in '0' .. '9'
           or else Name_Buffer (Name_Len) = '_'
         loop
            Name_Len := Name_Len - 1;
         end loop;

         RE (E) := RU (RE_Unit_Table (E)) and Name_Find;

         if Debug_Mode then
            Message (E'Img & " = " & Get_Name_String (RE (E)));
         end if;
      end loop;
   end Initialize;

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
     (SP : Unit_Name_Type;
      N1 : Name_Id := No_Name;
      S1 : String  := No_Str;
      N2 : Name_Id := No_Name;
      S2 : String  := No_Str;
      N3 : Name_Id := No_Name;
      S3 : String  := No_Str)
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
         for J in 1 .. (P'Length - 1) / Max_String_Length loop
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
