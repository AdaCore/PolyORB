------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                       X E _ B A C K . P O L Y O R B                      --
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
--                   GLADE  is maintained by AdaCore                        --
--                      (email: sales@adacore.com)                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with XE;          use XE;
with XE_Front;    use XE_Front;
with XE_Flags;    use XE_Flags;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Utils;    use XE_Utils;

with XE_Back;
pragma Elaborate_All (XE_Back);

package body XE_Back.PolyORB is

   type PolyORB_Backend is new Backend with null record;

   procedure Set_PCS_Dist_Flags (Self : access PolyORB_Backend);
   procedure Initialize (Self : access PolyORB_Backend);
   procedure Run_Backend (Self : access PolyORB_Backend);
   function Get_Detach_Flag (Self : access PolyORB_Backend) return Name_Id;

   Elaboration_File  : File_Name_Type;
   --  Partition elaboration unit

   Parameters_File   : File_Name_Type;
   --  Partition runtime parameters

   PCS_Project       : Name_Id;
   PCS_Project_File  : File_Name_Type;
   --  Project file for the PCS

   Dist_App_Project      : Name_Id;
   Dist_App_Project_File : File_Name_Type;
   --  Project file for the complete distributed application

   type RU_Id is
     (RU_PolyORB,
      RU_PolyORB_Binding_Data,
      RU_PolyORB_Binding_Data_GIOP,
      RU_PolyORB_Binding_Data_GIOP_IIOP,
      RU_PolyORB_Initialization,
      RU_PolyORB_ORB_Controller,
      RU_PolyORB_ORB_Controller_Workers,
      RU_PolyORB_ORB,
      RU_PolyORB_ORB_No_Tasking,
      RU_PolyORB_ORB_Thread_Pool,
      RU_PolyORB_DSA_P,
      RU_PolyORB_DSA_P_Remote_Launch,
      RU_PolyORB_Parameters,
      RU_PolyORB_Parameters_Partition,
      RU_PolyORB_Partition_Elaboration,
      RU_PolyORB_Setup,
      RU_PolyORB_Setup_Access_Points,
      RU_PolyORB_Setup_Access_Points_IIOP,
      RU_PolyORB_Setup_Base,
      RU_PolyORB_Setup_IIOP,
      RU_PolyORB_Setup_OA,
      RU_PolyORB_Setup_OA_Basic_POA,
      RU_PolyORB_Setup_Tasking,
      RU_PolyORB_Setup_Tasking_Full_Tasking,
      RU_PolyORB_Setup_Tasking_No_Tasking,
      RU_PolyORB_Utils,
      RU_PolyORB_Utils_Strings,
      RU_PolyORB_Utils_Strings_Lists,
      RU_System,
      RU_System_DSA_Services,
      RU_System_Partition_Interface,
      RU_System_RPC,
      RU_System_RPC_Server,
      RU_Tasking);

   RU : array (RU_Id) of Unit_Name_Type;

   type RE_Id is
     (RE_Check,
      RE_Launch_Partition,
      RE_Run,
      RE_Shutdown_World,
      RE_The_ORB);

   RE : array (RE_Id) of Unit_Name_Type;

   RE_Unit_Table : constant array (RE_Id) of RU_Id :=
     (RE_Check             => RU_System_Partition_Interface,
      RE_Launch_Partition  => RU_PolyORB_DSA_P_Remote_Launch,
      RE_Run               => RU_PolyORB_ORB,
      RE_Shutdown_World    => RU_PolyORB_Initialization,
      RE_The_ORB           => RU_PolyORB_Setup);

   ---------------------
   -- Parameter types --
   ---------------------

   type PS_Id is
     (PS_Tasking, PS_DSA, PS_DSA_Local_RCIs);

   PS : array (PS_Id) of Unit_Name_Type;

   type PE_Id is
     (PE_Max_Spare_Threads,
      PE_Max_Threads,
      PE_Min_Spare_Threads,
      PE_Rsh_Command,
      PE_Rsh_Options,
      PE_Termination_Initiator,
      PE_Termination_Policy);

   PE : array (PE_Id) of Unit_Name_Type;

   PE_Section_Table : constant array (PE_Id) of PS_Id :=
     (PE_Rsh_Command           => PS_DSA,
      PE_Rsh_Options           => PS_DSA,
      PE_Termination_Initiator => PS_DSA,
      PE_Termination_Policy    => PS_DSA,
      PE_Max_Spare_Threads     => PS_Tasking,
      PE_Max_Threads           => PS_Tasking,
      PE_Min_Spare_Threads     => PS_Tasking);

   -----------------------------
   -- Parameter table entries --
   -----------------------------

   type Parameter_Entry is record
      Var : Name_Id;
      Val : Name_Id;
   end record;

   Table : array (0 .. 31) of Parameter_Entry;
   Last  : Integer := -1;

   ---------------------------
   -- Generation Procedures --
   ---------------------------

   procedure Generate_Ada_Starter_Code;
   --  Generates Ada calls for starting the remote partitions

   procedure Generate_Elaboration_File (P : Partition_Id);
   --  Create the elaboration unit for the given partition. This unit
   --  overloads the default PCS settings.

   procedure Generate_Executable_File (P : Partition_Id);
   --  Compile main partition file and elaboration file.
   --  Bind and link partition to create executable.

   procedure Generate_Parameters_File (P : Partition_Id);
   --  Create parameters source unit for the given partition.

   procedure Generate_Partition_Main_File (P : Partition_Id);
   --  Create a procedure which "withes" all the RCI or SP receivers
   --  of the partition and insert the main procedure if needed.

   procedure Generate_Application_Project_Files;
   --  Generate a project file for the PCS, and a project file extending the
   --  one provided by the user and including a dependency upon the PCS
   --  project (the application-wide project file).

   function Strip (S : String) return Unit_Name_Type;
   --  Return the prefix and a possible suffix from S

   procedure Set_Conf (Var : PE_Id; Val : Name_Id);
   --  Add a new entry in the configuration table

   procedure Set_Conf (Section : Name_Id; Key : Name_Id; Val : Name_Id);
   --  Add a new entry in the configuration table

   procedure Reset_Conf;
   --  Clear the configuration table

   procedure Set_Corresponding_Project_File_Name (N : out File_Name_Type);
   --  Assuming the Name_Buffer contains a project name, set N to the name of
   --  the corrsponding project file. (Assumes that the project name is already
   --  all lowercase).

   -------------------------------
   -- Generate_Ada_Starter_Code --
   -------------------------------

   procedure Generate_Ada_Starter_Code
   is
      Env : constant String := Get_Environment_Vars_Command;
      Remote_Host : Name_Id;
   begin
      for J in Partitions.First + 1 .. Partitions.Last loop
         if J /= Main_Partition
           and then Partitions.Table (J).Passive /= BTrue
         then
            declare
               Partition   : Partition_Type renames Partitions.Table (J);
               Cmd : constant String := Get_Name_String
                 (To_Absolute_File (Partition.Executable_File)
                  & Partition.Command_Line);
               Full_Cmd : constant String := '"' & Env & Cmd & '"';
            begin
               Write_Image (Remote_Host, Partition.Host, J);
               if not Present (Remote_Host) then
                  Remote_Host := Id ("""localhost""");
               end if;

               Write_Call (RU (RE_Unit_Table (RE_Launch_Partition))
                           and RE (RE_Launch_Partition),
                           Remote_Host,
                           Full_Cmd);
            end;
         end if;
      end loop;
   end Generate_Ada_Starter_Code;

   ----------------------------------------
   -- Generate_Application_Project_Files --
   ----------------------------------------

   procedure Generate_Application_Project_Files
   is
      Prj_Fname  : File_Name_Type;
      Prj_File   : File_Descriptor;

      --  Get PolyORB/DSA installation directory using polyorb-config

      Status      : aliased Integer;
      Arg         : String_Access := new String'("--prefix");
      Install_Dir : constant String :=
                      Get_Command_Output
                        ("polyorb-config", (1 => Arg), "", Status'Access);
      DSA_Inc_Dir : constant String := Install_Dir & "/include/polyorb/";

      Secondary_PCS_Project      : Name_Id;
      Secondary_PCS_Project_File : File_Name_Type;
   begin
      Free (Arg);

      --  Create PCS project with all PCS sources except those units that
      --  need to be rebuilt for each partition, or that are overridden
      --  by each partition.

      Prj_Fname := Dir (Id (Root), PCS_Project_File);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);
      Write_Str  ("project ");
      Write_Name (PCS_Project);
      Write_Line (" is");
      Write_Line ("   for Object_Dir use """ & DSA_Inc_Dir & """;");
      Write_Line ("   for Source_Dirs use (""" & DSA_Inc_Dir & """);");
      Write_Line ("   for Locally_Removed_Files use");

      --  Overridden

      Write_Line ("     (""polyorb-parameters-partition.adb"",");
      Write_Line ("      ""polyorb-partition_elaboration.adb"",");

      --  Rebuilt

      Write_Line ("      ""polyorb-dsa_p-partitions.ads"",");
      Write_Line ("      ""polyorb-dsa_p-partitions.adb"");");

      Write_Str  ("end ");
      Write_Name (PCS_Project);
      Write_Line (";");
      Close (Prj_File);
      Set_Standard_Output;

      --  In the two project files below, we use ".." as the object directory,
      --  relative to the project directory, so that all objects are stored in
      --  the user's build directory.

      --  Create project for PCS units that need to be rebuilt per-partition

      Get_Name_String (PCS_Project);
      Add_Char_To_Name_Buffer ('1');
      Secondary_PCS_Project := Name_Find;
      Set_Corresponding_Project_File_Name (Secondary_PCS_Project_File);

      Prj_Fname := Dir (Id (Root), Secondary_PCS_Project_File);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);
      Write_Str  ("with """);
      Write_Name (PCS_Project);
      Write_Line (""";");
      Write_Str  ("project ");
      Write_Name (Secondary_PCS_Project);
      Write_Line (" is");
      Write_Str  ("   for Object_Dir use "".."";");
      Write_Line ("   for Source_Dirs use (""" & DSA_Inc_Dir & """);");
      Write_Line ("   for Source_Files use");
      Write_Line ("     (""polyorb-dsa_p-partitions.ads"",");
      Write_Line ("      ""polyorb-dsa_p-partitions.adb"");");
      Write_Str  ("end ");
      Write_Name (Secondary_PCS_Project);
      Write_Line (";");
      Close (Prj_File);
      Set_Standard_Output;

      --  Create application-wide project, extending user project file

      Prj_Fname := Dir (Id (Root), Dist_App_Project_File);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);
      Write_Str  ("with """);
      Write_Name (Secondary_PCS_Project);
      Write_Line (""";");
      Write_Str  ("project ");
      Write_Name (Dist_App_Project);
      Write_Str  (" extends """);
      Write_Str  (Project_File_Name.all);
      Write_Line (""" is");
      Write_Line ("   for Object_Dir use "".."";");
      Write_Str  ("   for Source_Dirs use ");
      Write_Str  (Strip_Directory (Project_File_Name.all));
      Write_Line ("'Source_Dirs;");

      Write_Str  ("end ");
      Write_Name (Dist_App_Project);
      Write_Line (";");
      Close (Prj_File);
      Set_Standard_Output;

      Free (Project_File_Name);

      --  Distributed app project file extends user provided project, and
      --  includes the PCS as well.

      Project_File_Name := new String'(
                             Normalize_Pathname (Get_Name_String (Prj_Fname)));
   end Generate_Application_Project_Files;

   -------------------------------
   -- Generate_Elaboration_File --
   -------------------------------

   procedure Generate_Elaboration_File (P : Partition_Id) is

      Filename     : File_Name_Type;
      File         : File_Descriptor;
      Current      : Partition_Type renames Partitions.Table (P);

   begin
      Filename := Elaboration_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);

      Write_Line  ("pragma Warnings (Off);");

      --  Remote_Launch is only needed when using the Ada Starter,
      --  we avoid "withing" it otherwise since it drags sockets.

      if P = Main_Partition and then Default_Starter = Ada_Import then
         Write_With_Clause (RU (RU_PolyORB_DSA_P_Remote_Launch));
      end if;

      Write_With_Clause (RU (RU_PolyORB_Setup_IIOP), False, True);

      --  Setup.IIOP must be withed here, because
      --  polyorb-partition_elaboration.ads does not have visibility over IIOP
      --  packages.

      if Current.Tasking = 'N' then
         Write_With_Clause (RU (RU_PolyORB_Setup_Tasking_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_Binding_Data_GIOP_IIOP));

      else
         Write_With_Clause (RU (RU_PolyORB_Setup_Tasking_Full_Tasking));

         if Current.Tasking = 'T' then
            Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
            Write_With_Clause (RU (RU_PolyORB_Binding_Data_GIOP_IIOP));

         else
            Write_With_Clause (RU (RU_PolyORB_ORB_Thread_Pool));
            Write_With_Clause (RU (RU_PolyORB_Setup_Access_Points_IIOP));
         end if;
      end if;

      Write_Str  ("package body ");
      Write_Name (RU (RU_PolyORB_Partition_Elaboration));
      Write_Line (" is");
      Increment_Indentation;
      Write_Indentation;

      --  Launch remote partitions if needed

      Write_Line ("procedure Full_Launch is");
      Write_Indentation;
      Write_Line ("begin");
      Increment_Indentation;

      if P = Main_Partition and then Default_Starter = Ada_Import then
         Generate_Ada_Starter_Code;
      end if;

      --  Write a null statement, so that partitions which have
      --  an empty Full_Launch can still compile.

      Write_Indentation;
      Write_Line  ("null;");

      Decrement_Indentation;
      Write_Indentation;
      Write_Line  ("end Full_Launch;");

      Decrement_Indentation;
      Write_Str  ("end ");
      Write_Name (RU (RU_PolyORB_Partition_Elaboration));
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Elaboration_File;

   ------------------------------
   -- Generate_Executable_File --
   ------------------------------

   procedure Generate_Executable_File (P : Partition_Id) is
      Current    : Partition_Type renames Partitions.Table (P);
      Executable : File_Name_Type;
      Part_Dir   : Directory_Name_Type;
      I_Part_Dir : String_Access;
      Comp_Args  : String_List (1 .. 9);
      Make_Args  : String_List (1 .. 8);
      Sfile      : File_Name_Type;
      Prj_Fname  : File_Name_Type;
      Length     : Natural;

   begin
      Executable := Current.Executable_File;
      Part_Dir   := Current.Partition_Dir;

      Name_Len := 2;
      Name_Buffer (1) := '-';
      Name_Buffer (2) := 'I';
      Get_Name_String_And_Append (Part_Dir);
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
         Comp_Args (6) := new String'(Get_Name_String (Part_Dir));

      else
         Comp_Args (5) := Project_File_Flag;
         Prj_Fname     := Dir (Part_Dir, Part_Prj_File_Name);
         Comp_Args (6) := new String'(Get_Name_String (Prj_Fname));
      end if;

      Length := 6;

      --  We already checked the consistency of all the partition
      --  units. In case of an inconsistency of exception mode, we may
      --  have to rebuild some parts of polyorb (units configured just
      --  for this partition). Note that some parts of PolyORB may have
      --  been already recompiled when the monolithic application was
      --  initially build. Some bodies may be missing as they are
      --  assigned to partitions we do not want to build. So compile
      --  silently and do not exit on errors (keep going).

      if Project_File_Name = null then
         Comp_Args (7) := Compile_Only_Flag;
         Comp_Args (8) := Keep_Going_Flag;
         Comp_Args (9) := Readonly_Flag;
         Length := 9;
      end if;

      --  Compile elaboration file

      Sfile := Elaboration_File & ADB_Suffix_Id;
      if Project_File_Name = null then
         Sfile := Dir (Part_Dir, Sfile);
      end if;
      Compile (Sfile, Comp_Args (1 .. Length));

      --  Compile parameters file

      Sfile := Parameters_File & ADB_Suffix_Id;
      if Project_File_Name = null then
         Sfile := Dir (Part_Dir, Sfile);
      end if;
      Compile (Sfile, Comp_Args (1 .. Length));

      --  Compile main file

      Sfile := Partition_Main_File & ADB_Suffix_Id;
      if Project_File_Name = null then
         Sfile := Dir (Part_Dir, Sfile);
      end if;
      Compile (Sfile, Comp_Args (1 .. Length));

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
         Prj_Fname := Dir (Part_Dir, Part_Prj_File_Name);
         Make_Args (8) := new String'(Get_Name_String (Prj_Fname));
      end if;

      Build (Sfile, Make_Args, Fatal => True);

      Free (Make_Args (2));
      Free (Make_Args (8));
   end Generate_Executable_File;

   ------------------------------
   -- Generate_Parameters_File --
   ------------------------------

   procedure Generate_Parameters_File (P : Partition_Id) is

      Filename     : File_Name_Type;
      File         : File_Descriptor;
      Current      : Partition_Type renames Partitions.Table (P);

   begin
      Filename := Parameters_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);

      --  Add the termination policy to the configuration table, if no
      --  termination policy is set, the default is Global_Termination.

      if Current.Termination /= No_Termination then
         Set_Conf (PE_Termination_Policy,
                   Termination_Img (Current.Termination));
      end if;

      --  Set the Tasking pool parameters

      if Current.Task_Pool /= No_Task_Pool then
         Set_Nat_To_Name_Buffer (Current.Task_Pool (1));
         Set_Conf (PE_Min_Spare_Threads, Name_Find);
         Set_Nat_To_Name_Buffer (Current.Task_Pool (2));
         Set_Conf (PE_Max_Spare_Threads, Name_Find);
         Set_Nat_To_Name_Buffer (Current.Task_Pool (3));
         Set_Conf (PE_Max_Threads, Name_Find);
      end if;

      --  Set the rsh parameters

      Set_Conf (PE_Rsh_Command, Get_Rsh_Command);
      Set_Conf (PE_Rsh_Options, Get_Rsh_Options);

      --  Set the DSA_Local_RCIs section parameters:
      --  For each RCI instantiated on this partition add a
      --  parameter <RCI NAME> set to true.

      declare
         U       : Conf_Unit_Id;
         Section : constant Name_Id := PS (PS_DSA_Local_RCIs);
         T       : constant Name_Id := Id ("true");
      begin
         U := Current.First_Unit;
         while U /= No_Conf_Unit_Id loop
            Set_Conf (Section, Conf_Units.Table (U).Name, T);
            U := Conf_Units.Table (U).Next_Unit;
         end loop;
      end;

      --  The configuration is done, start generating the code

      Write_Line ("pragma Warnings (Off);");
      Write_With_Clause (RU (RU_PolyORB_Initialization), True, True);
      Write_With_Clause (RU (RU_PolyORB_Utils), True);
      Write_With_Clause (RU (RU_PolyORB_Utils_Strings), True);
      Write_With_Clause (RU (RU_PolyORB_Utils_Strings_Lists), True);
      Write_Str  ("package body ");
      Write_Name (RU (RU_PolyORB_Parameters_Partition));
      Write_Line (" is");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("type Partition_Source is new Parameters_Source" &
                    " with null record;");
      Write_Indentation;
      Write_Line ("The_Partition_Source : aliased Partition_Source;");
      Write_Indentation;
      Write_Line ("type Parameter_Entry is record");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("Var : String_Ptr;");
      Write_Indentation;
      Write_Line ("Val : String_Ptr;");
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("end record;");
      Write_Indentation;
      Write_Line ("Table : array (0 .. 31) of Parameter_Entry;");
      Write_Indentation;
      Write_Line ("Last  : Integer := -1;");
      Write_Indentation;
      Write_Line ("function Get_Conf");

      Increment_Indentation;
      Write_Indentation (-1);
      Write_Line ("(Source       : access Partition_Source;");
      Write_Indentation;
      Write_Line ("Section, Key : String) return String");
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("is");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("S : constant String := Make_Global_Key (Section, Key);");
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("begin");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("for I in 0 .. Last loop");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("if Table (I).Var.all = S then");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("return Table (I).Val.all;");
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("end if;");
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("end loop;");
      Write_Indentation;
      Write_Line ("return """";");
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("end Get_Conf;");
      Write_Indentation;
      Write_Line ("procedure Initialize is");
      Write_Indentation;
      Write_Line ("begin");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("Last := -1;");

      for P in Table'First .. Last loop
         Write_Indentation;
         Write_Line ("Last := Last + 1;");
         Write_Indentation;
         Write_Str  ("Table (Last).Var := new String'(""");
         Write_Name (Table (P).Var);
         Write_Line (""");");
         Write_Indentation;
         Write_Str  ("Table (Last).Val := new String'(""");
         Write_Name (Table (P).Val);
         Write_Line (""");");
      end loop;

      Write_Indentation;
      Write_Line ("Register_Source (The_Partition_Source'Access);");
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("end Initialize;");
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("begin");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("Register_Module");

      Increment_Indentation;
      Write_Indentation (-1);
      Write_Line ("(Module_Info'");
      Write_Indentation;
      Write_Line ("(Name      => +""parameters.partition"",");
      Write_Indentation (+1);
      Write_Line ("Conflicts => Empty,");
      Write_Indentation (+1);
      Write_Line ("Depends   => Empty,");
      Write_Indentation (+1);
      Write_Line ("Provides  => +""parameters_sources"",");
      Write_Indentation (+1);
      Write_Line ("Implicit  => True,");
      Write_Indentation (+1);
      Write_Line ("Init      => Initialize'Access,");
      Write_Indentation (+1);
      Write_Line ("Shutdown  => null));");
      Decrement_Indentation;
      Decrement_Indentation;

      Write_Indentation;
      Write_Str  ("end ");
      Write_Name (RU (RU_PolyORB_Parameters_Partition));
      Write_Line (";");

      Close (File);
      Set_Standard_Output;

      --  Reset the configuration table so that next partition elaboration
      --  is configurated properly.

      Reset_Conf;
   end Generate_Parameters_File;

   ----------------------------------
   -- Generate_Partition_Main_File --
   ----------------------------------

   procedure Generate_Partition_Main_File (P : Partition_Id) is
      Filename  : File_Name_Type;
      File      : File_Descriptor;
      Current   : Partition_Type renames Partitions.Table (P);
      Conf_Unit : Conf_Unit_Id;
      Unit      : Unit_Id;
   begin
      Filename := Partition_Main_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);
      Write_Line  ("pragma Warnings (Off);");

      Write_With_Clause (RU (RU_PolyORB_ORB));
      Write_With_Clause (RU (RU_PolyORB_Initialization));
      Write_With_Clause (RU (RU_PolyORB_Setup));
      Write_With_Clause (RU (RU_System_Partition_Interface));

      Write_With_Clause (RU (RU_System_DSA_Services));

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

      Write_Str  ("procedure ");
      Write_Name (Partition_Main_Name);
      Write_Line (" is");
      Write_Line ("begin");
      Increment_Indentation;

      --  Check version consistency of RCI stubs

      if Default_Version_Check then
         for J in Current.First_Stub .. Current.Last_Stub loop
            Unit := ALIs.Table (Get_ALI_Id (Stubs.Table (J))).Last_Unit;
            Write_Call (RU (RE_Unit_Table (RE_Check)) and RE (RE_Check),
                        Quote (Stubs.Table (J)), No_Str,
                        Stubs.Table (J) & "'Version",
                        Units.Table (Unit).RCI'Img);
         end loop;
      end if;

      --  Invoke main subprogram when there is one

      if Present (Current.Main_Subprogram) then
         Write_Call (Current.Main_Subprogram);

      --  XXX We launch ORB.Run although this is only required for a
      --  non-tasking server. Note that this can be considered as
      --  incorrect since the env task becomes indirectly part of the
      --  task pool.

      else
         Write_Call
           (RU (RE_Unit_Table (RE_Run)) and RE (RE_Run),
            RU (RE_Unit_Table (RE_The_ORB)) and RE (RE_The_ORB),
            "May_Poll => True");
      end if;

      Write_Line  ("exception");

      Write_Indentation;
      Write_Line ("when others =>");
      Increment_Indentation;
      Write_Call
         (RU (RE_Unit_Table (RE_Shutdown_World)) and RE (RE_Shutdown_World));
      Write_Indentation;
      Write_Line ("raise;");

      Decrement_Indentation;
      Decrement_Indentation;

      Write_Str  ("end ");
      Write_Name (Partition_Main_Name);
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Partition_Main_File;

   ---------------------
   -- Get_Detach_Flag --
   ---------------------

   function Get_Detach_Flag (Self : access PolyORB_Backend) return Name_Id
   is
      pragma Unreferenced (Self);
   begin
      return Id ("--polyorb-dsa-detach");
   end Get_Detach_Flag;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access PolyORB_Backend) is
      pragma Unreferenced (Self);

      Pos : Integer;
      Len : Natural;

   begin
      XE_Back.Initialize;

      --  RCI unit PolyORB.DSA_P.Partition must be automatically configured on
      --  the main partition.

      PCS_Conf_Unit     := Id ("polyorb.dsa_p.partitions");

      PCS_Project           := Id ("pcs_project");
      Set_Corresponding_Project_File_Name (PCS_Project_File);

      Dist_App_Project      := Id ("dist_app_project");
      Set_Corresponding_Project_File_Name (Dist_App_Project_File);

      Elaboration_File      := Id ("polyorb-partition_elaboration");
      Parameters_File       := Id ("polyorb-parameters-partition");

      Register_Casing_Rule ("ORB");

      if Project_File_Name /= null then
         Generate_Application_Project_Files;
      end if;

      for U in RU_Id'First .. RU_Id'Last loop
         RU (U) := Strip (RU_Id'Image (U));

         --  Allow to get the litteral value back from the name id. As
         --  the default value of info is zero, and as the first pos
         --  of an enumeration type is also zero, increment the pos.

         Set_Name_Table_Info (RU (U), RU_Id'Pos (U) + 1);

         --  Look for parent units

         Pos := 0;
         Get_Name_String (RU (U));
         Len := Name_Len;
         while Name_Len > 0 loop
            if Name_Buffer (Name_Len) = '_' then
               Name_Len := Name_Len - 1;
               Pos := Integer (Get_Name_Table_Info (Name_Find));
               exit when Pos > 0;

            else
               Name_Len := Name_Len - 1;
            end if;
         end loop;

         --  When there is a parent, remove its name from unit name to get
         --  real identifier.

         if Pos > 0 then
            Set_Str_To_Name_Buffer (Name_Buffer (Name_Len + 2 .. Len));
            RU (U) := RU (RU_Id'Val (Pos - 1)) and Name_Find;

         else
            Set_Str_To_Name_Buffer (Name_Buffer (1 .. Len));
            RU (U) := Name_Find;
         end if;

         if Debug_Mode then
            Message (U'Img & " = " & Get_Name_String (RU (U)));
         end if;
      end loop;

      for E in RE_Id loop
         RE (E) := Strip (RE_Id'Image (E));
      end loop;

      for S in PS_Id loop
         PS (S) := Strip (PS_Id'Image (S));
      end loop;

      for E in PE_Id loop
         PE (E) := Strip (PE_Id'Image (E));
      end loop;
   end Initialize;

   ----------------
   -- Reset_Conf --
   ----------------

   procedure Reset_Conf is
   begin
      Last := -1;
   end Reset_Conf;

   -----------------
   -- Run_Backend --
   -----------------

   procedure Run_Backend (Self : access PolyORB_Backend)
   is
      Current : Partition_Type;
      Is_Initiator_Set : Boolean := False;
   begin

      Prepare_Directories;

      Generate_All_Stubs_And_Skels;

      Export_Environment_Var ("POLYORB_DSA_NAME_SERVICE");

      --  For each partition, generate the elaboration, main, executable
      --  and stamp files.

      for J in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (J).To_Build then
            Current := Partitions.Table (J);

            if not Is_Initiator_Set
              and then Current.Tasking /= 'N'
              and then Current.Termination /= Local_Termination
            then
               Set_Str_To_Name_Buffer ("true");
               Set_Conf (PE_Termination_Initiator, Name_Find);
               Is_Initiator_Set := True;

               --  Because configuration is reset after generating each
               --  elaboration file, this conf parameter will only affect
               --  current partition.

            end if;

            if Current.To_Build and then Current.Passive /= BTrue then
               if Rebuild_Partition (J) then
                  if not Quiet_Mode then
                     Message ("building partition", Current.Name);
                  end if;

                  Generate_Parameters_File (J);
                  Generate_Elaboration_File (J);
                  Generate_Partition_Main_File (J);
                  Generate_Executable_File (J);
                  Generate_Stamp_File (J);
               end if;

            elsif Verbose_Mode then
               Message ("no need to expand", Current.Name);
            end if;
         end if;
      end loop;

      Generate_Starter_File (Backend_Access (Self));
   end Run_Backend;

   --------------
   -- Set_Conf --
   --------------

   procedure Set_Conf (Var : PE_Id; Val : Name_Id) is
   begin
      Set_Conf (Section => PS (PE_Section_Table (Var)),
                Key     => PE (Var),
                Val     => Val);
   end Set_Conf;

   procedure Set_Conf (Section : Name_Id; Key : Name_Id; Val : Name_Id) is
   begin
      Last := Last + 1;
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("[");
      Get_Name_String_And_Append (Section);
      Add_Str_To_Name_Buffer ("]");
      Get_Name_String_And_Append (Key);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Table (Last).Var := Name_Find;
      Table (Last).Val := Val;
   end Set_Conf;

   -----------------------------------------
   -- Set_Corresponding_Project_File_Name --
   -----------------------------------------

   procedure Set_Corresponding_Project_File_Name (N : out File_Name_Type) is
   begin
      Add_Str_To_Name_Buffer (".gpr");
      N := Name_Find;
   end Set_Corresponding_Project_File_Name;

   ------------------------
   -- Set_PCS_Dist_Flags --
   ------------------------

   procedure Set_PCS_Dist_Flags (Self : access PolyORB_Backend) is
      pragma Unreferenced (Self);
      Status : aliased Integer;
   begin
      declare
         PolyORB_Config : constant String :=
           Get_Command_Output ("polyorb-config", (1 .. 0 => null), "",
                               Status'Access);
      begin
         Scan_Dist_Args (PolyORB_Config);
      end;
   exception
      when others =>
         Message ("PolyORB installation not found");
         raise Fatal_Error;
   end Set_PCS_Dist_Flags;

   -----------
   -- Strip --
   -----------

   function Strip (S : String) return Unit_Name_Type is
   begin
      Set_Str_To_Name_Buffer (S);
      Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
      Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

      while Name_Buffer (Name_Len) in '0' .. '9'
        or else Name_Buffer (Name_Len) = '_'
      loop
         Name_Len := Name_Len - 1;
      end loop;
      return Name_Find;
   end Strip;

begin
   Register_Backend ("polyorb", new PolyORB_Backend);
end XE_Back.PolyORB;
