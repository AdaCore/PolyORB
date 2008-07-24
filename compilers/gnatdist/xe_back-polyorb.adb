------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      X E _ B A C K . P O L Y O R B                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with XE;          use XE;
with XE_Front;    use XE_Front;
with XE_Flags;    use XE_Flags;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Utils;    use XE_Utils;
with XE_Storages; use XE_Storages;

with XE_Back;
pragma Elaborate_All (XE_Back);

package body XE_Back.PolyORB is

   type PolyORB_Backend is new Backend with null record;

   procedure Set_PCS_Dist_Flags (Self : access PolyORB_Backend);

   procedure Initialize (Self : access PolyORB_Backend);
   procedure Register_Storages (Self : access PolyORB_Backend);
   procedure Run_Backend (Self : access PolyORB_Backend);
   function Get_Detach_Flag (Self : access PolyORB_Backend) return Name_Id;

   Elaboration_File : File_Name_Type;
   --  Partition elaboration unit

   Storage_Config_File : File_Name_Type;
   --  Shared storage configuration unit

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
      RU_PolyORB_DSA_P,
      RU_PolyORB_DSA_P_Remote_Launch,
      RU_PolyORB_DSA_P_Storages,
      RU_PolyORB_DSA_P_Storages_Config,
      RU_PolyORB_Parameters,
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
      RU_PolyORB_Tasking,
      RU_PolyORB_Tasking_Threads,
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
      RE_Run_In_Task,
      RE_Shutdown_World,
      RE_The_ORB);

   RE : array (RE_Id) of Unit_Name_Type;

   RE_Unit_Table : constant array (RE_Id) of RU_Id :=
     (RE_Check            => RU_System_Partition_Interface,
      RE_Launch_Partition => RU_PolyORB_DSA_P_Remote_Launch,
      RE_Run              => RU_PolyORB_ORB,
      RE_Run_In_Task      => RU_PolyORB_Tasking_Threads,
      RE_Shutdown_World   => RU_PolyORB_Initialization,
      RE_The_ORB          => RU_PolyORB_Setup);

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
   --  Compile main partition file, storages configuration file and
   --  elaboration file. Bind and link partition to create executable.

   procedure Generate_Parameters_Source (P : Partition_Id);
   --  Create fragment of elaboration file that declares and registers a
   --  runtime parameters source for the given partition. Must be called last
   --  thing in Generate_Elaboration_File (creates elaboration statements for
   --  the package).

   procedure Generate_Partition_Main_File (P : Partition_Id);
   --  Create a procedure which "withes" all the RCI or SP receivers
   --  of the partition and insert the main procedure if needed.

   procedure Generate_Storage_Config_File (P : Partition_Id);
   --  Create storage configuration file that includes the storages
   --  required in the configuration file for this partition.

   procedure Generate_PCS_Project_Files;
   --  Generate project files to access the PCS

   function Strip (S : String) return Unit_Name_Type;
   --  Return the prefix and a possible suffix from S

   procedure Set_Conf (Var : PE_Id; Val : Name_Id);
   --  Add a new entry in the configuration table

   procedure Set_Conf (Section : Name_Id; Key : Name_Id; Val : Name_Id);
   --  Add a new entry in the configuration table

   procedure Reset_Conf;
   --  Clear the configuration table

   --  Installation information

   DSA_Inc_Rel_Dir : constant String :=
                       "include" & Dir_Separator & "polyorb";
   --  PolyORB include directory, relative to the installation prefix

   PolyORB_Prefix  : constant String :=
                       XE_Back.Prefix
                         (Check_For => DSA_Inc_Rel_Dir
                                         & Dir_Separator & "polyorb.ads");

   -------------------------------
   -- Generate_Ada_Starter_Code --
   -------------------------------

   procedure Generate_Ada_Starter_Code
   is
      Remote_Host : Name_Id;
   begin
      for J in Partitions.First + 1 .. Partitions.Last loop
         if J /= Main_Partition
           and then Partitions.Table (J).Passive /= BTrue
         then
            declare
               Partition : Partition_Type renames Partitions.Table (J);
               Full_Cmd  : constant String := Get_Name_String
                             (Quote (To_Absolute_File
                                       (Partition.Executable_File)
                                     & Partition.Command_Line));
               Env : constant String := Get_Env_Vars (J, Names_Only => True);
            begin
               Write_Image (Remote_Host, Partition.Host, J);
               if not Present (Remote_Host) then
                  Remote_Host := Id ("""localhost""");
               end if;

               Write_Call (RU (RE_Unit_Table (RE_Launch_Partition))
                           and RE (RE_Launch_Partition),
                           Remote_Host,
                           Full_Cmd,
                           Quote (Id (Env)));
            end;
         end if;
      end loop;
   end Generate_Ada_Starter_Code;

   -------------------------------
   -- Generate_Elaboration_File --
   -------------------------------

   procedure Generate_Elaboration_File (P : Partition_Id) is

      Filename : File_Name_Type;
      File     : File_Descriptor;
      Current  : Partition_Type renames Partitions.Table (P);

   begin
      Filename := Elaboration_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);

      Write_Line  ("pragma Warnings (Off);");

      --  First drag platform the specific base setup

      Write_With_Clause (RU (RU_PolyORB_Setup_Base), False, True);

      --  Remote_Launch is only needed when using the Ada Starter,
      --  we avoid "withing" it otherwise since it drags sockets.

      if P = Main_Partition and then Default_Starter = Ada_Import then
         Write_With_Clause (RU (RU_PolyORB_DSA_P_Remote_Launch));
      end if;

      Write_With_Clause (RU (RU_PolyORB_Setup_IIOP), False, True);

      if Current.Tasking = No_Tasking then
         Write_With_Clause (RU (RU_PolyORB_Setup_Tasking_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_Binding_Data_GIOP_IIOP));

      else
         Write_With_Clause (RU (RU_PolyORB_Setup_Tasking_Full_Tasking));

         if Current.Tasking = User_Tasking then
            Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
            Write_With_Clause (RU (RU_PolyORB_Binding_Data_GIOP_IIOP));

         else
            Write_With_Clause
              (RU (RU_PolyORB_ORB) and ORB_Tasking_Policy_Img
               (Current.ORB_Tasking_Policy));
            Write_With_Clause (RU (RU_PolyORB_Setup_Access_Points_IIOP));

         end if;
      end if;

      --  Dependencies related to the partition specific parameters source

      Write_With_Clause (RU (RU_PolyORB_Parameters), U => True);
      Write_With_Clause (RU (RU_PolyORB_Initialization), True, True);
      Write_With_Clause (RU (RU_PolyORB_Utils), True);
      Write_With_Clause (RU (RU_PolyORB_Utils_Strings), True);
      Write_With_Clause (RU (RU_PolyORB_Utils_Strings_Lists), True);
      Write_With_Clause (RU (RU_PolyORB_Utils_Strings_Lists), True);
      Write_With_Clause (RU (RU_PolyORB_Tasking_Threads), True);

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

      --  Run additional tasks if needed.
      --  Only Thread_Per_Request and Thread_Per_Session policies
      --  need an additional ORB task, as there is no task dedicated
      --  to process incoming requests.

      if Current.Tasking = PCS_Tasking
        and then Current.ORB_Tasking_Policy /= Thread_Pool
      then
         --  Generate a wrapper procedure that allow to give a
         --  parameterless procedure access to Run_In_task call.

         Write_Indentation;
         Write_Line ("procedure Run_In_Task_Wrapper is");
         Write_Indentation;
         Write_Line ("begin");
         Increment_Indentation;

         Write_Call
           (RU (RE_Unit_Table (RE_Run)) and RE (RE_Run),
            RU (RE_Unit_Table (RE_The_ORB)) and RE (RE_The_ORB),
            "May_Poll => True");

         Decrement_Indentation;
         Write_Indentation;
         Write_Line  ("end Run_In_Task_Wrapper;");
      end if;

      Write_Indentation;
      Write_Line ("procedure Run_Additional_Tasks is");

      if Current.Tasking = PCS_Tasking
        and then Current.ORB_Tasking_Policy /= Thread_Pool
      then
         Increment_Indentation;
         Write_Indentation;
         Write_Line ("Thread_Acc : Thread_Access;");
         Decrement_Indentation;
         Write_Indentation;
         Write_Line ("begin");
         Increment_Indentation;
         Write_Indentation;
         Write_Str ("Thread_Acc := ");
         Write_Call (RU (RE_Unit_Table (RE_Run_In_Task))
                     and RE (RE_Run_In_Task),
                     S1 => "TF => Get_Thread_Factory",
                     S2 => "P => Run_In_Task_Wrapper'Access");
      else
         --  Write a null statement, so that partitions which have
         --  an empty Run_Additional_Tasks can still compile.

         Write_Indentation;
         Write_Line ("begin");
         Increment_Indentation;
         Write_Indentation;
         Write_Line  ("null;");
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write_Line  ("end Run_Additional_Tasks;");

      Generate_Parameters_Source (P);

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
      Comp_Args (3) := A_Stub_Dir;
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

      Sfile := Dir (Part_Dir, Elaboration_File & ADB_Suffix_Id);
      Compile (Sfile, Comp_Args (1 .. Length));

      --  Compile storage support configuration file

      Sfile := Dir (Part_Dir, Storage_Config_File & ADB_Suffix_Id);
      Compile (Sfile, Comp_Args (1 .. Length));

      --  Compile main file

      Sfile := Dir (Part_Dir, Partition_Main_File & ADB_Suffix_Id);
      Compile (Sfile, Comp_Args (1 .. Length));

      Free (Comp_Args (6));

      --  Now we just want to bind and link as the ALI files are now
      --  consistent.

      Make_Args (1) := E_Current_Dir;
      Make_Args (2) := I_Part_Dir;
      Make_Args (3) := A_Stub_Dir;
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

   --------------------------------
   -- Generate_Parameters_Source --
   --------------------------------

   procedure Generate_Parameters_Source (P : Partition_Id) is
      Current      : Partition_Type renames Partitions.Table (P);
   begin

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

      Write_Indentation;
      Write_Line ("type Partition_Source is new Parameters_Source" &
                    " with null record;");

      Write_Indentation;
      Write_Line ("function Get_Conf");
      Increment_Indentation;
      Write_Indentation (-1);
      Write_Line ("(Source       : access Partition_Source;");
      Write_Indentation;
      Write_Line ("Section, Key : String) return String;");
      Decrement_Indentation;

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
      Write_Line ("The_Partition_Source : aliased Partition_Source;");

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

      --  Reset the configuration table so that next partition elaboration
      --  is configurated properly.

      Reset_Conf;
   end Generate_Parameters_Source;

   ----------------------------------
   -- Generate_Storage_Config_File --
   ----------------------------------

   procedure Generate_Storage_Config_File (P : Partition_Id) is
      Filename         : File_Name_Type;
      File             : File_Descriptor := Invalid_FD;
      Current          : Partition_Type renames Partitions.Table (P);
      Major            : Name_Id;
      Required_Storage : Required_Storage_Id;
      Location         : Location_Id;

   begin
      Filename := Storage_Config_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);
      Write_Line  ("pragma Warnings (Off);");

      --  Import the storage supports used by this partition

      Required_Storage := Partitions.Table (P).First_Required_Storage;
      while Required_Storage /= No_Required_Storage_Id loop
         Location := Required_Storages.Table (Required_Storage).Location;
         Major    := Capitalize (Locations.Table (Location).Major);
         Major    := RU (RU_PolyORB_DSA_P_Storages) and Major;
         Write_With_Clause (Major, False, True);

         Required_Storage := Required_Storages.Table
           (Required_Storage).Next_Storage;
      end loop;

      --  Initialize storage supports

      Write_Str  ("package body ");
      Write_Name (RU (RU_PolyORB_DSA_P_Storages_Config));
      Write_Line (" is");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("procedure Initialize_Storages is");
      Write_Indentation;
      Write_Line ("begin");

      Increment_Indentation;

      --  Follow the same approach as for package importation

      Required_Storage := Partitions.Table (P).First_Required_Storage;
      while Required_Storage /= No_Required_Storage_Id loop
         Location := Required_Storages.Table (Required_Storage).Location;
         Major    := Capitalize (Locations.Table (Location).Major);
         Write_Call
           (RU (RU_PolyORB_DSA_P_Storages)
            and Capitalize (Major)
            and "Register_Passive_Package",
            Quote (Name (Units.Table
              (Required_Storages.Table (Required_Storage).Unit).Uname)),
            Boolean'Image
              (Required_Storages.Table (Required_Storage).Is_Owner));

         Required_Storage := Required_Storages.Table
           (Required_Storage).Next_Storage;
      end loop;

      --  Write a null statement, so that partitions which have
      --  an empty Initialize_Storages can still compile.

      Write_Indentation;
      Write_Line  ("null;");

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Initialize_Storages;");
      Decrement_Indentation;

      Write_Eol;
      Write_Str  ("end ");
      Write_Name (RU (RU_PolyORB_DSA_P_Storages_Config));
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Storage_Config_File;

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

      --  ??? We launch ORB.Run although this is only required for a
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

   --------------------------------
   -- Generate_PCS_Project_Files --
   --------------------------------

   procedure Generate_PCS_Project_Files is
      Prj_Fname  : File_Name_Type;
      Prj_File   : File_Descriptor;

      DSA_Inc_Dir : constant String := PolyORB_Prefix & DSA_Inc_Rel_Dir;

      Secondary_PCS_Project      : Name_Id;
      Secondary_PCS_Project_File : File_Name_Type;
   begin
      --  In the two project files below, we use ".." as the object directory,
      --  relative to the project directory, so that all objects are stored in
      --  the user's build directory.

      --  Create intermediate PCS project, extending the main PolyORB project,
      --  but removing source files that need to be rebuilt as client or server
      --  stubs, and those that are overridden by each partition.

      Get_Name_String (PCS_Project);
      Add_Char_To_Name_Buffer ('1');
      Secondary_PCS_Project := Name_Find;
      Set_Corresponding_Project_File_Name (Secondary_PCS_Project_File);

      Prj_Fname := Dir (Id (Root), Secondary_PCS_Project_File);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);
      Write_Str  ("project ");
      Write_Name (Secondary_PCS_Project);

      Write_Str  (" extends all ""polyorb""");
      Write_Line (" is");
      Write_Line ("   for Externally_Built use ""true"";");
      Write_Line ("   for Source_Dirs use (""" & DSA_Inc_Dir & """);");
      Write_Line ("   for Locally_Removed_Files use");

      --  Overridden

      Write_Line ("     (""polyorb-partition_elaboration.adb"",");
      Write_Line ("      ""polyorb-dsa_p-storages-config.adb"",");

      --  Rebuilt as stubs

      Write_Line ("      ""polyorb-dsa_p-partitions.ads"",");
      Write_Line ("      ""polyorb-dsa_p-partitions.adb"");");

      Write_Str  ("end ");
      Write_Name (Secondary_PCS_Project);
      Write_Line (";");
      Close (Prj_File);
      Set_Standard_Output;

      --  Create project for PCS units that need to be rebuilt per-partition

      Prj_Fname := Dir (Id (Root), PCS_Project_File);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);
      Write_Str  ("with """);
      Write_Name (Secondary_PCS_Project);
      Write_Line (""";");
      Write_Str  ("project ");
      Write_Name (PCS_Project);
      Write_Line (" is");
      Write_Line ("   for Object_Dir use "".."";");
      Write_Line ("   for Source_Dirs use (""" & DSA_Inc_Dir & """);");
      Write_Line ("   for Source_Files use");
      Write_Line ("     (""polyorb-dsa_p-partitions.ads"",");
      Write_Line ("      ""polyorb-dsa_p-partitions.adb"");");
      Write_Str  ("end ");
      Write_Name (PCS_Project);
      Write_Line (";");
      Close (Prj_File);
      Set_Standard_Output;
   end Generate_PCS_Project_Files;

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

      PCS_Conf_Unit       := Id ("polyorb.dsa_p.partitions");
      Elaboration_File    := Id ("polyorb-partition_elaboration");
      Storage_Config_File := Id ("polyorb-dsa_p-storages-config");

      Register_Casing_Rule ("ORB");

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

      --  Pass name server IOR from starter to all slave partitions

      Add_Environment_Variable
        (Partitions.Table (Default_Partition_Id).First_Env_Var,
         Partitions.Table (Default_Partition_Id).Last_Env_Var,
         Id ("POLYORB_DSA_NAME_SERVICE"));

      Generate_PCS_Project_Files;
      Generate_Application_Project_Files;
   end Initialize;

   ----------------
   -- Reset_Conf --
   ----------------

   procedure Reset_Conf is
   begin
      Last := -1;
   end Reset_Conf;

   -----------------------
   -- Register_Storages --
   -----------------------

   procedure Register_Storages (Self : access PolyORB_Backend)
   is
      pragma Unreferenced (Self);
   begin
      Register_Storage
        (Storage_Name     => "dsm",
         Allow_Passive    => False,
         Allow_Local_Term => False,
         Need_Tasking     => True);
      --  Registrer "dsm" storage support

   end Register_Storages;

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

      --  For each partition, generate the elaboration, main, executable
      --  and stamp files.

      for J in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (J).To_Build then
            Current := Partitions.Table (J);

            if not Is_Initiator_Set
              and then Current.Tasking /= No_Tasking
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

                  Generate_Storage_Config_File (J);
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

   ------------------------
   -- Set_PCS_Dist_Flags --
   ------------------------

   procedure Set_PCS_Dist_Flags (Self : access PolyORB_Backend) is
      pragma Unreferenced (Self);
   begin
      --  WAG:61
      --  We would normally get linker switches for the PolyORB runtime library
      --  through project files. This is the only supported option in MinGW
      --  context, where we cannot use the polyorb-config shell script.
      --  In the UNIX case, we still use polyorb-config, so that we get not
      --  only the project file path but also the legacy -L/-l command line
      --  switches, which allow correct operation even with older compilers.
      --  Note that in the UNIX case we rely on polyorb-config to set both
      --  -aP and -aI, to avoid setting -aP here to a value that might be
      --  inconsistent with the -aI path set by polyorb-config.

      if XE_Flags.Use_PolyORB_Project then
         Scan_Dist_Arg ("-margs");
         Scan_Dist_Arg ("-aP" & PolyORB_Prefix
                              & "lib" & Dir_Separator & "gnat");

      else
         begin
            declare
               Status : aliased Integer;
               PolyORB_Config_Command : constant String :=
                                          PolyORB_Prefix
                                            & "bin" & Dir_Separator
                                            & "polyorb-config";
               PolyORB_Config_Output : constant String :=
                 Get_Command_Output (PolyORB_Config_Command,
                                     (1 .. 0 => null), "", Status'Access);
            begin
               Scan_Dist_Args (PolyORB_Config_Output);
            end;
         exception
            when others =>
               Message
                 ("PolyORB installation is invalid (polyorb-config failure)");
               raise Fatal_Error;
         end;
      end if;
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
