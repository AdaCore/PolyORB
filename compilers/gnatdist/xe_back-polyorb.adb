------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      X E _ B A C K . P O L Y O R B                       --
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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Table;

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

   overriding procedure Set_PCS_Dist_Flags (Self : access PolyORB_Backend);

   overriding procedure Initialize (Self : access PolyORB_Backend);
   overriding procedure Register_Storages (Self : access PolyORB_Backend);
   overriding procedure Run_Backend (Self : access PolyORB_Backend);
   overriding function Get_Detach_Flag
     (Self : access PolyORB_Backend) return Name_Id;

   Elaboration_File : File_Name_Type;
   --  Partition elaboration unit

   Storage_Config_File : File_Name_Type;
   --  Shared storage configuration unit

   type RU_Id is
     (RU_PolyORB,
      RU_PolyORB_Binding_Data,
      RU_PolyORB_Binding_Data_GIOP,
      RU_PolyORB_Binding_Data_GIOP_IIOP,
      RU_PolyORB_Binding_Data_DNS,
      RU_PolyORB_Binding_Data_DNS_MDNS,
      RU_PolyORB_Protocols,
      RU_PolyORB_Protocols_DNS,
      RU_PolyORB_Initialization,
      RU_PolyORB_ORB_Controller,
      RU_PolyORB_ORB_Controller_Workers,
      RU_PolyORB_ORB,
      RU_PolyORB_ORB_No_Tasking,
      RU_PolyORB_DSA_P,
      RU_PolyORB_DSA_P_Name_Server,
      RU_PolyORB_DSA_P_Name_Service,
      RU_PolyORB_DSA_P_Name_Service_COS_Naming,
      RU_PolyORB_DSA_P_Name_Service_MDNS,
      RU_PolyORB_DSA_P_Name_Service_None,
      RU_PolyORB_DSA_P_Remote_Launch,
      RU_PolyORB_DSA_P_Storages,
      RU_PolyORB_DSA_P_Storages_Config,
      RU_PolyORB_Parameters,
      RU_PolyORB_Partition_Elaboration,
      RU_PolyORB_Setup,
      RU_PolyORB_Setup_Access_Points,
      RU_PolyORB_Setup_Access_Points_IIOP,
      RU_PolyORB_Setup_Access_Points_mDNS,
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
     (No_RE_Id,
      RE_Check,
      RE_Launch_Partition,
      RE_Run,
      RE_Run_In_Task,
      RE_Shutdown_World,
      RE_The_ORB,
      RE_Activate_RPC_Receivers,
      RE_Name_Ctx,
      RE_COS_Name_Server,
      RE_MDNS_Name_Server,
      RE_None_Name_Server);

   subtype Valid_RE_Id is RE_Id range RE_Id'Succ (No_RE_Id) .. RE_Id'Last;

   RE : array (Valid_RE_Id) of Unit_Name_Type;

   RE_Unit_Table : constant array (Valid_RE_Id) of RU_Id :=
     (RE_Check                  => RU_System_Partition_Interface,
      RE_Launch_Partition       => RU_PolyORB_DSA_P_Remote_Launch,
      RE_Run                    => RU_PolyORB_ORB,
      RE_Run_In_Task            => RU_PolyORB_Tasking_Threads,
      RE_Shutdown_World         => RU_PolyORB_Initialization,
      RE_The_ORB                => RU_PolyORB_Setup,
      RE_Activate_RPC_Receivers => RU_System_Partition_Interface,
      RE_Name_Ctx               => RU_PolyORB_DSA_P_Name_Service,
      RE_COS_Name_Server        => RU_PolyORB_DSA_P_Name_Service_COS_Naming,
      RE_MDNS_Name_Server       => RU_PolyORB_DSA_P_Name_Service_MDNS,
      RE_None_Name_Server       => RU_PolyORB_DSA_P_Name_Service_None);

   ---------------------
   -- Parameter types --
   ---------------------

   type PS_Id is (PS_Tasking, PS_DSA);

   PS : array (PS_Id) of Unit_Name_Type;

   type PE_Id is
     (PE_Start_Threads,
      PE_Max_Spare_Threads,
      PE_Max_Threads,
      PE_Min_Spare_Threads,
      PE_Rsh_Command,
      PE_Rsh_Options,
      PE_Boot_Location,
      PE_Self_Location,
      PE_Tasking_Available,
      PE_Termination_Initiator,
      PE_Termination_Policy,
      PE_Partition_Name,
      PE_Main_Partition,
      PE_Name_Server_Kind);

   PE : array (PE_Id) of Unit_Name_Type;

   PE_Section_Table : constant array (PE_Id) of PS_Id :=
     (PE_Rsh_Command           => PS_DSA,
      PE_Rsh_Options           => PS_DSA,
      PE_Boot_Location         => PS_DSA,
      PE_Self_Location         => PS_DSA,
      PE_Tasking_Available     => PS_DSA,
      PE_Termination_Initiator => PS_DSA,
      PE_Termination_Policy    => PS_DSA,
      PE_Partition_Name        => PS_DSA,
      PE_Main_Partition        => PS_DSA,
      PE_Name_Server_Kind      => PS_DSA,
      PE_Start_Threads         => PS_Tasking,
      PE_Max_Spare_Threads     => PS_Tasking,
      PE_Max_Threads           => PS_Tasking,
      PE_Min_Spare_Threads     => PS_Tasking);

   ------------------------------------
   -- Configuration parameters table --
   ------------------------------------

   type Parameter_Entry is record
      Section : Name_Id;
      Key     : Name_Id;
      Value   : Name_Id;
   end record;

   package Conf is new GNAT.Table (
     Table_Component_Type => Parameter_Entry,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 0,
     Table_Initial        => 16,
     Table_Increment      => 16);

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
   --  Create the main unit for a partition: include dependencies for all
   --  RCI and SP units assigned on the partition, and generate a call to
   --  Activate_RPC_Receivers (after partition elaboration is completed) and to
   --  the user-specified main procedure, if applicable.

   procedure Generate_Storage_Config_File (P : Partition_Id);
   --  Create storage configuration file that includes the storages
   --  required in the configuration file for this partition.

   procedure Generate_PCS_Project_Files;
   --  Generate project files to access the PCS

   function Strip
     (S        : String;
      To_Lower : Boolean := False) return Unit_Name_Type;
   --  Return the prefix and a possible suffix from S. If To_Lower is set,
   --  convert to lowercase, else apply general casing rules.

   procedure Set_Conf (Var : PE_Id; Val : Name_Id; Quote : Boolean := True);
   procedure Set_Conf (Var : PE_Id; Val : Boolean; Quote : Boolean := True);
   --  Add a new entry in the configuration table

   function Name_Service_Type (NS_Type : Name_Server_Type) return RE_Id;
   --  Return the name of the client type for the desired name service

   procedure Set_Conf
     (Section : Name_Id;
      Key     : Name_Id;
      Val     : Name_Id;
      Quote   : Boolean);
   --  Add a new entry in the configuration table

   procedure Reset_Conf;
   --  Clear the configuration table

   --  Installation information

   PolyORB_Prefix  : constant String :=
                       XE_Back.Prefix
                         (Check_For => "lib" & Dir_Separator & "gnat"
                                        & Dir_Separator & "polyorb.gpr");

   -------------------------------
   -- Generate_Ada_Starter_Code --
   -------------------------------

   procedure Generate_Ada_Starter_Code is
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

      Write_Warnings_Pragmas;
      Write_Line  ("pragma Ada_2012;");

      --  First drag platform the specific base setup

      Write_With_Clause (RU (RU_PolyORB_Setup_Base), False, True);

      --  Remote_Launch is only needed when using the Ada Starter,
      --  we avoid "withing" it otherwise since it drags sockets.

      if P = Main_Partition then
         if Default_Starter = Ada_Import then
            Write_With_Clause (RU (RU_PolyORB_DSA_P_Remote_Launch));
         end if;

         if Default_Name_Server = Embedded_NS then
            Write_With_Clause (RU (RU_PolyORB_DSA_P_Name_Server));
         end if;
      end if;

      Write_With_Clause (RU (RU_PolyORB_Setup_IIOP), False, True);

      if Current.Tasking = No_Tasking then
         Write_With_Clause (RU (RU_PolyORB_Setup_Tasking_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_ORB_No_Tasking));
         Write_With_Clause (RU (RU_PolyORB_Binding_Data_GIOP_IIOP));

      else
         Write_With_Clause (RU (RU_PolyORB_Setup_Tasking_Full_Tasking));

         if Current.Tasking = User_Tasking then
            Write_With_Clause
              (RU (RU_PolyORB_ORB) and
                 ORB_Tasking_Policy_Img (Thread_Pool));
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

      if Default_Name_Server /= No_Name_Server then
         Write_With_Clause
           (RU (RE_Unit_Table (Name_Service_Type (Default_Name_Server))));
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
            S1 => "May_Exit => False");

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
      Current       : Partition_Type renames Partitions.Table (P);
      Executable    : File_Name_Type renames Current.Executable_File;
      Partition_Dir : Directory_Name_Type renames Current.Partition_Dir;
      Comp_Args,
      Make_Args     : Argument_Vec;
      Sfile         : File_Name_Type;
      I_Part_Dir    : constant Unbounded_String :=
                        +("-I" & Get_Name_String (Partition_Dir));
   begin
      Push (Comp_Args, Project_File_Flag);
      Push (Comp_Args, Dir (Partition_Dir, Part_Prj_File_Name));

      --  Compile elaboration file

      Sfile := Dir (Partition_Dir, Elaboration_File & ADB_Suffix_Id);
      Compile (Sfile, Comp_Args);

      --  Compile storage support configuration file

      Sfile := Dir (Partition_Dir, Storage_Config_File & ADB_Suffix_Id);
      Compile (Sfile, Comp_Args);

      --  Compile main file

      Sfile := Dir (Partition_Dir, Part_Main_Body_Name);
      Compile (Sfile, Comp_Args);

      Push (Make_Args, Project_File_Flag);
      Push (Make_Args, Dir (Partition_Dir, Part_Prj_File_Name));

      --  Now we just want to bind and link as the ALI files are now consistent

      Push (Make_Args, Bind_Only_Flag);
      Push (Make_Args, Link_Only_Flag);
      Push (Make_Args, Output_Flag);
      Push (Make_Args, Strip_Directory (Executable));

      Push (Make_Args, "-bargs");
      Push (Make_Args, No_Main_Proc_Flag);
      Push (Make_Args, E_Current_Dir);
      Push (Make_Args, I_Part_Dir);
      Push (Make_Args, A_Stub_Dir);
      Push (Make_Args, I_Current_Dir);

      Push (Make_Args, "-cargs");
      Push (Make_Args, E_Current_Dir);
      Push (Make_Args, I_Part_Dir);
      Push (Make_Args, I_Current_Dir);

      --  While binding and linking partitions, the original (monolithic)
      --  objects and ALIs must be moved out of the builder's visibility,
      --  so that they can be replaced by the stubbed versions supplied
      --  through directories specified on the command line.

      --  Note: we pass the base name Part_Main_Body_Name, not the full path.
      --  Gprbuild will locate it through the partition project file, and
      --  this avoids inconsistencies between the full path name as
      --  computed by gnatdist and that generated by the project circuitry.

      Hide_Stubbed_Units;
      Build (Part_Main_Body_Name, Make_Args, Fatal => True);
      Unhide_Stubbed_Units;
   end Generate_Executable_File;

   --------------------------------
   -- Generate_Parameters_Source --
   --------------------------------

   procedure Generate_Parameters_Source (P : Partition_Id) is
      Current : Partition_Type renames Partitions.Table (P);

      Section : constant Name_Id := PS (PS_DSA);

      type DSA_Attribute is (Partition, Reconnection, Location);
      subtype RCI_Attribute is DSA_Attribute range Partition .. Reconnection;
      subtype Partition_Attribute is DSA_Attribute range Location .. Location;

      function Attribute_Name
        (U : Conf_Unit_Id;
         A : RCI_Attribute) return Name_Id;
      --  Return U'A

      function Attribute_Name
        (P : Partition_Id;
         A : Partition_Attribute) return Name_Id;
      --  Return P'A

      function Attribute_Name
        (N : Name_Id;
         A : DSA_Attribute) return Name_Id;
      --  Return N'A

      --------------------
      -- Attribute_Name --
      --------------------

      function Attribute_Name
        (U : Conf_Unit_Id;
         A : RCI_Attribute) return Name_Id
      is
      begin
         return Attribute_Name (Conf_Units.Table (U).Name, A);
      end Attribute_Name;

      function Attribute_Name
        (P : Partition_Id;
         A : Partition_Attribute) return Name_Id
      is
      begin
         return Attribute_Name (Partitions.Table (P).Name, A);
      end Attribute_Name;

      function Attribute_Name
        (N : Name_Id;
         A : DSA_Attribute) return Name_Id
      is
      begin
         Get_Name_String (N);
         Add_Char_To_Name_Buffer (''');
         Add_Str_To_Name_Buffer (Ada.Characters.Handling.To_Lower (A'Img));
         return Name_Find;
      end Attribute_Name;

   --  Start of processing for Generate_Parameters_Source

   begin
      --  Set partition name

      Set_Conf (PE_Partition_Name, Current.Name);
      Set_Conf (PE_Main_Partition, P = Main_Partition);

      --  Set tasking mode

      Set_Conf (PE_Tasking_Available, Current.Tasking /= No_Tasking);

      --  Add the termination policy to the configuration table, if no
      --  termination policy is set, the default is Global_Termination.

      if Current.Termination /= No_Termination then
         Set_Conf (PE_Termination_Policy,
                   Termination_Img (Current.Termination));
      end if;

      --  Set boot location

      if Default_First_Boot_Location /= No_Location_Id then
         Set_Conf (PE_Boot_Location,
                   Location_List_Image (Default_First_Boot_Location),
                   Quote => False);
      end if;

      --  Set self location

      if Current.First_Network_Loc /= No_Location_Id then
         Set_Conf (PE_Self_Location,
                     Location_List_Image (Current.First_Network_Loc),
                   Quote => False);
      end if;

      --  Set name server kind

      if Default_Name_Server /= No_Name_Server then
         Set_Conf (PE_Name_Server_Kind, Name_Server_Img (Default_Name_Server));
      end if;

      --  Set task pool parameters

      if Current.Task_Pool /= No_Task_Pool then
         Set_Nat_To_Name_Buffer (Current.Task_Pool (1));
         declare
            N : constant Name_Id := Name_Find;
            --  Min_Spare_Threads, also used for Start_Threads
         begin
            Set_Conf (PE_Start_Threads, N);
            Set_Conf (PE_Min_Spare_Threads, N);
         end;

         Set_Nat_To_Name_Buffer (Current.Task_Pool (2));
         Set_Conf (PE_Max_Spare_Threads, Name_Find);

         Set_Nat_To_Name_Buffer (Current.Task_Pool (3));
         Set_Conf (PE_Max_Threads, Name_Find);
      end if;

      --  Set the rsh parameters

      Set_Conf (PE_Rsh_Command, Get_Rsh_Command);
      Set_Conf (PE_Rsh_Options, Get_Rsh_Options);

      --  Set configuration parameters describing remote partitions:
      --    location of remote partitions
      --    partition and reconnection policy of each RCI

      --  Note that we also set the partition and reconnection policy for
      --  local RCIs so that we can abort partition elaboration when a stale
      --  reference is present in the name server and the partition's policy
      --  is Reject_On_Restart. Further note, actually we set the reconnection
      --  parameter for all conf units (both SP and RCI), because at this point
      --  the ALI for some RCIs might be unavailable (if we are not building
      --  all partitions).

      for Rem_P in Partitions.First .. Partitions.Last loop
         declare
            Remote  : Partition_Type renames Partitions.Table (Rem_P);
            U       : Conf_Unit_Id;
            Key     : Name_Id;
            Loc     : Location_Id := No_Location_Id;
         begin
            --  Set partition location

            --  Partition location set explicitly

            if Remote.First_Network_Loc /= No_Location_Id then
               Loc := Remote.First_Network_Loc;

            --  Partition is main, and name server is embedded: partition
            --  location is boot location.

            elsif Rem_P = Main_Partition
                    and then Default_Name_Server = Embedded_NS
            then
               Loc := Default_First_Boot_Location;
            end if;

            if Loc /= No_Location_Id then
               Set_Conf (Section,
                         Attribute_Name (Rem_P, Location),
                         Location_List_Image (Loc),
                         Quote => False);
            end if;

            U := Remote.First_Unit;
            while U /= No_Conf_Unit_Id loop

               --  Set RCI partition assignment

               Key := Attribute_Name (U, Partition);
               Set_Conf
                 (Section, Key,
                  Remote.Name,
                  Quote => True);

               --  Set RCI reconnection policy, if non-default
               --  Should be made an attribute of the partition???

               if Remote.Reconnection /= No_Reconnection then
                  Key := Attribute_Name (U, Reconnection);
                  Set_Conf
                    (Section, Key,
                     Reconnection_Img (Remote.Reconnection),
                     Quote => True);
               end if;

               U := Conf_Units.Table (U).Next_Unit;
            end loop;
         end;
      end loop;

      --  The configuration is done, start generating the code

      Write_Indentation;
      Write_Line ("procedure Configure");
      Increment_Indentation;
      Write_Indentation (-1);
      Write_Line
        ("(Set_Conf : access procedure (Section, Key, Value : String))");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("is");

      Write_Indentation;
      Write_Line ("begin");

      Increment_Indentation;
      for P in Conf.First .. Conf.Last loop
         Write_Indentation;
         Write_Line ("Set_Conf");
         Increment_Indentation;

         Write_Indentation (-1);
         Write_Str ("(Section => """);
         Write_Name (Conf.Table (P).Section);
         Write_Line (""",");

         Write_Indentation;
         Write_Str ("Key     => """);
         Write_Name (Conf.Table (P).Key);
         Write_Line (""",");

         Write_Indentation;
         Write_Str ("Value   => ");
         Write_Name (Conf.Table (P).Value);
         Write_Line (");");
         Decrement_Indentation;
      end loop;

      --  Configure name server

      declare
         NS_Type : constant RE_Id := Name_Service_Type (Default_Name_Server);
      begin
         if NS_Type /= No_RE_Id then
            Write_Indentation;
            Write_Name (RU (RE_Unit_Table (RE_Name_Ctx)) and
                          RE (RE_Name_Ctx));
            Write_Line (" :=");
            Write_Indentation;
            Write_Str ("  new ");
            Write_Name (RU (RE_Unit_Table (NS_Type)) and RE (NS_Type));
            Write_Line (";");
         end if;
      end;

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Configure;");
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
      Write_Warnings_Pragmas;

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
              (Required_Storages.Table (Required_Storage).Is_Owner),
            Quote (Locations.Table (Location).Minor));

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
      File      : File_Descriptor;
      Current   : Partition_Type renames Partitions.Table (P);
      Conf_Unit : Conf_Unit_Id;
      Unit      : Unit_Id;

      procedure Start_File (Is_Body : Boolean);
      --  Start to write source file

      procedure Write_Declaration (Is_Body : Boolean);
      --  Start package declaration or package body

      procedure End_File;
      --  Complete write of current source file

      ----------------
      -- Start_File --
      ----------------

      procedure Start_File (Is_Body : Boolean) is
      begin
         Create_File (File,
           Dir (Current.Partition_Dir,
                (if Is_Body
                 then Part_Main_Body_Name
                 else Part_Main_Spec_Name)));
         Set_Output  (File);
         Write_Warnings_Pragmas;
      end Start_File;

      -----------------------
      -- Write_Declaration --
      -----------------------

      procedure Write_Declaration (Is_Body : Boolean) is
      begin
         Write_Str ("package ");
         if Is_Body then
            Write_Str ("body ");
         end if;
         Write_Name (Partition_Main_Name);
         Write_Line (" is");
      end Write_Declaration;

      --------------
      -- End_File --
      --------------

      procedure End_File is
      begin
         Write_Str  ("end ");
         Write_Name (Partition_Main_Name);
         Write_Line (";");

         Close (File);
         Set_Standard_Output;
      end End_File;

   --  Start of processing for Generate_Partition_Main_File

   begin
      Start_File (Is_Body => False);
      Write_Declaration (Is_Body => False);
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("pragma Elaborate_Body;");
      Decrement_Indentation;
      End_File;

      Start_File (Is_Body => True);

      --  Generate WITH clauses for the PCS and all configured user units,
      --  all with pragma Elaborate_All: the main unit must be the last
      --  elaborated one.

      Write_With_Clause (RU (RU_PolyORB_ORB),                E => True);
      Write_With_Clause (RU (RU_PolyORB_Initialization),     E => True);
      Write_With_Clause (RU (RU_PolyORB_Setup),              E => True);
      Write_With_Clause (RU (RU_System_Partition_Interface), E => True);
      Write_With_Clause (RU (RU_System_DSA_Services),        E => True);

      if Default_Name_Server = Multicast_NS then
         Write_With_Clause (RU (RU_PolyORB_Setup_Access_Points_mDNS));
      end if;

      --  Assign RCI or SP skels on the partition

      Conf_Unit := Current.First_Unit;
      while Conf_Unit /= No_Conf_Unit_Id loop
         Write_With_Clause (Conf_Units.Table (Conf_Unit).Name, E => True);
         Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
      end loop;

      --  Assign the RCI or SP stubs to compare version with skels

      for J in Current.First_Stub .. Current.Last_Stub loop
         Write_With_Clause (Stubs.Table (J), E => True);
      end loop;

      Write_Declaration (Is_Body => True);
      Write_Line ("begin");
      Increment_Indentation;

      --  Activate RPC receivers. From this point on, remote calls can be
      --  serviced by this partition.

      Write_Call (RU (RE_Unit_Table (RE_Activate_RPC_Receivers)) and
                  RE (RE_Activate_RPC_Receivers));

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

      --  We launch ORB.Run although this is only required for a non-tasking
      --  server. Note that this can be considered as incorrect since the
      --  environment task becomes indirectly part of the task pool???

      else
         Write_Call
           (RU (RE_Unit_Table (RE_Run)) and RE (RE_Run),
            RU (RE_Unit_Table (RE_The_ORB)) and RE (RE_The_ORB),
            S1 => "May_Exit => False");
      end if;

      Decrement_Indentation;
      End_File;
   end Generate_Partition_Main_File;

   --------------------------------
   -- Generate_PCS_Project_Files --
   --------------------------------

   procedure Generate_PCS_Project_Files is
      Prj_Fname  : File_Name_Type;
      Prj_File   : File_Descriptor;

      DSA_Inc_Dir : File_Name_Type;

      List_Flags  : Argument_Vec;
      Output      : File_Name_Type;
      First, Last : Text_Ptr;
      Buffer      : Text_Buffer_Ptr;

      Secondary_PCS_Project      : Name_Id;
      Secondary_PCS_Project_File : File_Name_Type;
   begin
      --  Get path to PolyORB sources by querying gnat list

      --  Note that we used to use PolyORB_Prefix for this, but this may
      --  lead to incorrect behaviour if that path involves symblic links,
      --  and we end up referencing sources using a diffrent path than the
      --  one the project manager expects.

      Push (List_Flags, "-Ppolyorb");
      Push (List_Flags, "-s");

      Set_Str_To_Name_Buffer ("polyorb.ali");
      List
        (Sources   => (1 => Name_Find),
         Arguments => List_Flags,
         Output    => Output);
      Read_File (Output, First, Last, Buffer);

      Set_Str_To_Name_Buffer (String (Buffer (First .. Last)));
      while Name_Buffer (Name_Len) /= Dir_Separator loop
         Name_Len := Name_Len - 1;
      end loop;
      Name_Len := Name_Len - 1;
      DSA_Inc_Dir := Name_Find;

      --  Create intermediate PCS project, extending the main PolyORB project,
      --  but removing source files that need to be rebuilt as client or server
      --  stubs, and those that are overridden by each partition.

      Get_Name_String (PCS_Project);
      Add_Char_To_Name_Buffer ('1');
      Secondary_PCS_Project := Name_Find;
      Set_Corresponding_Project_File_Name (Secondary_PCS_Project_File);

      Prj_Fname := Dir (Root_Id, Secondary_PCS_Project_File);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);
      Write_Str  ("project ");
      Write_Name (Secondary_PCS_Project);

      Write_Str  (" extends all ""polyorb""");
      Write_Line (" is");
      Write_Line ("   for Externally_Built use ""true"";");
      Write_Str  ("   for Source_Dirs use (""");
      Write_Name (DSA_Inc_Dir);
      Write_Line (""");");

      --  The files to be removed are the only source files for the extending
      --  project (all other sources are made visible as inherited), so we
      --  need to first list those files as sources, then as removed.

      for J in 1 .. 2 loop
         if J = 1 then
            Write_Line ("   for Source_Files use");
         else
            Write_Line ("   for Locally_Removed_Files use");
         end if;

         --  Overridden

         Write_Line ("     (""polyorb-partition_elaboration.adb"",");
         Write_Line ("      ""polyorb-dsa_p-storages-config.adb"",");

         --  Rebuilt as stubs

         Write_Line ("      ""polyorb-dsa_p-partitions.ads"",");
         Write_Line ("      ""polyorb-dsa_p-partitions.adb"");");
      end loop;

      Write_Str  ("end ");
      Write_Name (Secondary_PCS_Project);
      Write_Line (";");
      Close (Prj_File);
      Set_Standard_Output;

      --  Create project for PCS units that need to be rebuilt per-partition

      Prj_Fname := Dir (Root_Id, PCS_Project_File);
      Create_File (Prj_File, Prj_Fname);
      Set_Output (Prj_File);
      Write_Str  ("with """);
      Write_Name (Secondary_PCS_Project);
      Write_Line (""";");
      Write_Str  ("project ");
      Write_Name (PCS_Project);
      Write_Line (" is");
      Write_Line ("   for Object_Dir use ""obj"";");
      Write_Str  ("   for Source_Dirs use (""");
      Write_Name (DSA_Inc_Dir);
      Write_Line (""");");
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

   overriding function Get_Detach_Flag
     (Self : access PolyORB_Backend) return Name_Id
   is
      pragma Unreferenced (Self);
   begin
      return Id ("--polyorb-dsa-detach");
   end Get_Detach_Flag;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : access PolyORB_Backend) is
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

      for E in Valid_RE_Id loop
         RE (E) := Strip (RE_Id'Image (E));
      end loop;

      for S in PS_Id loop
         PS (S) := Strip (PS_Id'Image (S), To_Lower => True);
      end loop;

      for E in PE_Id loop
         PE (E) := Strip (PE_Id'Image (E), To_Lower => True);
      end loop;

      Generate_PCS_Project_Files;
      Generate_Application_Project_Files;
   end Initialize;

   -----------------------
   -- Name_Service_Type --
   -----------------------

   function Name_Service_Type (NS_Type : Name_Server_Type) return RE_Id is
   begin
      case NS_Type is
         when Embedded_NS | Standalone_NS =>
            return RE_COS_Name_Server;
         when Multicast_NS =>
            return RE_MDNS_Name_Server;
         when None_NS =>
            return RE_None_Name_Server;
         when others =>
            return No_RE_Id;
      end case;
   end Name_Service_Type;

   ----------------
   -- Reset_Conf --
   ----------------

   procedure Reset_Conf is
   begin
      Conf.Init;
   end Reset_Conf;

   -----------------------
   -- Register_Storages --
   -----------------------

   overriding procedure Register_Storages (Self : access PolyORB_Backend) is
      pragma Unreferenced (Self);
   begin
      Register_Storage
        (Storage_Name     => "dsm",
         Allow_Passive    => False,
         Allow_Local_Term => False,
         Need_Tasking     => True);
      --  Registrer "dsm" storage support

      Register_Storage
        (Storage_Name     => "dfs",
         Allow_Passive    => True,
         Allow_Local_Term => True,
         Need_Tasking     => False);
      --  Registrer "dfs" storage support

   end Register_Storages;

   -----------------
   -- Run_Backend --
   -----------------

   overriding procedure Run_Backend (Self : access PolyORB_Backend)
   is
      Current                   : Partition_Type;
      Is_Initiator_Set          : Boolean := False;
      Enable_Global_Termination : Boolean := False;
   begin
      Prepare_Directories;
      Generate_All_Stubs_And_Skels;

      --  Scan all partitions to check global application properties

      for J in Partitions.First + 1 .. Partitions.Last loop
         Current := Partitions.Table (J);

         --  Termination policy has been set for all built partitions

         pragma Assert (Current.Termination /= No_Termination
                          or else not Current.To_Build);

         --  Enable a termination initiator only if at least one node has
         --  global termination.

         if Current.Termination = Global_Termination then
            Enable_Global_Termination := True;
         end if;
      end loop;

      --  For each partition, generate the elaboration, main, executable
      --  and stamp files.

      for J in Partitions.First + 1 .. Partitions.Last loop
         Current := Partitions.Table (J);

         --  Reset the configuration table to forget all options set for
         --  previous partitions.

         Reset_Conf;

         --  Set termination initiator option on first partition with non local
         --  termination. Note that this must be done outside of the To_Build
         --  test. Otherwise, when building two partitions in separate gnatdist
         --  runs, both may end up being set up as initiators. Note that if
         --  no partition has global termination, then no initiator is defined.

         if not Is_Initiator_Set
           and then Current.Tasking /= No_Tasking
           and then Current.Termination /= Local_Termination
           and then Enable_Global_Termination
         then
            Set_Str_To_Name_Buffer ("true");
            Set_Conf (PE_Termination_Initiator, Name_Find);
            Is_Initiator_Set := True;
         end if;

         if Partitions.Table (J).To_Build then
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

         if Display_Compilation_Progress then
            Write_Str ("completed ");
            Write_Int (Int (J) - Int (Partition_Id'First) - 1);
            Write_Str (" out of ");
            Write_Int (Int (Partitions.Last) - Int (Partition_Id'First) - 1);
            Write_Str (" (");
            Write_Int
              (((Int (J) - Int (Partition_Id'First) - 1) * 100)
                  / Int (Partitions.Last - Partitions.First));
            Write_Str ("%)...");
            Write_Eol;
         end if;
      end loop;

      Generate_Starter_File (Backend_Access (Self));
   end Run_Backend;

   --------------
   -- Set_Conf --
   --------------

   procedure Set_Conf (Var : PE_Id; Val : Name_Id; Quote : Boolean := True) is
   begin
      Set_Conf (Section => PS (PE_Section_Table (Var)),
                Key     => PE (Var),
                Val     => Val,
                Quote   => Quote);
   end Set_Conf;

   procedure Set_Conf (Var : PE_Id; Val : Boolean; Quote : Boolean := True) is
   begin
      Set_Str_To_Name_Buffer (Boolean'Image (Val));
      Set_Conf (Var, Name_Find, Quote);
   end Set_Conf;

   procedure Set_Conf
     (Section : Name_Id;
      Key     : Name_Id;
      Val     : Name_Id;
      Quote   : Boolean)
   is
      Value : Name_Id;
   begin
      if Quote then
         Value := XE_Utils.Quote (Val);
      else
         Value := Val;
      end if;

      Conf.Append ((Section => Section, Key => Key, Value => Value));
   end Set_Conf;

   ------------------------
   -- Set_PCS_Dist_Flags --
   ------------------------

   overriding procedure Set_PCS_Dist_Flags (Self : access PolyORB_Backend) is
      pragma Unreferenced (Self);
   begin
      --  We get source, object and ALI paths and linker switches for the
      --  PolyORB runtime library through project files.

      --  Historically, on UNIX we used to rely on the polyorb-config script,
      --  but this does not work well in contexts where the path to the PolyORB
      --  sources involve symbolic links, because the paths we generate in the
      --  local PCS project files may be inconsistent with the project manager
      --  view of the original source paths from the installed polyorb project.
      --  See also comments in Generate_PCS_Project_Files.

      Scan_Dist_Arg ("-margs");
      Scan_Dist_Arg ("-aP" & PolyORB_Prefix
                           & "lib" & Dir_Separator & "gnat");
   end Set_PCS_Dist_Flags;

   -----------
   -- Strip --
   -----------

   function Strip
     (S        : String;
      To_Lower : Boolean := False) return Unit_Name_Type
   is
   begin
      Set_Str_To_Name_Buffer (S);
      Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));

      if To_Lower then
         for J in 1 .. Name_Len loop
            Name_Buffer (J) :=
              Ada.Characters.Handling.To_Lower (Name_Buffer (J));
         end loop;
      else
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));
      end if;

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
