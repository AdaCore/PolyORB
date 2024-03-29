------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       X E _ B A C K . G A R L I C                        --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with XE;          use XE;
with XE_Defs;     use XE_Defs;
with XE_Flags;    use XE_Flags;
with XE_Front;    use XE_Front;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Utils;    use XE_Utils;
with XE_Storages; use XE_Storages;

package body XE_Back.GARLIC is

   type GARLIC_Backend is new Backend with null record;

   overriding procedure Set_PCS_Dist_Flags (Self : access GARLIC_Backend);
   overriding procedure Initialize (Self : access GARLIC_Backend);
   overriding procedure Register_Storages (Self : access GARLIC_Backend);
   overriding procedure Run_Backend (Self : access GARLIC_Backend);
   overriding function Get_Detach_Flag
     (Self : access GARLIC_Backend) return Name_Id;

   Elaboration_File      : File_Name_Type;
   Protocol_Config_File  : File_Name_Type;
   Storage_Config_File   : File_Name_Type;

   type RU_Id is
     (RU_System,
      RU_System_Garlic,
      RU_System_Garlic_Elaboration,
      RU_System_Garlic_Filters,
      RU_System_Garlic_Heart,
      RU_System_Garlic_Name_Table,
      RU_System_Garlic_No_Tasking,
      RU_System_Garlic_Options,
      RU_System_Garlic_Priorities,
      RU_System_Garlic_Protocols,
      RU_System_Garlic_Protocols_Config,
      RU_System_Garlic_Remote,
      RU_System_Garlic_Startup,
      RU_System_Garlic_Storages,
      RU_System_Garlic_Storages_Config,
      RU_System_Garlic_Tasking,
      RU_System_Garlic_Types,
      RU_System_Partition_Interface,
      RU_System_RPC,
      RU_System_RPC_Server);

   RU : array (RU_Id) of Unit_Name_Type;

   type RE_Id is
     (RE_Register_Partition_To_Launch,
      RE_Set_RPC_Handler_Priority,
      RE_Set_RPC_Handler_Priority_Policy,
      RE_Set_Rsh_Command,
      RE_Set_Rsh_Options,
      RE_Set_Slave,
      RE_Set_Termination,
      RE_Set_Reconnection,
      RE_Set_Boot_Location,
      RE_Set_Self_Location,
      RE_Set_Data_Location,
      RE_Set_Nolaunch,
      RE_Set_Task_Pool_Bounds,
      RE_Set_Light_PCS,
      RE_Set_Pure_Client,
      RE_Initialize_0,
      RE_Initialize_1,
      RE_Initialize_2,
      RE_Set_Partition_Name,
      RE_Set_Registration_Filter,
      RE_Set_Default_Filter,
      RE_Set_Channel_Filter,
      RE_Register_Passive_Partition,
      RE_Register_Passive_Package,
      RE_Register_Passive_Package_On_Passive_Partition,
      RE_Elaborate_Passive_Partition,
      RE_Check,
      RE_Run,
      RE_Partition_ID,
      RE_Register);

   RE : array (RE_Id) of Unit_Name_Type;

   RE_Unit_Table : constant array (RE_Id) of RU_Id :=
     (RE_Register_Partition_To_Launch    => RU_System_Garlic_Remote,
      RE_Set_RPC_Handler_Priority        => RU_System_Garlic_Priorities,
      RE_Set_RPC_Handler_Priority_Policy => RU_System_Garlic_Priorities,
      RE_Set_Rsh_Command                 => RU_System_Garlic_Options,
      RE_Set_Rsh_Options                 => RU_System_Garlic_Options,
      RE_Set_Slave                       => RU_System_Garlic_Options,
      RE_Set_Termination                 => RU_System_Garlic_Options,
      RE_Set_Reconnection                => RU_System_Garlic_Options,
      RE_Set_Boot_Location               => RU_System_Garlic_Options,
      RE_Set_Self_Location               => RU_System_Garlic_Options,
      RE_Set_Data_Location               => RU_System_Garlic_Options,
      RE_Set_Nolaunch                    => RU_System_Garlic_Options,
      RE_Set_Task_Pool_Bounds            => RU_System_Garlic_Options,
      RE_Set_Light_PCS                   => RU_System_Garlic_Options,
      RE_Set_Pure_Client                 => RU_System_Garlic_Options,
      RE_Initialize_0                    => RU_System_Garlic_No_Tasking,
      RE_Initialize_1                    => RU_System_Garlic_Tasking,
      RE_Initialize_2                    => RU_System_Garlic_Name_Table,
      RE_Set_Partition_Name              => RU_System_Garlic_Options,
      RE_Set_Registration_Filter         => RU_System_Garlic_Filters,
      RE_Set_Default_Filter              => RU_System_Garlic_Filters,
      RE_Set_Channel_Filter              => RU_System_Garlic_Filters,
      RE_Register_Passive_Partition      => RU_System_Partition_Interface,
      RE_Register_Passive_Package        => RU_System_Partition_Interface,
      RE_Register_Passive_Package_On_Passive_Partition
                                         => RU_System_Partition_Interface,
      RE_Elaborate_Passive_Partition     => RU_System_Partition_Interface,
      RE_Check                           => RU_System_Partition_Interface,
      RE_Run                             => RU_System_Partition_Interface,
      RE_Partition_ID                    => RU_System_RPC,
      RE_Register                        => RU_System_Garlic_Protocols);

   procedure Add_Protocol
     (First : in out Location_Id;
      Last  : in out Location_Id;
      Name  : Name_Id);
   --  Add a protocol of name Name to the chained list which starts at
   --  First and ends at Last.

   procedure Generate_Elaboration_File (P : Partition_Id);
   --  Create the elaboration unit for the given partition. This unit
   --  overloads the default PCS settings.

   procedure Generate_Executable_File (P : Partition_Id);
   --  Compile main partition file and elaboration file.
   --  Bind and link partition to create executable.

   procedure Generate_PCS_Project_Files;

   procedure Generate_Partition_Main_File (P : Partition_Id);
   --  Create a procedure which "withes" all the RCI or SP receivers
   --  of the partition and insert the main procedure if needed.

   procedure Generate_Overridden_PCS_Units_File
     (P : Partition_Id;
      F : out File_Descriptor);
   --  Generate the initial list of PCS files that are overridden by the
   --  partition. Further per-partition generation subprograms may append to
   --  that list by writing further lines using the returned file descriptor.

   procedure Generate_Protocol_Config_File
     (P : Partition_Id;
      F : File_Descriptor);
   --  Create protocol configuration file that includes the protocols required
   --  for this partition.
   --  If the unit is generated, write its name to F to note that the
   --  corresponding PCS source is overridden.

   procedure Generate_Storage_Config_File
     (P : Partition_Id;
      F : File_Descriptor);
   --  Create storage configuration file that includes the storages required
   --  for this partition.
   --  If the unit is generated, write its name to F to note that the
   --  corresponding PCS source is overridden.

   function Name (U : Unit_Id) return Name_Id;
   --  Take a unit id and return its name removing unit suffix.

   DSA_Inc_Rel_Dir : constant String :=
                      "lib" & Directory_Separator & "garlic";
   --  GARLIC include directory, relative to the installation prefix

   GARLIC_Prefix : constant String :=
                     XE_Back.Prefix
                       (Check_For => DSA_Inc_Rel_Dir
                                      & Dir_Separator & "s-garlic.ads");

   DSA_Inc_Dir : constant String := GARLIC_Prefix & DSA_Inc_Rel_Dir;

   ------------------
   -- Add_Protocol --
   ------------------

   procedure Add_Protocol
     (First : in out Location_Id;
      Last  : in out Location_Id;
      Name  : Name_Id)
   is
      LID : Location_Id;

   begin
      if First /= No_Location_Id then
         LID := First;
         while LID /= No_Location_Id loop
            if Locations.Table (LID).Major = Name then
               return;
            end if;
            LID := Locations.Table (LID).Next_Location;
         end loop;
      end if;
      Add_Location (First, Last, Name, No_Name);
   end Add_Protocol;

   -------------------------------
   -- Generate_Elaboration_File --
   -------------------------------

   procedure Generate_Elaboration_File (P : Partition_Id) is

      procedure Register_Launched_Partition (P : Partition_Id);

      ---------------------------------
      -- Register_Launched_Partition --
      ---------------------------------

      procedure Register_Launched_Partition (P : Partition_Id) is
         Current      : Partition_Type renames Partitions.Table (P);
         Use_Rem_Host : Boolean;
         Remote_Host  : Name_Id;
         Executable   : File_Name_Type := Current.Executable_File;

      begin
         Write_Image (Remote_Host, Current.Host, P);
         Use_Rem_Host := Present (Remote_Host);

         if not Use_Rem_Host then
            Remote_Host := Quote (Current.Name);
         end if;

         Executable := Strip_Exec_Suffix (Executable);
         Write_Call
           (RE (RE_Register_Partition_To_Launch),
            N1 => Capitalize (Id (Boolean'Image (Use_Rem_Host))),
            S1 => Get_Name_String (Remote_Host),
            N2 => Quote (Id (Get_Env_Vars (P, Names_Only => False))),
            N3 => Quote (To_Absolute_File (Executable)
                           & Current.Command_Line));
      end Register_Launched_Partition;

      Filename     : File_Name_Type;
      File         : File_Descriptor;
      Current      : Partition_Type renames Partitions.Table (P);
      Channel      : Channel_Id;
      Filter       : Filter_Name_Type;
      Peer         : Partition_Id;

   begin
      Filename := Elaboration_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);
      Write_Warnings_Pragmas;

      Write_With_Clause (RU (RU_System_Garlic_Filters), True);
      Write_With_Clause (RU (RU_System_Garlic_Heart), True);
      Write_With_Clause (RU (RU_System_Garlic_Name_Table), True);
      Write_With_Clause (RU (RU_System_Garlic_Options), True);
      Write_With_Clause (RU (RU_System_Garlic_Priorities), True);
      Write_With_Clause (RU (RU_System_Garlic_Remote), True);
      Write_With_Clause (RU (RU_System_Garlic_Types), True);

      if Current.Tasking = No_Tasking then
         Write_With_Clause (RU (RU_System_Garlic_No_Tasking), False, True);

      else
         Write_With_Clause (RU (RU_System_Garlic_Tasking), False, True);
      end if;

      --  Add filtering package if needed

      Filter := Default_Registration_Filter;
      if Present (Filter) then
         Write_With_Clause (RU (RU_System_Garlic_Filters)
                            and Capitalize (Filter));
      end if;

      if Present (Current.Filter) then
         Write_With_Clause (RU (RU_System_Garlic_Filters)
                            and Capitalize (Current.Filter));
      end if;

      if Current.First_Channel /= No_Channel_Id then
         Channel := Current.First_Channel;
         while Channel /= No_Channel_Id loop
            Filter := Channels.Table (Channel).Filter;
            if Present (Filter) then
               Write_With_Clause (RU (RU_System_Garlic_Filters)
                                  and Capitalize (Filter));
            end if;
            if Channels.Table (Channel).Lower.My_Partition = P then
               Channel := Channels.Table (Channel).Lower.Next_Channel;
            else
               Channel := Channels.Table (Channel).Upper.Next_Channel;
            end if;
         end loop;
      end if;

      Write_Str  ("package body ");
      Write_Name (RU (RU_System_Garlic_Elaboration));
      Write_Line (" is");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("procedure Initialize is");
      Write_Indentation;
      Write_Line ("begin");

      Increment_Indentation;
      if Current.Priority /= No_Priority then
         Write_Call (RE (RE_Set_RPC_Handler_Priority),
                     No_Name,
                     I1_Present => True,
                     I1         => Int (Current.Priority));
      end if;

      if Default_Priority_Policy /= No_Priority_Policy then
         Write_Call (RE (RE_Set_RPC_Handler_Priority_Policy),
                     Priority_Policy_Img (Default_Priority_Policy));
      end if;

      Write_Call (RE (RE_Set_Rsh_Command), Quote (Get_Rsh_Command));
      Write_Call (RE (RE_Set_Rsh_Options), Quote (Get_Rsh_Options));

      --  If the partition holds the main unit, then it cannot be slave.
      --  Otherwise, it is.

      if P /= Main_Partition then
         Write_Call (RE (RE_Set_Slave), Id ("True"));
      else
         Write_Call (RE (RE_Set_Slave), Id ("False"));
      end if;

      --  How should the partition terminate. Note that
      --  Global_Termination is the default. No need to force the
      --  default.

      if Current.Termination /= No_Termination then
         Write_Call (RE (RE_Set_Termination),
                     Termination_Img (Current.Termination));
      end if;

      --  When this partition is restarted, how should we handle
      --  reconnections?

      if Current.Reconnection /= No_Reconnection then
         Write_Call (RE (RE_Set_Reconnection),
                     Reconnection_Img (Current.Reconnection));
      end if;

      --  If a protocol has been specified, then use it (with its data
      --  if present).

      if Default_First_Boot_Location /= No_Location_Id then
         Write_Call (RE (RE_Set_Boot_Location),
                     Location_List_Image (Default_First_Boot_Location));
      end if;

      --  Compute the self location string (eventually composed of
      --  several locations separated by commas).

      if Current.First_Network_Loc /= No_Location_Id then
         Write_Call (RE (RE_Set_Self_Location),
                     Location_List_Image (Current.First_Network_Loc));
      end if;

      --  Compute the data location string (eventually composed of
      --  several locations separated by commas).

      if Current.Storage_Loc /= No_Location_Id then
         Write_Call (RE (RE_Set_Data_Location),
                     Location_List_Image (Current.Storage_Loc));
      end if;

      --  If we have no Ada starter (None or Shell), then it is equivalent
      --  to having --nolaunch on the command line.

      if Default_Starter /= Ada_Import then
         Write_Call (RE (RE_Set_Nolaunch), No_Name, "True");
      end if;

      --  Do we want to control the number of anonymous tasks

      if Current.Task_Pool /= No_Task_Pool then
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Int'Image (Current.Task_Pool (1)));
         Add_Char_To_Name_Buffer (',');
         Add_Str_To_Name_Buffer (Int'Image (Current.Task_Pool (2)));
         Add_Char_To_Name_Buffer (',');
         Add_Str_To_Name_Buffer (Int'Image (Current.Task_Pool (3)));
         Write_Call (RE (RE_Set_Task_Pool_Bounds),
                     Id (Name_Buffer (2 .. Name_Len)));
      end if;

      --  Set tasking policy and with appropriate package

      if Current.Tasking = No_Tasking then
         Write_Call (RE (RE_Initialize_0));
         Write_Call (RE (RE_Set_Light_PCS), Id ("True"));

      elsif Current.Tasking = User_Tasking then
         Write_Call (RE (RE_Initialize_1));
         Write_Call (RE (RE_Set_Pure_Client), Id ("True"));

      else  --  Get_Tasking (P) = 'P'
         Write_Call (RE (RE_Initialize_1));
      end if;

      --  Elaborate Name_Table

      Write_Call (RE (RE_Initialize_2));

      Write_Call (RE (RE_Set_Partition_Name), Quote (Current.Name));

      --  Set registration filter, default filter and partition filters

      Filter := Default_Registration_Filter;
      if Present (Filter) then
         Write_Call (RE (RE_Set_Registration_Filter), Quote (Filter));
      end if;

      Filter := Partitions.Table (Default_Partition_Id).Filter;
      if Present (Filter) then
         Write_Call (RE (RE_Set_Default_Filter), Quote (Filter));
      end if;

      Channel := Current.First_Channel;
      while Channel /= No_Channel_Id loop
         Filter := Channels.Table (Channel).Filter;

         if Channels.Table (Channel).Lower.My_Partition = P then
            Peer    := Channels.Table (Channel).Upper.My_Partition;
            Channel := Channels.Table (Channel).Lower.Next_Channel;
         else
            Peer    := Channels.Table (Channel).Lower.My_Partition;
            Channel := Channels.Table (Channel).Upper.Next_Channel;
         end if;

         if Present (Filter) then
            Write_Call (RE (RE_Set_Channel_Filter),
                        Quote (Partitions.Table (Peer).Name), No_Str,
                        Quote (Filter));
         end if;
      end loop;

      --  Initialize filters

      Filter := Default_Registration_Filter;
      if Present (Filter) then
         Write_Call (RU (RU_System_Garlic_Filters)
                     and Capitalize (Filter) and "Initialize");
      end if;

      if Present (Current.Filter) then
         Write_Call (RU (RU_System_Garlic_Filters)
                     and Capitalize (Current.Filter) and "Initialize");
      end if;

      Channel := Current.First_Channel;
      while Channel /= No_Channel_Id loop
         Filter := Channels.Table (Channel).Filter;

         if Present (Filter) then
            Write_Call (RU (RU_System_Garlic_Filters)
                        and Capitalize (Filter) and "Initialize");
         end if;

         if Channels.Table (Channel).Lower.My_Partition = P then
            Channel := Channels.Table (Channel).Lower.Next_Channel;
         else
            Channel := Channels.Table (Channel).Upper.Next_Channel;
         end if;
      end loop;

      if P = Main_Partition then
         if Default_Starter = Ada_Import then
            for J in Partitions.First + 1 .. Partitions.Last loop
               if J /= Main_Partition
                 and then Partitions.Table (J).Passive /= BTrue
               then
                  Register_Launched_Partition (J);
               end if;
            end loop;
         end if;
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Initialize;");
      Decrement_Indentation;
      Write_Str  ("end ");
      Write_Name (RU (RU_System_Garlic_Elaboration));
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
      Push (Comp_Args, Partition_Dir_Flag (P));

      --  Give the priority to partition and stub directory against
      --  current directory.

      Push (Comp_Args, "-cargs");
      Push (Comp_Args, E_Current_Dir);
      Push (Comp_Args, I_Part_Dir);
      Push (Comp_Args, I_Current_Dir);

      Push (Comp_Args, "-bargs");
      Push (Comp_Args, E_Current_Dir);
      Push (Comp_Args, I_Part_Dir);
      Push (Comp_Args, A_Stub_Dir);
      Push (Comp_Args, I_Current_Dir);

      --  Compile Garlic elaboration file

      Sfile := Elaboration_File & ADB_Suffix_Id;
      Sfile := Dir (Partition_Dir, Sfile);
      Compile (Sfile, Comp_Args);

      --  Compile protocol configuration file if any

      Sfile := Protocol_Config_File & ADB_Suffix_Id;
      if Is_Regular_File (Dir (Partition_Dir, Sfile)) then
         Sfile := Dir (Partition_Dir, Sfile);
         Compile (Sfile, Comp_Args);
      end if;

      --  Compile storage support configuration file if any

      Sfile := Storage_Config_File & ADB_Suffix_Id;
      if Is_Regular_File (Dir (Partition_Dir, Sfile)) then
         Sfile := Dir (Partition_Dir, Sfile);
         Compile (Sfile, Comp_Args);
      end if;

      --  We already checked the consistency of all the partition
      --  units. In case of an inconsistency of exception mode, we may
      --  have to rebuild some parts of garlic (units configured just
      --  for this partition). Note that some parts of Garlic may have
      --  been already recompiled when the monolithic application was
      --  initially built. Some bodies may be missing as they are
      --  assigned to partitions we do not want to build. So compile
      --  silently and do not exit on errors (keep going).

      Sfile := Part_Main_Body_Name;
      Build (Sfile, Comp_Args, Fatal => False);

      --  Now we just want to bind and link as the ALI files are now
      --  consistent.

      Push (Make_Args, E_Current_Dir);
      Push (Make_Args, I_Part_Dir);
      Push (Make_Args, A_Stub_Dir);
      Push (Make_Args, I_Current_Dir);
      Push (Make_Args, Bind_Only_Flag);
      Push (Make_Args, Link_Only_Flag);

      Push (Make_Args, Project_File_Flag);
      Push (Make_Args, Dir (Partition_Dir, Part_Prj_File_Name));
      Push (Make_Args, Partition_Dir_Flag (P));

      Push (Make_Args, Output_Flag);
      Push (Make_Args, Strip_Directory (Executable));

      --  While binding and linking partitions, the original (monolithic)
      --  objects and ALIs must be moved out of the builder's visibility,
      --  so that they can be replaced by the stubbed versions supplied
      --  through directories specified on the command line.

      Hide_Stubbed_Units;
      Build (Sfile, Make_Args, Fatal => True);
      Unhide_Stubbed_Units;
   end Generate_Executable_File;

   ----------------------------------
   -- Generate_Partition_Main_File --
   ----------------------------------

   procedure Generate_Partition_Main_File (P : Partition_Id) is

      Filename  : File_Name_Type;
      File      : File_Descriptor;
      Current   : Partition_Type renames Partitions.Table (P);
      Conf_Unit : Conf_Unit_Id;
      Unit      : Unit_Id;
      Variable  : constant Name_Id := Id ("Partition");
      Location  : Location_Id;

      function Import_Stub_From (Partition : Partition_Id) return Boolean;
      --  Return True when we include stubs assigned to Partition

      ----------------------
      -- Import_Stub_From --
      ----------------------

      function Import_Stub_From (Partition : Partition_Id) return Boolean is
      begin
         for J in Current.First_Stub .. Current.Last_Stub loop
            if Get_Partition_Id (Stubs.Table (J)) = Partition then
               return True;
            end if;
         end loop;

         return False;
      end Import_Stub_From;

   begin
      Filename := Part_Main_Body_Name;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);
      Write_Warnings_Pragmas;

      Write_With_Clause (RU (RU_System_Partition_Interface), True);
      Write_With_Clause (RU (RU_System_RPC));
      if Current.Tasking /= No_Tasking then
         Write_With_Clause (RU (RU_System_RPC_Server));
      end if;
      Write_With_Clause (RU (RU_System_Garlic_Startup), False, True);

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

      Increment_Indentation;
      Write_Indentation;
      Write_Name (Variable);
      Write_Line (" : ");
      Write_Name (RE (RE_Partition_ID));
      Write_Line (";");

      Decrement_Indentation;
      Write_Line ("begin");

      Increment_Indentation;

      --  Register passive partitions and their shared passive
      --  packages as they are not going to do this registration by
      --  themselves.

      for J in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (J).Passive = BTrue
           and then Import_Stub_From (J)
         then
            Location := Partitions.Table (J).Storage_Loc;
            if Location = No_Location_Id then
               Location := Default_Data_Location;
            end if;

            Write_Call (RE (RE_Register_Passive_Partition),
                        Variable, No_Str,
                        Quote (Partitions.Table (J).Name),
                        Get_Name_String (Location_List_Image (Location)));

            Conf_Unit := Partitions.Table (J).First_Unit;
            while Conf_Unit /= No_Conf_Unit_Id loop
               Write_Call
                 (RE (RE_Register_Passive_Package_On_Passive_Partition),
                  Variable, No_Str,
                  Quote (Conf_Units.Table (Conf_Unit).Name), No_Str,
                  Conf_Units.Table (Conf_Unit).Name & "'Version");
               Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
            end loop;

            Conf_Unit := Partitions.Table (J).First_Unit;
            if Conf_Unit /= No_Conf_Unit_Id then
               Write_Call (RE (RE_Elaborate_Passive_Partition), Variable);
            end if;
         end if;
      end loop;

      --  Register shared passive packages since they have no
      --  elaboration code.

      Conf_Unit := Current.First_Unit;
      while Conf_Unit /= No_Conf_Unit_Id loop
         Unit := Conf_Units.Table (Conf_Unit).My_Unit;
         if Units.Table (Unit).Shared_Passive then
            Write_Call (RE (RE_Register_Passive_Package),
                        Quote (Conf_Units.Table (Conf_Unit).Name),
                        Get_Name_String (Name (Unit)) & "'Version");
         end if;
         Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
      end loop;

      --  Check version consistency of RCI and SP stubs

      if Default_Version_Check then
         for J in Current.First_Stub .. Current.Last_Stub loop
            Unit := ALIs.Table (Get_ALI_Id (Stubs.Table (J))).Last_Unit;
            Write_Call (RE (RE_Check),
                        Quote (Stubs.Table (J)), No_Str,
                        Stubs.Table (J) & "'Version",
                        Boolean'Image (Units.Table (Unit).RCI));
         end loop;
      end if;

      --   Invoke main subprogram through Run routine

      if Present (Current.Main_Subprogram) then
         Get_Name_String (Current.Main_Subprogram);
         Add_Str_To_Name_Buffer ("'Access");
         Write_Call (RE (RE_Run), Name_Find);

      else
         Write_Call (RE (RE_Run));
      end if;

      Decrement_Indentation;
      Write_Str  ("end ");
      Write_Name (Partition_Main_Name);
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Partition_Main_File;

   ----------------------------------------
   -- Generate_Overridden_PCS_Units_File --
   ----------------------------------------

   procedure Generate_Overridden_PCS_Units_File
     (P : Partition_Id;
      F : out File_Descriptor)
   is
      Partition_Dir : Directory_Name_Type;
      Filename      : File_Name_Type;
   begin
      if P = No_Partition_Id then
         Partition_Dir := Stub_Dir_Name;
      else
         Partition_Dir := Partitions.Table (P).Partition_Dir;
      end if;
      Filename := Dir (Partition_Dir, Overridden_PCS_Units);
      Create_File (F, Filename);
      Set_Output  (F);

      --  Always overridden

      Write_Line  ("s-garela.adb");

      if User_Provided_S_RPC then
         --  User has provided an alternative version of s-rpc: remove the
         --  one from GARLIC from the project.

         Write_Line ("s-rpc.adb");
      end if;

      Set_Standard_Output;
   end Generate_Overridden_PCS_Units_File;

   --------------------------------
   -- Generate_PCS_Project_Files --
   --------------------------------

   procedure Generate_PCS_Project_Files is
      Prj_Fname : File_Name_Type;
      File      : File_Descriptor;
   begin
      --  Use GARLIC sources, but remove files that need to be overridden
      --  per-partition.

      Prj_Fname := Dir (Root_Id, PCS_Project_File);
      Create_File (File, Prj_Fname);
      Set_Output (File);
      Write_Str  ("project ");
      Write_Name (PCS_Project);

      Write_Line (" is");
      Write_Line ("   for Externally_Built use ""true"";");
      Write_Line ("   for Source_Dirs use (""" & DSA_Inc_Dir & """);");
      Write_Str  ("   Partition_Dir := external (""PARTITION_DIR"", """);
      Write_Name (Stub_Dir_Name);
      Write_Line (""");");
      Write_Str  ("   for Excluded_Source_List_File use"
                  & " Partition_Dir & """ & Dir_Separator);
      Write_Name (Overridden_PCS_Units);
      Write_Line (""";");
      Write_Str  ("end ");
      Write_Name (PCS_Project);
      Write_Line (";");
      Close (File);
      Set_Standard_Output;

      --  Generate default exclusion list for RCI calling stubs

      Generate_Overridden_PCS_Units_File (No_Partition_Id, File);
      Close (File);
   end Generate_PCS_Project_Files;

   -----------------------------------
   -- Generate_Protocol_Config_File --
   -----------------------------------

   procedure Generate_Protocol_Config_File
     (P : Partition_Id;
      F : File_Descriptor)
   is
      Filename  : File_Name_Type;
      File      : File_Descriptor;
      Current   : Partition_Type renames Partitions.Table (P);
      Major     : Name_Id;
      Location  : Location_Id;
      First_Loc : Location_Id := No_Location_Id;
      Last_Loc  : Location_Id := No_Location_Id;
      Light_PCS : Boolean;

   begin
      Filename := Dir (Current.Partition_Dir, Protocol_Config_File);
      Location := Current.First_Network_Loc;

      Light_PCS := Current.Tasking = No_Tasking
        and then Current.Light_PCS /= BFalse;

      --  Having no protocol configured on this partition is not
      --  enough for using the default protocol configuration. The
      --  default protocol configuration is customized for a tasking
      --  profile. Therefore, in case of light PCS, we have to produce
      --  a specific protocol configuration.

      if Location = No_Location_Id
        and then not Light_PCS
      then
         Delete_File (Filename & ADB_Suffix_Id);
         Delete_File (Filename & ALI_Suffix_Id);
         Delete_File (Filename & Obj_Suffix_Id);
         return;
      end if;

      Filename := Filename & ADB_Suffix_Id;

      --  Record that this PCS source file is overridden

      Set_Output (F);
      Write_Name (Protocol_Config_File);
      Write_Name (ADB_Suffix_Id);
      Write_Eol;

      Create_File (File, Filename);
      Set_Output  (File);
      Write_Warnings_Pragmas;

      --  Required location protocols

      while Location /= No_Location_Id loop
         Add_Protocol (First_Loc, Last_Loc, Locations.Table (Location).Major);
         Location := Locations.Table (Location).Next_Location;
      end loop;

      if First_Loc = No_Location_Id then
         Location := Default_First_Boot_Location;
         while Location /= No_Location_Id loop
            Add_Protocol
              (First_Loc, Last_Loc, Locations.Table (Location).Major);
            Location := Locations.Table (Location).Next_Location;
         end loop;
      end if;

      if First_Loc = No_Location_Id then
         Add_Protocol (First_Loc, Last_Loc, Id (Get_Def_Protocol_Name));
      end if;

      Location := First_Loc;
      while Location /= No_Location_Id loop
         Major := Capitalize (Locations.Table (Location).Major);
         Major := RU (RU_System_Garlic_Protocols) and Major;
         Write_With_Clause (Major, False, True);
         if Current.Tasking /= No_Tasking then
            Major := Major and "Server";
            Write_With_Clause (Major, False, True);
         end if;
         Location := Locations.Table (Location).Next_Location;
      end loop;

      Write_Str  ("package body ");
      Write_Name (RU (RU_System_Garlic_Protocols_Config));
      Write_Line (" is");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("procedure Initialize is");
      Write_Indentation;
      Write_Line ("begin");

      --  Register protocols used in partition locations

      Increment_Indentation;
      Location := First_Loc;
      while Location /= No_Location_Id loop
         Major := Capitalize (Locations.Table (Location).Major);
         Major := RU (RU_System_Garlic_Protocols) and Major;
         Write_Call (RE (RE_Register), Major and "Create");
         Location := Locations.Table (Location).Next_Location;
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Initialize;");
      Decrement_Indentation;
      Write_Eol;
      Write_Str  ("end ");
      Write_Name (RU (RU_System_Garlic_Protocols_Config));
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Protocol_Config_File;

   ----------------------------------
   -- Generate_Storage_Config_File --
   ----------------------------------

   procedure Generate_Storage_Config_File
     (P : Partition_Id;
      F : File_Descriptor)
   is
      Filename    : File_Name_Type;
      File        : File_Descriptor := Invalid_FD;
      Current     : Partition_Type renames Partitions.Table (P);
      Partition   : Partition_Id;
      Location    : Location_Id;
      Uname       : Unit_Name_Type;
      Major       : Name_Id;
      Use_Default : Boolean := False;
      Conf_Unit   : Conf_Unit_Id;
      Unit        : Unit_Id;

      procedure Setup_Output;
      --  Open output file and record it as an overridden PCS unit, if not
      --  already done.

      ------------------
      -- Setup_Output --
      ------------------

      procedure Setup_Output is
      begin
         if File = Invalid_FD then

            --  Record that this PCS source file is overridden

            Set_Output (F);
            Write_Name (Storage_Config_File);
            Write_Name (ADB_Suffix_Id);
            Write_Eol;

            Create_File (File, Filename);
            Set_Output  (File);
            Write_Warnings_Pragmas;
         end if;
      end Setup_Output;

   begin
      Filename := Dir (Current.Partition_Dir, Storage_Config_File);
      Filename := Filename & ADB_Suffix_Id;
      Delete_File (To_Afile (Filename));
      Delete_File (To_Ofile (Filename));
      Delete_File (Filename);

      --  Import the storage supports needed for shared passive stub
      --  packages configured on other partitions.

      for S in Current.First_Stub .. Current.Last_Stub loop
         Uname := Stubs.Table (S);
         Unit  := ALIs.Table (Get_ALI_Id (Uname)).Last_Unit;

         if Units.Table (Unit).Shared_Passive then
            Setup_Output;

            Partition := Get_Partition_Id (Uname);
            Location  := Partitions.Table (Partition).Storage_Loc;

            if Location /= No_Location_Id then
               Major := Capitalize (Locations.Table (Location).Major);
               Major := RU (RU_System_Garlic_Storages) and Major;
               Write_With_Clause (Major, False, True);

            else
               Use_Default := True;
            end if;
         end if;
      end loop;

      --  Import storage supports needed for shared passive packages
      --  configured on this partition.

      Conf_Unit := Current.First_Unit;
      while Conf_Unit /= No_Conf_Unit_Id loop
         Unit := Conf_Units.Table (Conf_Unit).My_Unit;

         if Units.Table (Unit).Shared_Passive then
            Setup_Output;

            Location := Current.Storage_Loc;
            if Location /= No_Location_Id then
               Major := Capitalize (Locations.Table (Location).Major);
               Major := RU (RU_System_Garlic_Storages) and Major;
               Write_With_Clause (Major, False, True);

            else
               Use_Default := True;
            end if;
         end if;

         Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
      end loop;

      --  Import default storage supports when a partition did not
      --  come with its own.

      if Use_Default then
         Location := Default_Data_Location;
         Major := Capitalize (Locations.Table (Location).Major);
         Major := RU (RU_System_Garlic_Storages) and Major;
         Write_With_Clause (Major, False, True);
      end if;

      if File = Invalid_FD then
         return;
      end if;

      --  Initialize storage supports

      Write_Str  ("package body ");
      Write_Name (RU (RU_System_Garlic_Storages_Config));
      Write_Line (" is");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("procedure Initialize is");
      Write_Indentation;
      Write_Line ("begin");

      --  Follow the same approach as for package importation

      Increment_Indentation;
      for S in Current.First_Stub .. Current.Last_Stub loop
         Uname := Stubs.Table (S);
         Unit  := ALIs.Table (Get_ALI_Id (Uname)).Last_Unit;

         if Units.Table (Unit).Shared_Passive then
            Partition := Get_Partition_Id (Uname);
            Location  := Partitions.Table (Partition).Storage_Loc;
            if Location /= No_Location_Id then
               Major := Locations.Table (Location).Major;
               Write_Call (RU (RU_System_Garlic_Storages)
                           and Capitalize (Major) and "Initialize");
            end if;
         end if;
      end loop;

      Conf_Unit := Current.First_Unit;
      while Conf_Unit /= No_Conf_Unit_Id loop
         Unit := Conf_Units.Table (Conf_Unit).My_Unit;

         if Units.Table (Unit).Shared_Passive then
            Location := Current.Storage_Loc;

            if Location /= No_Location_Id then
               Major := Locations.Table (Location).Major;
               Write_Call (RU (RU_System_Garlic_Storages)
                           and Capitalize (Major) and "Initialize");
            end if;
         end if;

         Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
      end loop;

      if Use_Default then
         Major := Locations.Table (Default_Data_Location).Major;
         Write_Call (RU (RU_System_Garlic_Storages)
                     and Capitalize (Major) and "Initialize");
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Initialize;");
      Decrement_Indentation;
      Write_Eol;
      Write_Str  ("end ");
      Write_Name (RU (RU_System_Garlic_Storages_Config));
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Storage_Config_File;

   ---------------------
   -- Get_Detach_Flag --
   ---------------------

   overriding function Get_Detach_Flag
     (Self : access GARLIC_Backend) return Name_Id
   is
      pragma Unreferenced (Self);
   begin
      return Id ("--detach");
   end Get_Detach_Flag;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : access GARLIC_Backend) is
      pragma Unreferenced (Self);

      Position : Integer;
      Length   : Natural;

   begin
      XE_Back.Initialize;
      Elaboration_File     := Id ("s-garela");
      Protocol_Config_File := Id ("s-gaprco");
      Storage_Config_File  := Id ("s-gastco");

      Register_Casing_Rule ("PCS");
      Register_Casing_Rule ("RPC");

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

      Generate_PCS_Project_Files;
      Generate_Application_Project_Files;
   end Initialize;

   ----------
   -- Name --
   ----------

   function Name (U : Unit_Id) return Name_Id is
   begin
      return Name (Units.Table (U).Uname);
   end Name;

   -----------------------
   -- Register_Storages --
   -----------------------

   overriding procedure Register_Storages (Self : access GARLIC_Backend) is
      pragma Unreferenced (Self);
   begin
      Register_Storage
        (Storage_Name     => "dfs",
         Allow_Passive    => True,
         Allow_Local_Term => True,
         Need_Tasking     => False);
      --  Registrer "dfs" storage support

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

   overriding procedure Run_Backend (Self : access GARLIC_Backend)
   is
      type Pass_Type is (Prepare, Build);
      --  We iterate twice over all partitions: a Prepare pass where source
      --  code and configuration files are produced, and then a Build pass
      --  after stubs have been compiled.

      procedure Iterate_Over_All_Partitions (Pass : Pass_Type);
      --  Execute required processing for the given pass for all partitions
      --  that need to be built.

      procedure Iterate_Over_All_Partitions (Pass : Pass_Type) is
         Current              : Partition_Type;
         Overridden_PCS_Units : File_Descriptor;
      begin
         for J in Partitions.First + 1 .. Partitions.Last loop
            if Partitions.Table (J).To_Build then
               Current := Partitions.Table (J);

               if Current.To_Build and then Current.Passive /= BTrue then
                  if Rebuild_Partition (J) then
                     case Pass is
                        when Prepare =>
                           Generate_Partition_Main_File (J);

                           --  Prepare overridden PCS units. This is required
                           --  for building the receiver stubs, since they
                           --  use the partition project file.

                           Generate_Overridden_PCS_Units_File
                             (J, Overridden_PCS_Units);
                           Generate_Elaboration_File (J);
                           Generate_Protocol_Config_File
                             (J, Overridden_PCS_Units);
                           Generate_Storage_Config_File
                             (J, Overridden_PCS_Units);
                           Close (Overridden_PCS_Units);

                        when Build =>
                           if not Quiet_Mode then
                              Message ("building partition", Current.Name);
                           end if;
                           Generate_Executable_File (J);
                           Generate_Stamp_File (J);
                     end case;
                  end if;

               elsif Verbose_Mode and then Pass = Prepare then
                  Message ("no need to expand", Current.Name);
               end if;
            end if;
         end loop;
      end Iterate_Over_All_Partitions;

   begin
      Prepare_Directories;
      Iterate_Over_All_Partitions (Pass => Prepare);

      Generate_All_Stubs_And_Skels;
      Iterate_Over_All_Partitions (Pass => Build);

      Generate_Starter_File (Backend_Access (Self));
   end Run_Backend;

   ------------------------
   -- Set_PCS_Dist_Flags --
   ------------------------

   overriding procedure Set_PCS_Dist_Flags (Self : access GARLIC_Backend) is
      pragma Unreferenced (Self);
   begin
      if not Is_Directory (DSA_Inc_Dir) then
         raise Fatal_Error with "GARLIC library not found";
      end if;

      Scan_Dist_Arg ("-aI" & DSA_Inc_Dir);
      Scan_Dist_Arg ("-aO" & DSA_Inc_Dir);
      Scan_Dist_Arg ("-largs");
      Scan_Dist_Arg ("-L" & DSA_Inc_Dir);
      Scan_Dist_Arg ("-lgarlic");
   end Set_PCS_Dist_Flags;

begin
   Register_Backend ("garlic", new GARLIC_Backend);
end XE_Back.GARLIC;
