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
with XE_Defs;     use XE_Defs;
with XE_Flags;    use XE_Flags;
with XE_Front;    use XE_Front;
with XE_IO;       use XE_IO;
with XE_Names;    use XE_Names;
with XE_Types;    use XE_Types;
with XE_Units;    use XE_Units;
with XE_Utils;    use XE_Utils;

package body XE_Back is

   PCS_Name              : File_Name_Type;
   Build_Stamp_File      : File_Name_Type;
   Elaboration_File      : File_Name_Type;
   Partition_Main_File   : File_Name_Type;
   Protocol_Config_File  : File_Name_Type;
   Storage_Config_File   : File_Name_Type;

   Config                : Unit_Name_Type;
   Partition_Main_Name   : Unit_Name_Type;
   Partition_Interface   : Unit_Name_Type;
   RPC                   : Unit_Name_Type;
   RPC_Server            : Unit_Name_Type;

   type RE_Id is
     (RE_Null,
      RE_Elaboration,
      RE_Filters,
      RE_Heart,
      RE_Initialize,
      RE_Name_Table,
      RE_No_Tasking,
      RE_Options,
      RE_Priorities,
      RE_Protocols,
      RE_Remote,
      RE_Startup,
      RE_Storages,
      RE_Tasking,
      RE_Types);

   Image : array (RE_Id) of Unit_Name_Type;

   function PCS
     (F : RE_Id;
      N : Name_Id := No_Name;
      L : RE_Id   := RE_Null)
     return Name_Id;

   function PCS
     (F : RE_Id;
      N : Name_Id := No_Name;
      L : RE_Id   := RE_Null)
     return String;

   procedure Add_Protocol
     (First : in out Location_Id;
      Last  : in out Location_Id;
      Name  : Name_Id);

   function Location_List_Image (Location : Location_Id) return Name_Id;
   --  Explore linked list of locations to build its image

   procedure Generate_Elaboration_File (P : Partition_Id);
   --  Create the elaboration unit for the given partition. This unit
   --  overloads the default PCS settings.

   procedure Generate_Executable_File (P : Partition_Id);
   --  Compile main partition file and elaboration file.
   --  Bind and link partition to create executable.

   procedure Generate_Partition_Main_File (P : Partition_Id);
   --  Create a procedure which "withes" all the RCI or SP receivers
   --  of the partition and insert the main procedure if needed.

   procedure Generate_Partition_Project_File
     (D : Directory_Name_Type;
      P : Partition_Id := No_Partition_Id);

   procedure Generate_Protocol_Config_File (P : Partition_Id);
   --  Create protocol configuration file that includes the protocols
   --  required in the GLADE configuration file for this partition.

   procedure Generate_Starter_File;
   --  Create the starter file to launch the other partitions from
   --  main partition subprogram. This can be a shell script or an Ada
   --  program.

   procedure Generate_Storage_Config_File (P : Partition_Id);
   --  Create storage configuration file that includes the storages
   --  required in the GLADE configuration file for this partition.

   procedure Generate_Stamp_File (P : Partition_Id);
   --  Create a stamp file in which the executable file stamp and the
   --  configuration file stamp are stored.

   procedure Generate_Stub (A : ALI_Id);
   --  Create stub and skel for a RCI or SP unit.

   procedure Generate_Skel (A : ALI_Id; P : Partition_Id);
   --  Create skel for a RCI or SP unit and store them in the
   --  directory of the partition on which the unit is assigned.

   function Name (U : Unit_Id) return Name_Id;
   --  Take a unit id and return its name removing unit suffix.

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
     (S1 : String;
      N1 : Name_Id := No_Name;
      S2 : String  := No_Str;
      N2 : Name_Id := No_Name;
      S3 : String  := No_Str;
      N3 : Name_Id := No_Name;
      S4 : String  := No_Str);
   --  Insert a procedure call. The first non-null parameter
   --  is supposed to be the procedure name. The next parameters
   --  are parameters for this procedure call.

   procedure Write_With_Clause (Uname : Name_Id);
   procedure Write_Use_Clause  (Uname : Name_Id);
   --  Add a with clause and possibly a use clause as well.

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
                  Generate_Elaboration_File (J);
                  Generate_Protocol_Config_File (J);
                  Generate_Storage_Config_File (J);
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
           ("Register_Partition_To_Launch",
            Capitalize (Id (Boolean'Image (Use_Rem_Host))),
            Get_Name_String (Remote_Host),
            Quote (To_Absolute_File (Executable) & Current.Command_Line));
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
      Write_Line  ("pragma Warnings (Off);");

      Write_With_Clause (PCS (RE_Filters));
      Write_Use_Clause  (PCS (RE_Filters));
      Write_With_Clause (PCS (RE_Heart));
      Write_Use_Clause  (PCS (RE_Heart));
      Write_With_Clause (PCS (RE_Options));
      Write_Use_Clause  (PCS (RE_Options));
      Write_With_Clause (PCS (RE_Name_Table));
      Write_Use_Clause  (PCS (RE_Name_Table));
      Write_With_Clause (PCS (RE_Remote));
      Write_Use_Clause  (PCS (RE_Remote));
      Write_With_Clause (PCS (RE_Types));
      Write_Use_Clause  (PCS (RE_Types));
      Write_With_Clause (PCS (RE_Priorities));
      Write_Use_Clause  (PCS (RE_Priorities));

      if Current.Tasking = 'N' then
         Write_With_Clause (PCS (RE_No_Tasking));
         Write_Call ("pragma Elaborate_All", PCS (RE_No_Tasking));

      else
         Write_With_Clause (PCS (RE_Tasking));
         Write_Call ("pragma Elaborate_All", PCS (RE_Tasking));
      end if;

      --  Add filtering package if needed

      Filter := Default_Registration_Filter;
      if Present (Filter) then
         Write_With_Clause (PCS (RE_Filters, Filter));
      end if;

      if Present (Current.Filter) then
         Write_With_Clause (PCS (RE_Filters, Current.Filter));
      end if;

      if Current.First_Channel /= No_Channel_Id then
         Channel := Current.First_Channel;
         while Channel /= No_Channel_Id loop
            Filter := Channels.Table (Channel).Filter;
            if Present (Filter) then
               Write_With_Clause (PCS (RE_Filters, Filter));
            end if;
            if Channels.Table (Channel).Lower.My_Partition = P then
               Channel := Channels.Table (Channel).Lower.Next_Channel;
            else
               Channel := Channels.Table (Channel).Upper.Next_Channel;
            end if;
         end loop;
      end if;

      Write_Str  ("package body ");
      Write_Name (PCS (RE_Elaboration));
      Write_Line (" is");

      Increment_Indentation;
      Write_Indentation;
      Write_Line ("procedure Initialize is");
      Write_Indentation;
      Write_Line ("begin");

      Increment_Indentation;
      if Current.Priority /= No_Priority then
         Write_Call ("Set_RPC_Handler_Priority", No_Name,
                     Current.Priority'Img);
      end if;

      if Default_Priority_Policy /= No_Priority_Policy then
         Write_Call ("Set_RPC_Handler_Priority_Policy",
                     Priority_Policy_Img (Default_Priority_Policy));
      end if;

      Write_Call ("Set_Rsh_Command", Quote (Get_Rsh_Command));
      Write_Call ("Set_Rsh_Options", Quote (Get_Rsh_Options));

      --  If the partition holds the main unit, then it cannot be slave.
      --  Otherwise, it is.

      if P /= Main_Partition then
         Write_Call ("Set_Slave", Id ("True"));
      else
         Write_Call ("Set_Slave", Id ("False"));
      end if;

      --  How should the partition terminate. Note that
      --  Global_Termination is the default. No need to force the
      --  default.

      if Current.Termination /= No_Termination then
         Write_Call ("Set_Termination",
                     Termination_Img (Current.Termination));
      end if;

      --  When this partition is restarted, how should we handle
      --  reconnections?

      if Current.Reconnection /= No_Reconnection then
         Write_Call ("Set_Reconnection",
                     Reconnection_Img (Current.Reconnection));
      end if;

      --  If a protocol has been specified, then use it (with its data
      --  if present).

      if Default_First_Boot_Location /= No_Location_Id then
         Write_Call ("Set_Boot_Location",
                     Location_List_Image (Default_First_Boot_Location));
      end if;

      --  Compute the self location string (eventually composed of
      --  several locations separated by commas).

      if Current.First_Network_Loc /= No_Location_Id then
         Write_Call ("Set_Self_Location",
                     Location_List_Image (Current.First_Network_Loc));
      end if;

      --  Compute the data location string (eventually composed of
      --  several locations separated by commas).

      if Current.Storage_Loc /= No_Location_Id then
         Write_Call ("Set_Data_Location",
                     Location_List_Image (Current.Storage_Loc));
      end if;

      --  If we have no Ada starter (None or Shell), then it is equivalent
      --  to having --nolaunch on the command line.

      if Default_Starter /= Ada_Import then
         Write_Call ("Set_Nolaunch", No_Name, "True");
      end if;

      --  Do we want to control the number of anonymous tasks

      if Current.Task_Pool /= No_Task_Pool then
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Int'Image (Current.Task_Pool (1)));
         Add_Char_To_Name_Buffer (',');
         Add_Str_To_Name_Buffer (Int'Image (Current.Task_Pool (2)));
         Add_Char_To_Name_Buffer (',');
         Add_Str_To_Name_Buffer (Int'Image (Current.Task_Pool (3)));
         Write_Call ("Set_Task_Pool_Bounds", Id (Name_Buffer (2 .. Name_Len)));
      end if;

      --  Set tasking policy and with appropriate package

      if Current.Tasking = 'N' then
         Write_Call (PCS (RE_No_Tasking, Image (RE_Initialize)));
         Write_Call ("Set_Light_PCS", Id ("True"));

      elsif Current.Tasking = 'U' then
         Write_Call (PCS (RE_Tasking, Image (RE_Initialize)));
         Write_Call ("Set_Pure_Client", Id ("True"));

      else  --  Get_Tasking (P) = 'P'
         Write_Call (PCS (RE_Tasking, Image (RE_Initialize)));
      end if;

      --  Elaborate Name_Table

      Write_Call (PCS (RE_Name_Table, Image (RE_Initialize)));

      Write_Call ("Set_Partition_Name", Quote (Current.Name));

      --  Set registration filter, default filter and partition filters

      Filter := Default_Registration_Filter;
      if Present (Filter) then
         Write_Call ("Set_Registration_Filter", Quote (Filter));
      end if;

      Filter := Partitions.Table (Default_Partition_Id).Filter;
      if Present (Filter) then
         Write_Call ("Set_Default_Filter", Quote (Filter));
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
            Write_Call ("Set_Channel_Filter",
                        Quote (Partitions.Table (Peer).Name), No_Str,
                        Quote (Filter));
         end if;
      end loop;

      --  Initialize filters

      Filter := Default_Registration_Filter;
      if Present (Filter) then
         Write_Call (PCS (RE_Filters, Filter, RE_Initialize));
      end if;

      if Present (Current.Filter) then
         Write_Call (PCS (RE_Filters, Current.Filter, RE_Initialize));
      end if;

      Channel := Current.First_Channel;
      while Channel /= No_Channel_Id loop
         Filter := Channels.Table (Channel).Filter;

         if Present (Filter) then
            Write_Call (PCS (RE_Filters, Filter, RE_Initialize));
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
      Write_Name (PCS (RE_Elaboration));
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
      Directory  : Directory_Name_Type;
      I_Part_Dir : String_Access;
      Comp_Args  : String_List (1 .. 6);
      Make_Args  : String_List (1 .. 8);
      Sfile      : File_Name_Type;
      Prj_Fname  : File_Name_Type;

   begin
      Executable := Current.Executable_File;
      Directory  := Current.Partition_Dir;

      Name_Len := 2;
      Name_Buffer (1) := '-';
      Name_Buffer (2) := 'I';
      Get_Name_String_And_Append (Directory);
      I_Part_Dir := new String'(Name_Buffer (1 .. Name_Len));

      Comp_Args (1) := E_Current_Dir;
      Comp_Args (2) := I_Part_Dir;
      Comp_Args (3) := I_Stub_Dir;
      Comp_Args (4) := I_Current_Dir;

      if Project_File_Name = null then
         Comp_Args (5) := Object_Dir_Flag;
         Comp_Args (6) := new String'(Get_Name_String (Directory));

      else
         Comp_Args (5) := Project_File_Flag;
         Prj_Fname := Dir (Directory, Part_Prj_File_Name);
         Get_Name_String (Prj_Fname);
         Comp_Args (6) := new String'(Name_Buffer (1 .. Name_Len));
      end if;

      Sfile := Elaboration_File & ADB_Suffix_Id;
      if Project_File_Name = null then
         Sfile := Dir (Directory, Sfile);
      end if;
      Compile (Sfile, Comp_Args);

      Sfile := Protocol_Config_File & ADB_Suffix_Id;
      if Is_Regular_File (Dir (Directory, Sfile)) then
         if Project_File_Name = null then
            Sfile := Dir (Directory, Sfile);
         end if;
         Compile (Sfile, Comp_Args);
      end if;

      Sfile := Storage_Config_File & ADB_Suffix_Id;
      if Is_Regular_File (Dir (Directory, Sfile)) then
         if Project_File_Name = null then
            Sfile := Dir (Directory, Sfile);
         end if;
         Compile (Sfile, Comp_Args);
      end if;

      Sfile := Partition_Main_File & ADB_Suffix_Id;
      if Project_File_Name = null then
         Sfile := Dir (Directory, Sfile);
      end if;
      Compile (Sfile, Comp_Args);

      Free (Comp_Args (6));

      Make_Args (1) := E_Current_Dir;
      Make_Args (2) := I_Part_Dir;
      Make_Args (3) := I_Stub_Dir;
      Make_Args (4) := I_Current_Dir;
      Make_Args (5) := Bind_Only_Flag;
      Make_Args (6) := Link_Only_Flag;

      if Project_File_Name = null then
         Make_Args (7) := Output_Flag;
         Get_Name_String (Executable);
         Make_Args (8) := new String'(Name_Buffer (1 .. Name_Len));

      else
         Make_Args (7) := Project_File_Flag;
         Prj_Fname := Dir (Directory, Part_Prj_File_Name);
         Get_Name_String (Prj_Fname);
         Make_Args (8) := new String'(Name_Buffer (1 .. Name_Len));
      end if;

      Build (Sfile, Make_Args);

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
      Filename := Partition_Main_File & ADB_Suffix_Id;
      Filename := Dir (Current.Partition_Dir, Filename);
      Create_File (File, Filename);
      Set_Output  (File);
      Write_Line  ("pragma Warnings (Off);");

      Write_With_Clause (Partition_Interface);
      Write_Use_Clause  (Partition_Interface);
      Write_With_Clause (RPC);
      if Current.Tasking /= 'N' then
         Write_With_Clause (RPC_Server);
      end if;
      Write_With_Clause (PCS (RE_Startup));
      Write_Call        ("pragma Elaborate_All", PCS (RE_Startup));

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
      Write_Line (" : System.RPC.Partition_ID;");

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

            Write_Call ("Register_Passive_Partition",
                        Variable, No_Str,
                        Quote (Partitions.Table (J).Name),
                        Get_Name_String (Location_List_Image (Location)));

            Conf_Unit := Partitions.Table (J).First_Unit;
            while Conf_Unit /= No_Conf_Unit_Id loop
               Write_Call ("Register_Passive_Package_On_Passive_Partition",
                           Variable, No_Str,
                           Quote (Conf_Units.Table (Conf_Unit).Name), No_Str,
                           Conf_Units.Table (Conf_Unit).Name & "'Version");
               Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
            end loop;

            Conf_Unit := Partitions.Table (J).First_Unit;
            if Conf_Unit /= No_Conf_Unit_Id then
               Write_Call ("Elaborate_Passive_Partition", Variable);
            end if;
         end if;
      end loop;

      --  Register shared passive packages since they have no
      --  elaboration code.

      Conf_Unit := Current.First_Unit;
      while Conf_Unit /= No_Conf_Unit_Id loop
         Unit := Conf_Units.Table (Conf_Unit).My_Unit;
         if Units.Table (Unit).Shared_Passive then
            Write_Call ("Register_Passive_Package",
                        Quote (Conf_Units.Table (Conf_Unit).Name),
                        Get_Name_String (Name (Unit)) & "'Version");
         end if;
         Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
      end loop;

      --  Check version consistency of RCI and SP stubs

      if Default_Version_Check then
         for J in Current.First_Stub .. Current.Last_Stub loop
            Unit := ALIs.Table (Get_ALI_Id (Stubs.Table (J))).Last_Unit;
            Write_Call ("Check",
                        Quote (Stubs.Table (J)), No_Str,
                        Stubs.Table (J) & "'Version",
                        Units.Table (Unit).RCI'Img);
         end loop;
      end if;

      --   Invoke main subprogram thorugh Run routine

      if Present (Current.Main_Subprogram) then
         Get_Name_String (Current.Main_Subprogram);
         Add_Str_To_Name_Buffer ("'Access");
         Write_Call ("Run", Name_Find);

      else
         Write_Call ("Run");
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

   -----------------------------------
   -- Generate_Protocol_Config_File --
   -----------------------------------

   procedure Generate_Protocol_Config_File (P : Partition_Id) is
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

      Light_PCS := Current.Tasking = 'N'
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
      Create_File (File, Filename);
      Set_Output  (File);
      Write_Line  ("pragma Warnings (Off);");

      --  Withed location protocols

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
         Major := PCS (RE_Protocols, Locations.Table (Location).Major);
         Write_With_Clause (Major);
         Write_Call ("pragma Elaborate_All", Major);
         if Current.Tasking /= 'N' then
            Major := Major & ".Server";
            Write_With_Clause (Major);
            Write_Call ("pragma Elaborate_All", Major);
         end if;
         Location := Locations.Table (Location).Next_Location;
      end loop;

      Write_Str  ("package body ");
      Write_Name (PCS (RE_Protocols, Config));
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
         Major := PCS (RE_Protocols, Locations.Table (Location).Major);
         Write_Call ("Register", Major & ".Create");
         Location := Locations.Table (Location).Next_Location;
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Initialize;");
      Decrement_Indentation;
      Write_Eol;
      Write_Str  ("end ");
      Write_Name (PCS (RE_Protocols, Config));
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Protocol_Config_File;

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

         Compile (Full_Unit_File, Arguments);

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

   ----------------------------------
   -- Generate_Storage_Config_File --
   ----------------------------------

   procedure Generate_Storage_Config_File (P : Partition_Id) is
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
            if File = Invalid_FD then
               Create_File (File, Filename);
               Set_Output  (File);
               Write_Line  ("pragma Warnings (Off);");
            end if;

            Partition := Get_Partition_Id (Uname);
            Location  := Partitions.Table (Partition).Storage_Loc;

            if Location /= No_Location_Id then
               Major := PCS (RE_Storages, Locations.Table (Location).Major);
               Write_With_Clause (Major);
               Write_Call ("pragma Elaborate_All", Major);

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
            if File = Invalid_FD then
               Create_File (File, Filename);
               Set_Output  (File);
               Write_Line  ("pragma Warnings (Off);");
            end if;

            Location := Current.Storage_Loc;
            if Location /= No_Location_Id then
               Major := PCS (RE_Storages, Locations.Table (Location).Major);
               Write_With_Clause (Major);
               Write_Call ("pragma Elaborate_All", Major);
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
         Major := PCS (RE_Storages, Locations.Table (Location).Major);
         Write_With_Clause (Major);
         Write_Call ("pragma Elaborate_All", Major);
      end if;

      if File = Invalid_FD then
         return;
      end if;

      --  Initialize storage supports

      Write_Str  ("package body ");
      Write_Name (PCS (RE_Storages, Config));
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
               Write_Call (PCS (RE_Storages, Major, RE_Initialize));
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
               Write_Call (PCS (RE_Storages, Major, RE_Initialize));
            end if;
         end if;

         Conf_Unit := Conf_Units.Table (Conf_Unit).Next_Unit;
      end loop;

      if Use_Default then
         Major := Locations.Table (Default_Data_Location).Major;
         Write_Call (PCS (RE_Storages, Major, RE_Initialize));
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Initialize;");
      Decrement_Indentation;
      Write_Eol;
      Write_Str  ("end ");
      Write_Name (PCS (RE_Storages, Config));
      Write_Line (";");

      Close (File);
      Set_Standard_Output;
   end Generate_Storage_Config_File;

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

         Compile (Full_Unit_File, Arguments, False);

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
   begin
      PCS_Name             := Id (Get_PCS_Name);
      Build_Stamp_File     := Id ("glade.sta");
      Elaboration_File     := Id ("s-garela");
      Partition_Main_File  := Id ("partition");
      Protocol_Config_File := Id ("s-gaprco");
      Storage_Config_File  := Id ("s-gastco");

      Partition_Main_Name  := Id ("Partition");
      Config               := Id ("Config");
      Partition_Interface  := Id ("System.Partition_Interface");
      RPC                  := Id ("System.RPC");
      RPC_Server           := Id ("System.RPC.Server");

      for E in Image'Range loop
         Set_Str_To_Name_Buffer (E'Img);
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         Image (E) := Name_Find;
      end loop;
   end Initialize;

   -------------------------
   -- Location_List_Image --
   -------------------------

   function Location_List_Image
     (Location : Location_Id)
     return Name_Id
   is
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

   ----------
   -- Name --
   ----------

   function Name (U : Unit_Id) return Name_Id is
   begin
      return Name (Units.Table (U).Uname);
   end Name;

   ---------
   -- PCS --
   ---------

   function PCS
     (F : RE_Id;
      N : Name_Id := No_Name;
      L : RE_Id   := RE_Null)
     return Name_Id is
   begin
      Get_Name_String (PCS_Name);
      Add_Char_To_Name_Buffer ('.');
      Get_Name_String_And_Append (Image (F));
      if Present (N) then
         Add_Char_To_Name_Buffer ('.');
         Get_Name_String_And_Append (N);
         if L /= RE_Null then
            Add_Char_To_Name_Buffer ('.');
            Get_Name_String_And_Append (Image (L));
         end if;
      end if;
      return Capitalize (Name_Find);
   end PCS;

   function PCS
     (F : RE_Id;
      N : Name_Id := No_Name;
      L : RE_Id   := RE_Null)
     return String is
   begin
      return Get_Name_String (PCS (F, N, L));
   end PCS;

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

   ----------------
   -- Write_Call --
   ----------------

   procedure Write_Call
     (S1 : String;
      N1 : Name_Id := No_Name;
      S2 : String  := No_Str;
      N2 : Name_Id := No_Name;
      S3 : String  := No_Str;
      N3 : Name_Id := No_Name;
      S4 : String  := No_Str)
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
         if N_Params = 2 then
            Write_Eol;
            Increment_Indentation;
            Write_Indentation (-1);
            Write_Str ("(");
         elsif N_Params > 2 then
            Write_Str (",");
            Write_Eol;
            Write_Indentation;
         end if;
      end Write_Separator;

   begin
      Write_Indentation;
      if S1 /= No_Str then
         Write_Separator;
         Write_Parameter (S1);
      end if;
      if Present (N1) then
         Write_Separator;
         Write_Parameter (Get_Name_String (N1));
      end if;
      if S2 /= No_Str then
         Write_Separator;
         Write_Parameter (S2);
      end if;
      if Present (N2) then
         Write_Separator;
         Write_Parameter (Get_Name_String (N2));
      end if;
      if S3 /= No_Str then
         Write_Separator;
         Write_Parameter (S3);
      end if;
      if Present (N3) then
         Write_Separator;
         Write_Parameter (Get_Name_String (N3));
      end if;
      if S4 /= No_Str then
         Write_Separator;
         Write_Parameter (S4);
      end if;
      pragma Assert (N_Params /= 0);
      if N_Params /= 1 then
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

   ----------------------
   -- Write_Use_Clause --
   ----------------------

   procedure Write_Use_Clause (Uname : Name_Id) is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("use  ");
      Get_Name_String_And_Append (Uname);
      Add_Char_To_Name_Buffer (';');
      Write_Str (Name_Buffer (1 .. Name_Len));
      Write_Eol;
   end Write_Use_Clause;

   ------------------------
   -- Write_With_Clause --
   ------------------------

   procedure Write_With_Clause (Uname : Name_Id) is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer ("with ");
      Get_Name_String_And_Append (Uname);
      Add_Char_To_Name_Buffer (';');
      Write_Str (Name_Buffer (1 .. Name_Len));
      Write_Eol;
   end Write_With_Clause;

end XE_Back;
