-----------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                               X E _ S E M                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2006 Free Software Foundation, Inc.           --
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

with GNAT.Table;

with XE;               use XE;
with XE_Back;          use XE_Back;
with XE_Front;         use XE_Front;
with XE_Flags;         use XE_Flags;
with XE_IO;            use XE_IO;
with XE_List;          use XE_List;
with XE_Names;         use XE_Names;
with XE_Types;         use XE_Types;
with XE_Units;         use XE_Units;
with XE_Utils;         use XE_Utils;

package body XE_Sem is

   package Files is new GNAT.Table
     (Table_Component_Type => File_Name_Type,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100);

   procedure Apply_Default_Channel_Attributes (Channel : Channel_Id);
   --  When a channel attribute has not been assigned, apply the
   --  default channel attribute.

   procedure Apply_Default_Partition_Attributes (Partition : Partition_Id);
   --  When a partition attribute has not been assigned, apply the
   --  default partition attribute.

   procedure Assign_Unit_Tasking (ALI : ALI_Id);
   --  Assign PCS tasking for a RCI unit.

   procedure Assign_Partition_Termination (Partition : Partition_Id);
   --  Assign termination policy based on tasking policy.

   procedure Update_Partition_Tasking
     (ALI : ALI_Id; Partition : Partition_Id);
   --  Update partition tasking with ALI tasking as well as all its
   --  withed and collocated units.

   procedure Detect_Configured_Channel_Duplication
     (Channel : Channel_Id;
      Success : in out Boolean);
   --  Detect when two channels are defined designating the same
   --  partition pair. This may be incorrect as the configuration of
   --  the two channels may be inconsistent.

   procedure Detect_Empty_Partition
     (Partition : Partition_Id;
      Success   : in out Boolean);
   --  Detect empty partition since they cannot register to the
   --  distributed system.

   procedure Detect_Incorrect_Main_Subprogram
     (Partition : Partition_Id;
      Success   : in out Boolean);
   --  Detect that the configured unit used as main subprogram is
   --  really a main subprogram from the Ada point of view.

   procedure Detect_Mal_Formed_Location
     (Location : Location_Id;
      Success  : in out Boolean);
   --  Detecte that the major location is not missing.

   procedure Detect_Multiply_Assigned_Conf_Unit
     (Conf_Unit : Conf_Unit_Id;
      Success   : in out Boolean);
   --  RCI and SP units cannot be multiply assigned. They have to be
   --  unique in the global distributed system. No replication yet.

   procedure Detect_Non_Ada_Conf_Unit
     (Conf_Unit : Conf_Unit_Id;
      Success   : in out Boolean);
   --  Detect that configured units do really designate Ada units.

   procedure Detect_Non_Collocated_Categorized_Child_Unit
     (Conf_Unit : Conf_Unit_Id;
      Success   : in out Boolean);
   --  When two RCI or SP units are child and parent, they have to be
   --  collocated as they have visibility on their private parts.

   procedure Detect_Non_Configured_Categorized_Unit
     (ALI     : ALI_Id;
      Success : in out Boolean);
   --  A RCI or SP unit has to be configured on one partition. This
   --  rule is applied only when we produce the global distributed
   --  system. If a unit is not configured then we already know that
   --  it is erroneous.

   procedure Find_Stubs_And_Stamps_From_Closure (Partition : Partition_Id);
   --  Explore the partition transitive closure to compute the list of
   --  units for which we need only their stubs (RCI and SP that are
   --  not configured on this partition). We need this list to check
   --  the version consistency between stubs and skels. In the same
   --  time, compute the most recent file time stamp to see whether we
   --  need to update the partition executable file.

   -------------
   -- Analyze --
   -------------

   procedure Analyze is
      A  : ALI_Id;
      CU : Conf_Unit_Id;
      OK : Boolean := True;
      P  : Partition_Id;

   begin
      --  Add units configured on the partition type to each
      --  partition (for instance, main subprogram).

      CU := Partitions.Table (Default_Partition_Id).First_Unit;
      while CU /= No_Conf_Unit_Id loop
         for J in Partitions.First + 1 .. Partitions.Last loop
            Add_Conf_Unit (Conf_Units.Table (CU).Name, J);
         end loop;
         CU := Conf_Units.Table (CU).Next_Unit;
      end loop;

      --  PCS may require to configure one of its units on the main
      --  partition.

      if PCS_Conf_Unit /= No_Name then
         Add_Conf_Unit (PCS_Conf_Unit, Main_Partition);
      end if;

      XE_List.Initialize;
      Main_Subprogram := Partitions.Table (Main_Partition).Main_Subprogram;
      if Partitions.Table (Main_Partition).To_Build then
         Register_Unit_To_Load (Main_Subprogram);
      end if;
      for U in Conf_Units.First .. Conf_Units.Last loop
         if To_Build (U) then
            Register_Unit_To_Load (Conf_Units.Table (U).Name);
         end if;
      end loop;
      Load_All_Registered_Units;

      ----------------------------
      -- Use of Name Table Info --
      ----------------------------

      --  All unit names and file names are entered into the Names
      --  table. The Info and Byte fields of these entries are used as
      --  follows:
      --
      --    Unit name           Info field has Unit_Id
      --    Conf. unit name     Info field has ALI_Id
      --                        Byte fiels has Partition_Id (*)
      --    ALI file name       Info field has ALI_Id
      --    Source file name    Info field has Unit_Id
      --
      --  (*) A (normal, RT) unit may be assigned to several partitions.

      --  We want to detect whether these configured units are real
      --  ada units. Set the configured unit name to No_ALI_Id. When
      --  we load an ali file, its unit name is set to its ali id. If
      --  a configured unit name has no ali id, it is not an Ada unit.
      --  Assign byte field of configured unit name to No_Partition_Id
      --  in order to detect units that are multiply assigned.

      for J in Conf_Units.First .. Conf_Units.Last loop
         Set_ALI_Id       (Conf_Units.Table (J).Name, No_ALI_Id);
         Set_Partition_Id (Conf_Units.Table (J).Name, No_Partition_Id);
      end loop;

      --  Set name table info of conf. unit name (%s or %b removed) to
      --  ALI id. Set use of tasking to unknown.

      for J in ALIs.First .. ALIs.Last loop
         Set_ALI_Id (ALIs.Table (J).Uname, J);
      end loop;

      if not Quiet_Mode then
         Message ("checking configuration consistency");
      end if;

      if Debug_Mode then
         Message ("detect non Ada configured units");
         Message ("detect (RCI/SP) multiply assigned configured units");
      end if;

      for J in Conf_Units.First .. Conf_Units.Last loop
         if Partitions.Table (Conf_Units.Table (J).Partition).To_Build then
            Detect_Non_Ada_Conf_Unit (J, OK);
            Detect_Multiply_Assigned_Conf_Unit (J, OK);
         end if;
      end loop;

      if Partitions.Table (Default_Partition_Id).To_Build then
         if Debug_Mode then
            Message ("detect non configured (RCI/SP) categorized units");
         end if;

         for J in ALIs.First .. ALIs.Last loop
            Detect_Non_Configured_Categorized_Unit (J, OK);
         end loop;
      end if;

      if Debug_Mode then
         Message ("detect non collocated (RCI/SP) child and parents");
      end if;

      for J in Conf_Units.First .. Conf_Units.Last loop
         if To_Build (J) then
            Detect_Non_Collocated_Categorized_Child_Unit (J, OK);
         end if;
      end loop;

      if Debug_Mode then
         Message ("detect empty partitions");
         Message ("detect incorrect main subprograms");
      end if;

      for J in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (J).To_Build then
            Detect_Empty_Partition (J, OK);
            Detect_Incorrect_Main_Subprogram (J, OK);
         end if;
      end loop;

      if Debug_Mode then
         Message ("detect channel duplications");
      end if;

      for J in Channels.First + 1 .. Channels.Last loop
         Detect_Configured_Channel_Duplication (J, OK);
      end loop;

      if Debug_Mode then
         Message ("detect mal formed locations");
      end if;

      for J in Locations.First .. Locations.Last loop
         Detect_Mal_Formed_Location (J, OK);
      end loop;

      if not OK then
         raise Partitioning_Error;
      end if;

      for J in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (J).To_Build then
            Apply_Default_Partition_Attributes (J);
         end if;
      end loop;

      for J in Channels.First + 1 .. Channels.Last loop
         Apply_Default_Channel_Attributes (J);
      end loop;

      if not Quiet_Mode then
         Show_Configuration;
      end if;

      if Debug_Mode then
         Message ("look for units dragging tasking");
      end if;

      --  This step checks whether we need tasking on a partition.
      --
      --  Notation:
      --     '?' : unknown
      --     'P' : tasking is required because of PCS code
      --     'U' : tasking is required because of user code
      --     'N' : tasking is not required for this unit
      --
      --  Check whether a unit comes with tasking. Possibly because of
      --  its dependencies. Note that we do not look any further a
      --  dependency designating a RCI unit since it may not be
      --  collocated with the initial unit. Check also whether this
      --  unit is RCI and or has RACW as such a unit requires tasking
      --  from the PCS. Check whether the partition is candidate for a
      --  local termination.

      for J in ALIs.First .. ALIs.Last loop
         Set_Partition_Id (ALIs.Table (J).Uname, No_Partition_Id);
         Assign_Unit_Tasking (J);
      end loop;

      for J in Conf_Units.First .. Conf_Units.Last loop
         A := Conf_Units.Table (J).My_ALI;
         P := Conf_Units.Table (J).Partition;
         Set_Partition_Id (Conf_Units.Table (J).Name, P);

         if To_Build (J) then
            Update_Partition_Tasking (A, P);
         end if;
      end loop;
      Partitions.Table (Main_Partition).Tasking := 'P';

      if Debug_Mode then
         Message ("configure partition termination");
         Message ("find partition stub-only units");
         Message ("update partition most recent stamp");
      end if;

      for J in Partitions.First + 1 .. Partitions.Last loop
         if Partitions.Table (J).To_Build then
            Assign_Partition_Termination (J);
            Find_Stubs_And_Stamps_From_Closure (J);
         end if;
      end loop;
   end Analyze;

   --------------------------------------
   -- Apply_Default_Channel_Attributes --
   --------------------------------------

   procedure Apply_Default_Channel_Attributes (Channel : Channel_Id) is
      Current : Channel_Type renames Channels.Table (Channel);
      Default : Channel_Type renames Channels.Table (Default_Channel_Id);

   begin
      if No (Current.Filter) then
         Current.Filter := Default.Filter;
      end if;
   end Apply_Default_Channel_Attributes;

   ----------------------------------------
   -- Apply_Default_Partition_Attributes --
   ----------------------------------------

   procedure Apply_Default_Partition_Attributes (Partition : Partition_Id) is
      Current : Partition_Type renames Partitions.Table (Partition);
      Default : Partition_Type renames Partitions.Table (Default_Partition_Id);

   begin
      Current.Partition_Dir := Dir (Configuration, Current.Name);
      Current.Partition_Dir := Dir (Id (Root), Current.Partition_Dir);

      if No (Current.Command_Line) then
         Current.Command_Line := Default.Command_Line;
      end if;

      if No (Current.Executable_Dir) then
         Current.Executable_Dir := Default.Executable_Dir;
      end if;

      Current.Executable_File := Current.Name & Exe_Suffix_Id;
      if Present (Current.Executable_Dir) then
         Current.Executable_File :=
           Dir (Current.Executable_Dir, Current.Executable_File);
      end if;

      if No (Current.Filter) then
         Current.Filter := Default.Filter;
      end if;

      if No (Current.Main_Subprogram) then
         Current.Main_Subprogram := Default.Main_Subprogram;
      end if;

      if Current.Host = No_Host_Id then
         Current.Host := Default.Host;
      end if;

      if Current.Light_PCS = BMaybe then
         Current.Light_PCS := Default.Light_PCS;
      end if;
      if Current.Light_PCS = BFalse then
         Current.Tasking := 'P';
      end if;

      if Current.Passive = BMaybe then
         Current.Passive := Default.Passive;
      end if;

      if Current.Priority = No_Priority then
         Current.Priority := Default.Priority;
      end if;

      if Current.First_Network_Loc = No_Location_Id then
         Current.First_Network_Loc := Default.First_Network_Loc;
         Current.Last_Network_Loc  := Default.Last_Network_Loc;
      end if;

      if Current.Reconnection = No_Reconnection then
         Current.Reconnection := Default.Reconnection;
      end if;

      if Current.Storage_Loc = No_Location_Id then
         Current.Storage_Loc := Default.Storage_Loc;
      end if;

      if Current.Task_Pool = No_Task_Pool then
         Current.Task_Pool := Default.Task_Pool;
      end if;

      if Current.Termination = No_Termination then
         Current.Termination := Default.Termination;
      end if;
   end Apply_Default_Partition_Attributes;

   ----------------------------------
   -- Assign_Partition_Termination --
   ----------------------------------

   procedure Assign_Partition_Termination (Partition : Partition_Id) is
      Current : Partition_Type renames Partitions.Table (Partition);

   begin
      if Debug_Mode then
         Message ("partition", Current.Name, "has tasking " & Current.Tasking);
      end if;

      if Current.Termination = No_Termination
        and then Current.Tasking /= 'P'
      then
         Current.Termination := Local_Termination;

         if Debug_Mode then
            Message ("local termination forced for", Current.Name);
         end if;
      end if;
   end Assign_Partition_Termination;

   -------------------------
   -- Assign_Unit_Tasking --
   -------------------------

   procedure Assign_Unit_Tasking (ALI : ALI_Id) is
      T : Character := Get_Tasking (ALI);

   begin
      for J in ALIs.Table (ALI).First_Unit .. ALIs.Table (ALI).Last_Unit loop

         --  No need to investigate further when the unit is a RCI
         --  unit or has RACW objects.

         if Units.Table (J).RCI
           or else Units.Table (J).Has_RACW
         then
            T := 'P';
            exit;
         end if;
      end loop;

      if T = '?' then
         T := 'N';
      end if;
      Set_Tasking (ALI, T);

      if Debug_Mode then
         Message ("unit", ALIs.Table (ALI).Uname,
                  "has tasking " & Get_Tasking (ALI));
      end if;
   end Assign_Unit_Tasking;

   -------------------------------------------
   -- Detect_Configured_Channel_Duplication --
   -------------------------------------------

   procedure Detect_Configured_Channel_Duplication
     (Channel : Channel_Id;
      Success : in out Boolean)
   is
      N  : Name_Id := Channels.Table (Channel).Name;
      LP : Partition_Id renames Channels.Table (Channel).Lower.My_Partition;
      UP : Partition_Id renames Channels.Table (Channel).Upper.My_Partition;
      C  : Channel_Id;

   begin
      if UP = LP then
         Message ("channel", Quote (N), "is an illegal pair of partitions");
         Success := False;
      end if;

      Get_Name_String (Partitions.Table (UP).Name);
      Add_Char_To_Name_Buffer ('#');
      Get_Name_String_And_Append (Partitions.Table (UP).Name);
      N := Name_Find;
      C := Get_Channel_Id (N);

      if C /= No_Channel_Id then
         Message ("channels", Quote (N), "and",
                  Quote (Channels.Table (C).Name),
                  "designate the same pair");
         Success := False;
      end if;

      Set_Channel_Id (N, Channel);
   end Detect_Configured_Channel_Duplication;

   ----------------------------
   -- Detect_Empty_Partition --
   ----------------------------

   procedure Detect_Empty_Partition
     (Partition : Partition_Id;
      Success   : in out Boolean)
   is
      N : constant Name_Id := Partitions.Table (Partition).Name;

   begin
      --  We cannot have an empty partition

      if Partitions.Table (Partition).First_Unit = No_Conf_Unit_Id then
         Message ("partition", Quote (N), "is empty");
         Success := False;
      end if;
   end Detect_Empty_Partition;

   --------------------------------------
   -- Detect_Incorrect_Main_Subprogram --
   --------------------------------------

   procedure Detect_Incorrect_Main_Subprogram
     (Partition : Partition_Id;
      Success   : in out Boolean)
   is
      N : constant Unit_Name_Type :=
        Partitions.Table (Partition).Main_Subprogram;
      A : ALI_Id;

   begin
      if No (N) then
         return;
      end if;

      A := Get_ALI_Id (N);
      if A = No_ALI_Id
        or else ALIs.Table (A).Main_Program = None
      then
         Message ("", Quote (N), "is not a main program");
         Success := False;
      end if;
   end Detect_Incorrect_Main_Subprogram;

   --------------------------------
   -- Detect_Mal_Formed_Location --
   --------------------------------

   procedure Detect_Mal_Formed_Location
     (Location : Location_Id;
      Success  : in out Boolean) is
   begin
      Get_Name_String (Locations.Table (Location).Major);
      if Name_Len = 0 then
         Add_Str_To_Name_Buffer ("://");
         if Present (Locations.Table (Location).Minor) then
            Get_Name_String_And_Append (Locations.Table (Location).Minor);
         end if;
         Message ("missing location name in", Quote (Name_Find));
         Success := False;
      end if;
   end Detect_Mal_Formed_Location;

   ----------------------------------------
   -- Detect_Multiply_Assigned_Conf_Unit --
   ----------------------------------------

   procedure Detect_Multiply_Assigned_Conf_Unit
     (Conf_Unit : Conf_Unit_Id;
      Success   : in out Boolean)
   is
      N : constant Name_Id := Conf_Units.Table (Conf_Unit).Name;
      A : constant ALI_Id  := Get_ALI_Id (N);
      U : Unit_Id;

   begin
      if A = No_ALI_Id then
         return;
      end if;

      --  The last unit is always the spec when there is a spec.

      U := ALIs.Table (A).Last_Unit;
      Conf_Units.Table (Conf_Unit).My_Unit := U;
      Conf_Units.Table (Conf_Unit).My_ALI  := A;

      if Units.Table (U).Is_Generic then
         Message ("generic unit", Quote (N),
                  "cannot be assigned to a partition");
         Success := False;

      elsif Units.Table (U).RCI or else Units.Table (U).Shared_Passive then

         --  If null, we have not yet assigned this rci or sp unit
         --  name to a partition.

         if Get_Partition_Id (N) /= No_Partition_Id then
            if Units.Table (U).RCI then
               Message ("RCI Ada unit", Quote (N), "has been assigned twice");
            else
               Message ("SP Ada unit", Quote (N), "has been assigned twice");
            end if;
            Success := False;
         end if;

         --  Assign unit to partition in order not to assign it twice
         --  as this unit is a RCI or a SP package.

         Set_Partition_Id (N, Conf_Units.Table (Conf_Unit).Partition);
      end if;
   end Detect_Multiply_Assigned_Conf_Unit;

   ------------------------------
   -- Detect_Non_Ada_Conf_Unit --
   ------------------------------

   procedure Detect_Non_Ada_Conf_Unit
     (Conf_Unit : Conf_Unit_Id;
      Success   : in out Boolean)
   is
      N : constant Unit_Name_Type := Conf_Units.Table (Conf_Unit).Name;
      A : constant ALI_Id         := Get_ALI_Id (N);

   begin
      --  There is no ali file associated to this configured
      --  unit. The configured unit is not an Ada unit.

      if A = No_ALI_Id then
         Message ("configured unit", Quote (N), "is not an Ada unit");
         Success := False;
      end if;
   end Detect_Non_Ada_Conf_Unit;

   --------------------------------------------------
   -- Detect_Non_Collocated_Categorized_Child_Unit --
   --------------------------------------------------

   procedure Detect_Non_Collocated_Categorized_Child_Unit
     (Conf_Unit : Conf_Unit_Id;
      Success   : in out Boolean)
   is
      P : constant Partition_Id   := Conf_Units.Table (Conf_Unit).Partition;
      U : Unit_Id                 := Conf_Units.Table (Conf_Unit).My_Unit;
      X : constant Unit_Name_Type := Conf_Units.Table (Conf_Unit).Name;
      N : Unit_Name_Type          := X;
      A : ALI_Id;

   begin
      if not Units.Table (U).RCI
        and then not Units.Table (U).Shared_Passive
      then
         return;
      end if;

      Get_Name_String (N);

      --  Find a parent

      while Name_Len > 0
        and then Name_Buffer (Name_Len) /= '.'
      loop
         Name_Len := Name_Len - 1;
      end loop;

      --  There is a parent

      if Name_Len > 1 then
         Name_Len := Name_Len - 1;
         N := Name_Find;
         A := Get_ALI_Id (N);

         --  When this is an issue this has already been reported

         if A = No_ALI_Id then
            return;
         end if;

         --  It is a RCI or SP package

         U := ALIs.Table (A).Last_Unit;

         if Units.Table (U).RCI
           or else Units.Table (U).Shared_Passive
         then
            --  There are not on the same partition

            if Get_Partition_Id (N) /= P then
               Message ("", Quote (N), "and", Quote (X),
                        "are not on the same partition");
               Success := False;
            end if;
         end if;
      end if;
   end Detect_Non_Collocated_Categorized_Child_Unit;

   --------------------------------------------
   -- Detect_Non_Configured_Categorized_Unit --
   --------------------------------------------

   procedure Detect_Non_Configured_Categorized_Unit
     (ALI     : ALI_Id;
      Success : in out Boolean)
   is
      U : constant Unit_Id        := ALIs.Table (ALI).Last_Unit;
      N : constant Unit_Name_Type := ALIs.Table (ALI).Uname;

   begin
      if (Units.Table (U).RCI or else Units.Table (U).Shared_Passive)
        and then not Units.Table (U).Is_Generic
        and then Get_Partition_Id (N) = No_Partition_Id
      then
         if Units.Table (U).RCI then
            Message ("RCI Ada unit", Quote (N),
                     "has not been assigned to a partition");
         else
            Message ("Shared passive Ada unit", Quote (N),
                     "has not been assigned to a partition");
         end if;
         Success := False;
      end if;
   end Detect_Non_Configured_Categorized_Unit;

   ----------------------------------------
   -- Find_Stubs_And_Stamps_From_Closure --
   ----------------------------------------

   procedure Find_Stubs_And_Stamps_From_Closure (Partition : Partition_Id) is
      CU : Conf_Unit_Id;
      U  : Unit_Id;
      A  : ALI_Id;
      F  : File_Name_Type;
      L  : Stub_Id;

   begin
      Partitions.Table (Partition).First_Stub := Stubs.Last + 1;
      Partitions.Table (Partition).Last_Stub  := Stubs.Last;

      --  Append all the dependencies on units which are assigned to
      --  this partition.

      CU := Partitions.Table (Partition).First_Unit;
      while CU /= No_Conf_Unit_Id loop
         A := Conf_Units.Table (CU).My_ALI;

         if Debug_Mode then
            Message ("update stamp from", ALIs.Table (A).Afile);
         end if;

         --  Update most recent stamp of this partition

         Update_Most_Recent_Stamp (Partition, ALIs.Table (A).Afile);

         for J in
           ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit
         loop
            for K in
              Units.Table (J).First_With .. Units.Table (J).Last_With
            loop
               if Present (Withs.Table (K).Afile) then
                  Files.Append (Withs.Table (K).Afile);
               end if;
            end loop;
         end loop;

         CU := Conf_Units.Table (CU).Next_Unit;
      end loop;

      --  Explore the withed units

      <<Next_With>>
      while Files.First <= Files.Last loop
         F := Files.Table (Files.Last);
         Files.Decrement_Last;
         A := Get_ALI_Id (F);

         --  Some units may not have ALI files like generic units

         if A = No_ALI_Id then
            goto Next_With;
         end if;

         U := ALIs.Table (A).Last_Unit;

         if Debug_Mode then
            Message ("check stamp", F);
         end if;

         --  Update most recent stamp of this partition

         Update_Most_Recent_Stamp (Partition, F);

         --  This unit has already been assigned to this
         --  partition. No need to explore any further.

         if Get_Partition_Id (ALIs.Table (A).Uname) = Partition then
            null;

         elsif not Units.Table (U).Is_Generic
           and then (Units.Table (U).RCI
                       or else Units.Table (U).Shared_Passive)
         then
            --  This unit is not assigned to partition J and it is an RCI or
            --  SP unit. Therefore, we append it to the partition stub list.

            Stubs.Increment_Last;
            L := Stubs.Last;
            Stubs.Table (L) := ALIs.Table (A).Uname;

            if Partitions.Table (Partition).Last_Stub = No_Stub_Id then
               Partitions.Table (Partition).First_Stub := L;
            end if;
            Partitions.Table (Partition).Last_Stub := L;

            if Verbose_Mode then
               Message ("append stub", ALIs.Table (A).Uname);
            end if;

         else
            --  Mark this unit as explored and append its dependencies

            Set_Partition_Id (ALIs.Table (A).Uname, Partition);
            for J in
              ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit
            loop
               for K in
                 Units.Table (J).First_With .. Units.Table (J).Last_With
               loop
                  if Present (Withs.Table (K).Afile) then
                     A := Get_ALI_Id (Withs.Table (K).Afile);
                     if A /= No_ALI_Id
                        and then
                       Get_Partition_Id (ALIs.Table (A).Uname) /= Partition
                     then
                        Files.Append (Withs.Table (K).Afile);
                     end if;
                  end if;
               end loop;
            end loop;
         end if;
      end loop;
   end Find_Stubs_And_Stamps_From_Closure;

   ------------------------------
   -- Update_Partition_Tasking --
   ------------------------------

   procedure Update_Partition_Tasking
     (ALI       : ALI_Id;
      Partition : Partition_Id)
   is
      A : ALI_Id;
      F : File_Name_Type;
      N : Unit_Name_Type;
      U : Unit_Id;
      T : Character renames Partitions.Table (Partition).Tasking;

   begin
      if Debug_Mode then
         Message ("update partition", Partitions.Table (Partition).Name,
                  "tasking " & T);
      end if;

      Files.Append (ALIs.Table (ALI).Afile);
      while Files.First <= Files.Last loop
         F := Files.Table (Files.Last);
         Files.Decrement_Last;
         A := Get_ALI_Id (F);

         if Debug_Mode then
            Message ("pop unit", ALIs.Table (A).Uname);
         end if;

         --  Update partition tasking to unit tasking

         if T = 'P' then
            null;

         elsif T = 'U' then
            if ALIs.Table (A).Tasking = 'P' then
               if Debug_Mode then
                  Message ("update tasking from " &
                           T & " to " & ALIs.Table (A).Tasking);
               end if;

               T := ALIs.Table (A).Tasking;
            end if;

         else
            if T /= ALIs.Table (A).Tasking then
               if Debug_Mode then
                  Message ("update tasking from " &
                           T & " to " & ALIs.Table (A).Tasking);
               end if;

               T := ALIs.Table (A).Tasking;
            end if;
         end if;

         --  When needed, push into the collocated units stack the withed units

         for J in
           ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit
         loop
            for K in
              Units.Table (J).First_With .. Units.Table (J).Last_With
            loop
               F := Withs.Table (K).Afile;
               if Present (F) then
                  A := Get_ALI_Id (F);

                  --  Discard predefined units since they do not bring
                  --  tasking with them.

                  if Is_Predefined_File (F) then
                     null;

                  elsif A /= No_ALI_Id then
                     U := ALIs.Table (A).Last_Unit;
                     N := ALIs.Table (A).Uname;

                     --  Discard unit that has already been assigned
                     --  to this partition.

                     if Get_Partition_Id (N) = Partition then
                        null;

                     --  Discard this unit as it may not be collocated

                     elsif Units.Table (U).RCI
                       or else Units.Table (U).Shared_Passive
                     then
                        null;

                     --  Continue investigation later on

                     else
                        if Debug_Mode then
                           Message ("push unit", N);
                        end if;

                        Set_Partition_Id (N, Partition);
                        Files.Append (F);
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
      end loop;
   end Update_Partition_Tasking;

end XE_Sem;
