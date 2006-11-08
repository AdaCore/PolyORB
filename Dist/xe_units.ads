------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ U N I T S                              --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides all the types and tables required to
--  represent the configuration of the distributed system like
--  Ada units, configured units, partitions, channels, locations, ...

with GNAT.Table;
with XE;       use XE;
with XE_Types; use XE_Types;

package XE_Units is

   ----------------
   -- Name Types --
   ----------------

   No_Unit_Name : constant Unit_Name_Type := Unit_Name_Type (No_Name);

   subtype Partition_Name_Type is Unit_Name_Type;
   No_Partition_Name : constant Partition_Name_Type := No_Unit_Name;

   subtype Channel_Name_Type is Name_Id;
   No_Channel_Name : constant Channel_Name_Type := Channel_Name_Type (No_Name);

   subtype Filter_Name_Type is Name_Id;
   No_Filter_Name : constant Filter_Name_Type := Filter_Name_Type (No_Name);

   subtype Host_Name_Type is Name_Id;
   No_Host_Name : constant Host_Name_Type := Host_Name_Type (No_Name);

   subtype Main_Subprogram_Type is Unit_Name_Type;
   No_Main_Subprogram : constant Main_Subprogram_Type := No_Unit_Name;

   subtype Command_Line_Type is Name_Id;
   No_Command_Line : constant Command_Line_Type := Command_Line_Type (No_Name);

   subtype Directory_Name_Type is File_Name_Type;
   No_Directory_Name  : constant Directory_Name_Type := No_File_Name;

   -------------------
   -- Tasking Types --
   -------------------

   type Task_Pool_Type is array (1 .. 3) of Int;
   No_Task_Pool : constant Task_Pool_Type := (-1, -1, -1);

   type Priority_Type is new Int;
   No_Priority : constant Priority_Type := -1;

   --------------
   -- Id Types --
   --------------

   --  The various entries are stored in tables with distinct subscript
   --  ranges. The following type definitions indicate the ranges used
   --  for the subscripts (Id values) for the various tables.

   type ALI_Id is range 0 .. 999_999;
   --  Id values used for ALIs table entries

   No_ALI_Id    : constant ALI_Id := ALI_Id'First;
   First_ALI_Id : constant ALI_Id := No_ALI_Id + 1;

   type Unit_Id is range 1_000_000 .. 1_999_999;
   --  Id values used for Unit table entries

   No_Unit_Id    : constant Unit_Id := Unit_Id'First;
   First_Unit_Id : constant Unit_Id := No_Unit_Id + 1;

   type With_Id is range 2_000_000 .. 2_999_999;
   --  Id values used for Withs table entries

   No_With_Id    : constant With_Id := With_Id'First;
   First_With_Id : constant With_Id := No_With_Id + 1;

   type Sdep_Id is range 4_000_000 .. 4_999_999;
   --  Id values used for Sdep table entries

   No_Sdep_Id    : constant Sdep_Id := Sdep_Id'First;
   First_Sdep_Id : constant Sdep_Id := No_Sdep_Id + 1;

   type Partition_Id is range 7_000_000 .. 7_000_255;

   No_Partition_Id    : constant Partition_Id := Partition_Id'First;
   First_Partition_Id : constant Partition_Id := No_Partition_Id + 1;

   type Conf_Unit_Id is range 7_100_000 .. 7_199_999;

   No_Conf_Unit_Id    : constant Conf_Unit_Id := Conf_Unit_Id'First;
   First_Conf_Unit_Id : constant Conf_Unit_Id := No_Conf_Unit_Id + 1;

   type Channel_Id is range 7_200_000 .. 7_299_999;

   No_Channel_Id    : constant Channel_Id := Channel_Id'First;
   First_Channel_Id : constant Channel_Id := No_Channel_Id + 1;

   type Host_Id is range 7_300_000 .. 7_399_999;

   No_Host_Id    : constant Host_Id := Host_Id'First;
   First_Host_Id : constant Host_Id := No_Host_Id + 1;

   type Location_Id is range 7_400_000 .. 7_499_999;

   No_Location_Id    : constant Location_Id := Location_Id'First;
   First_Location_Id : constant Location_Id := No_Location_Id + 1;

   type Stub_Id is range 7_500_000 .. 7_599_999;

   No_Stub_Id    : constant Stub_Id := Stub_Id'First;
   First_Stub_Id : constant Stub_Id := No_Stub_Id + 1;

   --------------------
   -- ALI File Table --
   --------------------

   --  Each ALI file read generates an entry in the ALIs table

   type Main_Program_Type is (None, Proc, Func);
   --  Indicator of whether unit can be used as main program

   type ALIs_Record is record

      Ofile : File_Name_Type;
      --  Name of object file (because GNATLS does not return the ali file)

      Afile : File_Name_Type;
      --  Name of ALI file (based on object file)

      Sfile : File_Name_Type;
      --  Name of source file that generates this ALI file (which is equal
      --  to the name of the source file in the first unit table entry for
      --  this ALI file, since the body if present is always first).

      Uname : Unit_Name_Type;
      --  Name of Unit

      First_Unit : Unit_Id;
      --  Id of first Unit table entry for this file

      Last_Unit : Unit_Id;
      --  Id of last Unit table entry for this file

      First_Sdep : Sdep_Id;
      --  Id of first Sdep table entry for this file

      Last_Sdep : Sdep_Id;
      --  Id of last Sdep table entry for this file

      Main_Program : Main_Program_Type;
      --  Indicator of whether first unit can be used as main program.

      Tasking : Character;
      --  Indicator of whether the unit (or the collocated units it
      --  depends on) drags tasking. 'P' indicates that tasking is
      --  required by the PCS (for concurrent remote calls), 'U' that
      --  tasking is required in user code, '?' that the use of
      --  tasking is still unknown and 'N' that the tasking is not
      --  used. Note that 'P' is a stronger property than 'U' as this
      --  has also an impact on the termination policy.

   end record;

   Default_ALI : constant ALIs_Record := (
      Ofile        => No_File_Name,
      Afile        => No_File_Name,
      Sfile        => No_File_Name,
      Uname        => No_Unit_Name,
      First_Unit   => No_Unit_Id,
      Last_Unit    => No_Unit_Id,
      First_Sdep   => First_Sdep_Id,
      Last_Sdep    => No_Sdep_Id,
      Main_Program => None,
      Tasking      => '?');

   package ALIs is new GNAT.Table (
     Table_Component_Type => ALIs_Record,
     Table_Index_Type     => ALI_Id,
     Table_Low_Bound      => First_ALI_Id,
     Table_Initial        => 500,
     Table_Increment      => 200);

   ----------------
   -- Unit Table --
   ----------------

   --  Each unit within an ALI file generates an entry in the unit table

   type Unit_Type is (Is_Spec, Is_Body, Is_Spec_Only, Is_Body_Only);
   --  Indicates type of entry, if both body and spec appear in the ALI file,
   --  then the first unit is marked Is_Body, and the second is marked Is_Spec.
   --  If only a spec appears, then it is marked as Is_Spec_Only, and if only
   --  a body appears, then it is marked Is_Body_Only).

   type Unit_Record is record

      My_ALI : ALI_Id;
      --  Corresponding ALI entry

      Uname : Unit_Name_Type;
      --  Name of Unit

      Sfile : File_Name_Type;
      --  Name of source file

      First_With : With_Id;
      --  Id of first withs table entry for this file

      Last_With : With_Id;
      --  Id of last withs table entry for this file

      Has_RACW : Boolean;
      --  Indicates presence of RA parameter for a package that declares
      --  at least one Remote Access to Class_Wide (RACW) object.
      Remote_Types : Boolean;
      --  Indicates a Remote_Types package.

      Shared_Passive : Boolean;
      --  Indicates a Shared_Passive package.

      RCI : Boolean;
      --  Indicates a Remote_Call_Interface package.

      Preelaborated : Boolean;
      --  Indicates a preelaborated package.

      Pure : Boolean;
      --  Indicates a pure package.

      Predefined : Boolean;
      --  Indicates if unit is language predefined (or a child of such a unit)

      Internal : Boolean;
      --  Indicates if unit is an internal unit (or a child of such a unit)

      Utype : Unit_Type;
      --  Type of entry

      Is_Generic : Boolean;
      --  True for generic unit (i.e. a generic declaration, or a generic
      --  body). False for a non-generic unit.

      Unit_Kind : Character;
      --  Indicates the nature of the unit. 'p' for Packages and 's' for
      --  subprograms.

   end record;

   package Units is new GNAT.Table (
     Table_Component_Type => Unit_Record,
     Table_Index_Type     => Unit_Id,
     Table_Low_Bound      => First_Unit_Id,
     Table_Initial        => 100,
     Table_Increment      => 200);

   Default_Unit : constant Unit_Record := (
      My_ALI         => No_ALI_Id,
      Uname          => No_Unit_Name,
      Sfile          => No_File_Name,
      First_With     => First_With_Id,
      Last_With      => No_With_Id,
      Has_RACW       => False,
      Remote_Types   => False,
      Shared_Passive => False,
      RCI            => False,
      Preelaborated  => False,
      Pure           => False,
      Predefined     => False,
      Internal       => False,
      Utype          => Is_Spec_Only,
      Is_Generic     => False,
      Unit_Kind      => 'p');

   ----------------
   -- With Table --
   ----------------

   --  Each with within an ALI file generates an entry in the Withs table

   type With_Record is record

      Uname : Unit_Name_Type;
      --  Name of Unit

      Sfile : File_Name_Type;
      --  Name of source file, set to No_File in generic case

      Afile : File_Name_Type;
      --  Name of ALI file, set to No_File in generic case
   end record;

   package Withs is new GNAT.Table (
     Table_Component_Type => With_Record,
     Table_Index_Type     => With_Id,
     Table_Low_Bound      => First_With_Id,
     Table_Initial        => 100,
     Table_Increment      => 200);

   Default_With : constant With_Record := (
      Uname          => No_Unit_Name,
      Afile          => No_File_Name,
      Sfile          => No_File_Name);

   ------------------------------------
   -- Sdep (Source Dependency) Table --
   ------------------------------------

   --  Each source dependency (D line) in an ALI file generates an
   --  entry in the Sdep table.

   type Sdep_Record is record

      Sfile : File_Name_Type;
      --  Name of source file

   end record;

   package Sdep is new GNAT.Table (
     Table_Component_Type => Sdep_Record,
     Table_Index_Type     => Sdep_Id,
     Table_Low_Bound      => First_Sdep_Id,
     Table_Initial        => 5000,
     Table_Increment      => 200);

   ---------------------
   -- Partition Table --
   ---------------------

   type Partition_Type is record

      Name : Partition_Name_Type;
      --  Name of partition

      First_Unit : Conf_Unit_Id;
      --  Id of first unit table entry for this partition

      Last_Unit : Conf_Unit_Id;
      --  Id of last unit table entry for this partition

      Main_Subprogram : Unit_Name_Type;
      --  Main subprogram for this partition

      First_Stub : Stub_Id;
      --  Id of first stub unit table entry for this partition

      Last_Stub : Stub_Id;
      --  Id of last stub unit table entry for this partition

      First_Channel : Channel_Id;
      --  Id of first channel table entry for this partition

      Last_Channel : Channel_Id;
      --  Id of last channel table entry for this partition

      Passive : Boolean_Type;
      --  Indicate whether this partition is passive

      Tasking : Character;
      --  Indicate why this partition requires tasking. '?' when the
      --  use of tasking has not been established. 'N' when the
      --  partition does not require tasking, 'U' when the partition
      --  requires tasking because of user needs, 'P' when the
      --  partition requires tasking because of PCS needs.

      Task_Pool : Task_Pool_Type;
      --  Configuration of the task pool

      Priority : Priority_Type;
      --  Priority to which remote calls are executed when priority
      --  policy is server declared.

      Light_PCS : Boolean_Type;
      --  True when the partition may be configured with a light PCS

      Termination : Termination_Type;
      --  Termination policy to activate on this partition

      Reconnection : Reconnection_Type;
      --  Reconnection policy to execute on this partition when
      --  another partition crashes.

      Host : Host_Id;
      --  Host on which this partition has to be launched

      Command_Line : Command_Line_Type;
      --  Command line to pass when we launch the partition

      First_Network_Loc : Location_Id;
      --  Id of first network location table entry for this partition

      Last_Network_Loc : Location_Id;
      --  Id of last network location table entry for this partition

      Storage_Loc : Location_Id;
      --  Id of storage location table entry for this partition

      Filter : Filter_Name_Type;
      --  Name of filter to apply during filter registration

      Partition_Dir : File_Name_Type;
      --  Internal directory in which we build the partition

      Executable_Dir : Directory_Name_Type;
      --  Directory to store the executable file

      Executable_File : File_Name_Type;
      --  Fully qualified executable file name

      To_Build : Boolean;
      --  True when we want to build this partition

      Node : Node_Id;
      --  Frontend node

      Most_Recent : File_Name_Type;
      --  Most recent source file of this partition

   end record;

   package Partitions  is new GNAT.Table
     (Table_Component_Type => Partition_Type,
      Table_Index_Type     => Partition_Id,
      Table_Low_Bound      => First_Partition_Id,
      Table_Initial        => 20,
      Table_Increment      => 10);

   Null_Partition : constant Partition_Type :=
     (Name               => No_Partition_Name,
      First_Unit         => No_Conf_Unit_Id,
      Last_Unit          => No_Conf_Unit_Id,
      Main_Subprogram    => No_Unit_Name,
      First_Stub         => First_Stub_Id,
      Last_Stub          => No_Stub_Id,
      First_Channel      => No_Channel_Id,
      Last_Channel       => No_Channel_Id,
      Passive            => BMaybe,
      Tasking            => '?',
      Task_Pool          => No_Task_Pool,
      Priority           => No_Priority,
      Light_PCS          => BMaybe,
      Termination        => No_Termination,
      Reconnection       => No_Reconnection,
      Command_Line       => No_Command_Line,
      Host               => No_Host_Id,
      First_Network_Loc  => No_Location_Id,
      Last_Network_Loc   => No_Location_Id,
      Storage_Loc        => No_Location_Id,
      Filter             => No_Filter_Name,
      Partition_Dir      => No_Directory_Name,
      Executable_Dir     => No_Directory_Name,
      Executable_File    => No_File_Name,
      To_Build           => True,
      Node               => Null_Node,
      Most_Recent        => No_File_Name);

   ---------------------------
   -- Configured Unit Table --
   ---------------------------

   --  Configured units are different from units. Such units come from
   --  the configuration language and mau not correspond to ada units
   --  since the configuration file can be erroneous.

   type Conf_Unit_Type is record

      Name : Unit_Name_Type;
      --  Name of unit

      Node : Node_Id;
      --  Frontend node

      My_ALI : ALI_Id;
      --  Corresponding ALI id

      My_Unit : Unit_Id;
      --  Corresponding unit id

      Partition : Partition_Id;
      --  Partition on which this unit is assigned

      Next_Unit : Conf_Unit_Id;
      --  Next unit assigned on the unit partition

      Most_Recent : File_Name_Type;
      --  Most recent file for this unit

   end record;

   package Conf_Units is new GNAT.Table
     (Table_Component_Type => Conf_Unit_Type,
      Table_Index_Type     => Conf_Unit_Id,
      Table_Low_Bound      => First_Conf_Unit_Id,
      Table_Initial        => 200,
      Table_Increment      => 100);

   Null_Conf_Unit : constant Conf_Unit_Type :=
     (Name        => No_Unit_Name,
      Node        => Null_Node,
      My_ALI      => No_ALI_Id,
      My_Unit     => No_Unit_Id,
      Partition   => No_Partition_Id,
      Next_Unit   => No_Conf_Unit_Id,
      Most_Recent => No_File_Name);

   -------------------
   -- Channel Types --
   -------------------

   type Channel_Partition_Type is record
      My_Partition : Partition_Id;
      Next_Channel : Channel_Id;
   end record;

   Null_Channel_Partition : Channel_Partition_Type :=
     (No_Partition_Id, No_Channel_Id);

   type Channel_Type is record
      Name   : Channel_Name_Type;
      Node   : Node_Id;
      Lower  : Channel_Partition_Type;
      Upper  : Channel_Partition_Type;
      Filter : Filter_Name_Type;
   end record;

   package Channels  is new GNAT.Table
     (Table_Component_Type => Channel_Type,
      Table_Index_Type     => Channel_Id,
      Table_Low_Bound      => First_Channel_Id,
      Table_Initial        => 20,
      Table_Increment      => 10);

   ----------------
   -- Host Table --
   ----------------

   type Host_Type is record
      Name        : Host_Name_Type;
      Node        : Node_Id;
      Static      : Boolean            := True;
      Import      : Import_Method_Type := None_Import;
      External    : Host_Name_Type     := No_Host_Name;
      Most_Recent : File_Name_Type     := No_File_Name;
   end record;

   package Hosts is new GNAT.Table
     (Table_Component_Type => Host_Type,
      Table_Index_Type     => Host_Id,
      Table_Low_Bound      => First_Host_Id,
      Table_Initial        => 20,
      Table_Increment      => 10);

   --------------------
   -- Location Table --
   --------------------

   type Location_Type is record
      Major         : Name_Id;
      Minor         : Name_Id;
      Next_Location : Location_Id;
   end record;

   package Locations is new GNAT.Table
     (Table_Component_Type => Location_Type,
      Table_Index_Type     => Location_Id,
      Table_Low_Bound      => First_Location_Id,
      Table_Initial        => 20,
      Table_Increment      => 10);

   ----------------
   -- Stub Table --
   ----------------

   package Stubs is new GNAT.Table
     (Table_Component_Type => Unit_Name_Type,
      Table_Index_Type     => Stub_Id,
      Table_Low_Bound      => First_Stub_Id,
      Table_Initial        => 20,
      Table_Increment      => 10);

end XE_Units;
