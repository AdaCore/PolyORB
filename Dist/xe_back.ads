------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with ALI;
with Table;
with Types;
with XE;

package XE_Back is


   --  CID_Type : channel id type

   type CID_Type is new Types.Int range 100_000 .. 199_999;

   Null_CID  : constant CID_Type := 100_000;
   First_CID : constant CID_Type := 100_001;
   Last_CID  : constant CID_Type := 199_999;


   --  CUID_Type : configuration unit id type

   type CUID_Type is new Types.Int range 200_000 .. 299_999;
   --  CUID = Configure Unit ID to differentiate from Unit_Id. Such units
   --  from the configuration language are not always real ada units as
   --  configuration file can be erroneous.

   Null_CUID  : constant CUID_Type := 200_000;
   First_CUID : constant CUID_Type := 200_001;
   Last_CUID  : constant CUID_Type := 299_999;


   --  HID_Type : host id type

   type HID_Type is new Types.Int range 300_000 .. 399_999;

   Null_HID  : constant HID_Type := 300_000;
   First_HID : constant HID_Type := 300_001;
   Last_HID  : constant HID_Type := 399_999;


   --  PID_Type : partition id type

   type PID_Type is new Types.Int range 400_000 .. 499_999;

   Null_PID  : constant PID_Type := 400_000;
   First_PID : constant PID_Type := 400_001;
   Last_PID  : constant PID_Type := 499_999;


   -- Names --

   subtype Partition_Name_Type is Types.Name_Id;
   No_Partition_Name : constant Partition_Name_Type := Types.No_Name;

   subtype Channel_Name_Type is Types.Name_Id;
   No_Channel_Name : constant Channel_Name_Type := Types.No_Name;

   subtype Filter_Name_Type is Types.Name_Id;
   No_Filter_Name  : constant Filter_Name_Type := Types.No_Name;

   subtype CUnit_Name_Type is Types.Name_Id;
   No_CUnit_Name   : constant CUnit_Name_Type := Types.No_Name;

   subtype Host_Name_Type is Types.Name_Id;
   No_Host_Name    : constant Host_Name_Type := Types.No_Name;

   subtype Main_Subprogram_Type is Types.Name_Id;
   No_Main_Subprogram : constant Main_Subprogram_Type := Types.No_Name;

   subtype Command_Line_Type is Types.Name_Id;
   No_Command_Line : constant Command_Line_Type := Types.No_Name;

   subtype Storage_Dir_Name_Type is Types.Name_Id;
   No_Storage_Dir  : constant Storage_Dir_Name_Type := Types.No_Name;

   type Task_Pool_Type is array (1 .. 3) of Types.Name_Id;
   No_Task_Pool    : Task_Pool_Type;

   -- Defaults --

   Default_Partition           : PID_Type;
   Default_Channel             : CID_Type;

   Default_Registration_Filter : Filter_Name_Type      := No_Filter_Name;
   Default_Protocol_Name       : Types.Name_Id         := Types.No_Name;
   Default_Protocol_Data       : Types.Name_Id         := Types.No_Name;
   Default_Starter             : XE.Import_Method_Type := XE.Ada_Import;
   Default_Version_Check       : Boolean               := True;

   -- Table element types --

   type Channel_Partition_Type is record
      My_Partition : PID_Type;
      Next_Channel : CID_Type;
   end record;

   Null_Channel_Partition : Channel_Partition_Type := (Null_PID, Null_CID);

   type Channel_Type is record
      Name   : Channel_Name_Type;
      Node   : XE.Node_Id;
      Lower  : Channel_Partition_Type;
      Upper  : Channel_Partition_Type;
      Filter : Filter_Name_Type;
   end record;

   type Conf_Unit_Type is record
      CUname      : CUnit_Name_Type;
      Node        : XE.Node_Id;
      My_ALI      : ALI.ALI_Id;
      My_Unit     : ALI.Unit_Id;
      Partition   : PID_Type;
      Next        : CUID_Type;
      Most_Recent : Types.File_Name_Type;
   end record;

   type Host_Type is record
      Name        : Host_Name_Type;
      Node        : XE.Node_Id;
      Static      : Boolean               := True;
      Import      : XE.Import_Method_Type := XE.None_Import;
      External    : Host_Name_Type        := Types.No_Name;
      Most_Recent : Types.File_Name_Type  := Types.No_File;
   end record;

   type Partition_Type is record
      Name            : Partition_Name_Type;
      Node            : XE.Node_Id;
      Host            : HID_Type;
      Storage_Dir     : Storage_Dir_Name_Type;
      Command_Line    : Command_Line_Type;
      Main_Subprogram : Types.Unit_Name_Type;
      Termination     : XE.Termination_Type;
      First_Unit      : CUID_Type;
      Last_Unit       : CUID_Type;
      First_Channel   : CID_Type;
      Last_Channel    : CID_Type;
      To_Build        : Boolean;
      Most_Recent     : Types.File_Name_Type;
      Filter          : Filter_Name_Type;
      Task_Pool       : Task_Pool_Type;
      Light_PCS       : Boolean;
      Executable_File : Types.File_Name_Type;
      Partition_Dir   : Types.File_Name_Type;
      Global_Checksum : Types.Word;
   end record;

   -- Tables --

   package Partitions  is new Table.Table
     (Table_Component_Type => Partition_Type,
      Table_Index_Type     => PID_Type,
      Table_Low_Bound      => First_PID,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Partition");

   package Hosts  is new Table.Table
     (Table_Component_Type => Host_Type,
      Table_Index_Type     => HID_Type,
      Table_Low_Bound      => First_HID,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Host");

   package Channels  is new Table.Table
     (Table_Component_Type => Channel_Type,
      Table_Index_Type     => CID_Type,
      Table_Low_Bound      => First_CID,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Channel");

   package CUnit is new Table.Table
     (Table_Component_Type => Conf_Unit_Type,
      Table_Index_Type     => CUID_Type,
      Table_Low_Bound      => First_CUID,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "CUnit");

   Configuration   : Types.Name_Id := Types.No_Name;
   --  Name of the configuration.

   Main_Partition  : PID_Type      := Null_PID;
   --  Partition where the main procedure has been assigned.

   Main_Subprogram : Types.Name_Id := Types.No_Name;
   --  Several variables related to the main procedure.

   procedure Add_Channel_Partition
     (Partition : in Partition_Name_Type; To : in CID_Type);
   --  Assign a paritition to a channel. Sort the partition pair.

   procedure Add_Conf_Unit
     (CU : in CUnit_Name_Type; To : in PID_Type);
   --  Assign a Conf Unit to a partition. This unit is declared in the
   --  configuration file (it is not yet mapped to an ada unit).

   function Already_Loaded (Unit : Types.Name_Id) return Boolean;
   --  Check that this unit has not been previously loaded in order
   --  to avoid multiple entries in GNAT tables.

   procedure Back;

   procedure Compute_Checksum
     (P : in PID_Type;
      F : in Types.File_Name_Type);

   Image : array (XE.Termination_Type) of Types.Name_Id;

   procedure Create_Channel
     (Name : in  Channel_Name_Type;
      Node : in  XE.Node_Id;
      CID  : out CID_Type);
   --  Create a new channel and store its CID in its name key.

   procedure Create_Host
     (Name : in  Host_Name_Type;
      Node : in  XE.Node_Id;
      HID  : out HID_Type);
   --  Create a new host and store its HID in its name key.

   procedure Create_Partition
     (Name : in  Partition_Name_Type;
      Node : in  XE.Node_Id;
      PID  : out PID_Type);
   --  Create a new partition and store its PID in its name key.

   function Get_Absolute_Exec (P : PID_Type) return Types.File_Name_Type;
   --  Look for storage_dir into partitions and compute absolute executable
   --  name. If null, return default.

   function Get_ALI_Id (N : Types.Name_Id) return ALI.ALI_Id;
   --  Return N name key if its value is in ALI_Id range, otherwise
   --  return No_ALI_Id.

   function Get_CID (N : Types.Name_Id) return CID_Type;

   function Get_Command_Line (P : PID_Type) return Command_Line_Type;
   --  Look for conammd_line into partitions. If null, return default.

   function Get_CUID            (N : Types.Name_Id) return CUID_Type;

   function Get_Filter          (C : CID_Type) return Types.Name_Id;
   --  Look for filter in channels. If null, return default.

   function Get_Filter          (P : PID_Type) return Types.Name_Id;
   --  Look for filter in partitions. If null, return default.

   function Get_HID             (N : Types.Name_Id) return HID_Type;

   function Get_Host            (P : PID_Type) return Types.Name_Id;
   --  Look for host into partitions. If null, return default.

   function Get_Light_PCS       (P : PID_Type) return Boolean;
   --  Return true when a partition has neither RCI nor RACW.

   function Get_Main_Subprogram (P : PID_Type) return Main_Subprogram_Type;
   --  Look for main_subprogram into partitions. If null, return default.

   function Get_Parent          (N : Types.Name_Id) return Types.Name_Id;
   --  Extract any parent from this name.

   function Get_Partition_Dir   (P : PID_Type) return Types.File_Name_Type;
   --  Look for partition_dir into partitions. If null, return default.

   function Get_PID             (N : Types.Name_Id) return PID_Type;

   function Get_Relative_Exec   (P : PID_Type) return Types.File_Name_Type;
   --  Look for storage_dir into partitions and compute relative executable
   --  name into partitions. If null, return default.

   function Get_Storage_Dir     (P : PID_Type) return Storage_Dir_Name_Type;
   --  Look for storage_dir into partitions. If null, return default.

   function Get_Task_Pool       (P : PID_Type) return Task_Pool_Type;
   --  Look for task_pool into partitions. If null, return default.

   function Get_Termination     (P : PID_Type) return XE.Termination_Type;
   --  Look for termination into partitions. If null, return default.

   function Get_Unit_Id         (N : Types.Name_Id) return ALI.Unit_Id;
   --  Return N name key if its value is in Unit_Id range, otherwise
   --  return No_Unit_Id.

   function Get_Unit_Sfile      (U : ALI.Unit_Id)  return Types.File_Name_Type;
   --  Look for sfile into unit.

   procedure Initialize;
   --  Initialize the first item of each table to use them as default.

   function Is_Set (Partition : PID_Type) return Boolean;
   --  Some units have already been assigned to this partition.

   procedure Most_Recent_Stamp
     (P : in PID_Type; F : in Types.File_Name_Type);
   --  The more recent stamp of files needed to build a partition is
   --  updated.

   procedure Set_ALI_Id
     (N : in Types.Name_Id; A : in ALI.ALI_Id);
   --  Set A in N key.

   procedure Set_CID
     (N : in Types.Name_Id; C : in CID_Type);

   procedure Set_CUID
     (N : in Types.Name_Id; U : in CUID_Type);

   procedure Set_Light_PCS
     (P : in PID_Type; B : in Boolean);
   --  Set when neither RCI nor RACW are present.

   procedure Set_HID
     (N : in Types.Name_Id; H : in HID_Type);

   procedure Set_PID
     (N : in Types.Name_Id; P : in PID_Type);

   procedure Set_Termination
     (P : in PID_Type; T : in XE.Termination_Type);

   procedure Set_Unit_Id
     (N : in Types.Name_Id; U : in ALI.Unit_Id);
   --  Set U into N key.

   procedure Show_Configuration;
   --  Report the current configuration.

end XE_Back;
