------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2005 Free Software Foundation, Inc.           --
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

--  This package and its children define all the routines to generate stubs
--  (object files), skeleton files (object files), PCS units (source and
--  object files) and eventually executable files.

with XE_Types; use XE_Types;
with XE_Units; use XE_Units;

package XE_Back is

   type Backend is abstract tagged limited private;
   type Backend_Access is access all Backend'Class;

   procedure Set_PCS_Dist_Flags (Self : access Backend) is abstract;
   --  Set PCS-specific default command line flags

   procedure Initialize (Self : access Backend) is abstract;
   --  Initialize the backend

   procedure Run_Backend (Self : access Backend) is abstract;
   --  Generate stubs, skels, PCS units and executables.

   function Find_Backend (PCS_Name : String) return Backend_Access;
   --  Return an instance of the backend appropriate for the specified PCS

   function Get_Detach_Flag (Self : access Backend) return Name_Id is abstract;
   --  Returns the flag for detaching a partition from command line

   PCS_Conf_Unit : Name_Id := No_Name;
   --  Define a PCS unit that gnatdist has to automatically configure
   --  on the main partition.

private

   type Backend is abstract tagged limited null record;

   function "and" (N : Name_Id; S : String) return Name_Id;
   function "and" (L, R : Name_Id) return Name_Id;

   procedure Register_Backend
     (PCS_Name : String; The_Backend : Backend_Access);

   procedure Export_Environment_Var (EV : String);
   --  Add EV environment var to the list of exported variables in the remote
   --  partitions environments.

   procedure Generate_Partition_Project_File
     (D : Directory_Name_Type;
      P : Partition_Id := No_Partition_Id);
   --  Generate a project file extending the user's project to build
   --  one partition.

   procedure Generate_All_Stubs_And_Skels;
   --  Generates needed stubs and skels for all the partitions

   procedure Generate_Skel (A : ALI_Id; P : Partition_Id);
   --  Create skel for a RCI or SP unit and store them in the
   --  directory of the partition on which the unit is assigned.

   procedure Generate_Stub (A : ALI_Id);
   --  Create stub and skel for a RCI or SP unit.

   procedure Generate_Stamp_File (P : Partition_Id);
   --  Create a stamp file in which the executable file stamp and the
   --  configuration file stamp are stored.

   procedure Generate_Starter_File (Backend : Backend_Access);
   --  Create the starter file to launch the other partitions from
   --  main partition subprogram. This can be a shell script or an Ada
   --  program.

   function Get_Environment_Vars_Command return String;
   --  Return a shell command that exports all the registered environment vars

   procedure Initialize;
   --  Initialize PCS-independent backend information

   procedure Prepare_Directories;
   --  Create partition and executable directories, and clean old object files

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

   procedure Write_Image (I : out Name_Id; H : Host_Id; P : Partition_Id);
   --  Write in I the text to get the partition hostname. This can be
   --  a shell script.

   procedure Write_With_Clause
     (W : Name_Id;
      U : Boolean := False;
      E : Boolean := False);
   --  Add a with clause W, a use clause when U is true and an
   --  elaborate clause when E is true.

   procedure Apply_Casing_Rules (S : in out String);
   procedure Register_Casing_Rule (S : String);
   --  ??? documentation needed!

   Build_Stamp_File    : File_Name_Type;
   Partition_Main_File : File_Name_Type;
   Partition_Main_Name : Unit_Name_Type;

end XE_Back;
