------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              X E _ B A C K                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2009, Free Software Foundation, Inc.          --
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

   procedure Register_Storages (Self : access Backend) is abstract;
   --  Register available storage supports

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

   function Get_Env_Vars
     (P : Partition_Id; Names_Only : Boolean) return String;
   --  Return a series of environment variables assignment for partition P
   --  (if Names_Only is False), or a space separated list of environment
   --  variable names only (if Names_Only is True).

   procedure Generate_Application_Project_Files;
   --  Generate a project file for the appplication code, extending the one
   --  provided by the user (if any), and including a dependency upon the PCS
   --  project. This is PCS independent.

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
      S3 : String  := No_Str;
      I1_Present : Boolean := False;
      I1         : Int     := -1);
   --  Insert a procedure call. The first non-null parameter is supposed to be
   --  the procedure name. The next parameters are parameters for this
   --  procedure call.

   function Location_List_Image (Location : Location_Id) return Name_Id;
   --  Explore linked list of locations to build its image

   procedure Write_Image (I : out Name_Id; H : Host_Id; P : Partition_Id);
   --  Write in I the text to get the partition hostname. This can be
   --  a shell script.

   procedure Write_With_Clause
     (W : Name_Id;
      U : Boolean := False;
      E : Boolean := False);
   --  Add a with clause W, a use clause when U is true and an
   --  elaborate clause when E is true.

   function Prefix (Check_For : String) return String;
   --  Return the PCS installation prefix as dynamically determined by the
   --  location of the gnatdist executable, or fall back to the default
   --  (configure-time) prefix. The validity of a candidate prefix is
   --  checked by testing whether file Check_For exists under that prefix.
   --  The returned string always ends with a directory separator.

   procedure Apply_Casing_Rules (S : in out String);
   procedure Register_Casing_Rule (S : String);
   --  ??? documentation needed!

   function Partition_Dir_Flag (P : Partition_Id) return String;
   --  Return a gnatmake command line flag setting external variable
   --  PARTITION_DIR for partition P.

   Build_Stamp_File    : File_Name_Type;
   Partition_Main_File : File_Name_Type;
   Partition_Main_Name : Unit_Name_Type;

end XE_Back;
