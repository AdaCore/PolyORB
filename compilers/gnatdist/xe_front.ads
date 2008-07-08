------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ F R O N T                              --
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

--  This package contains all the data structures needed to represent
--  the configuration of the distributed system. As a result of the
--  frontend, these structures will provide info required to produce
--  stubs, skels and other packages.

with XE;       use XE;
with XE_Types; use XE_Types;
with XE_Units; use XE_Units;

package XE_Front is

   --------------
   -- Defaults --
   --------------

   Default_Partition_Id : Partition_Id;
   Default_Channel_Id   : Channel_Id;

   Default_Registration_Filter : Filter_Name_Type     := No_Filter_Name;
   Default_First_Boot_Location : Location_Id          := No_Location_Id;
   Default_Last_Boot_Location  : Location_Id          := No_Location_Id;
   Default_Data_Location       : Location_Id          := No_Location_Id;
   Default_Starter             : Import_Method_Type   := Ada_Import;
   Default_Version_Check       : Boolean              := True;
   Default_Rsh_Command         : Name_Id              := No_Name;
   Default_Rsh_Options         : Name_Id              := No_Name;
   Default_Priority_Policy     : Priority_Policy_Type := No_Priority_Policy;

   Configuration : Unit_Name_Type := No_Unit_Name;
   --  Name of the configuration.

   Main_Partition : Partition_Id := No_Partition_Id;
   --  Partition where the main procedure has been assigned.

   Main_Subprogram : Unit_Name_Type := No_Unit_Name;
   --  Several variables related to the main procedure.

   procedure Frontend;

   procedure Add_Conf_Unit (U : Unit_Name_Type; P : Partition_Id);
   --  Assign a unit to a partition. This unit is declared in the
   --  configuration file (it is not yet mapped to an ada unit).

   procedure Add_Location
     (First : in out Location_Id;
      Last  : in out Location_Id;
      Major : Name_Id;
      Minor : Name_Id);
   --  Read major and minor from variable and add this pair to
   --  partition location list.

   procedure Create_Channel
     (Name : Channel_Name_Type;
      Node : Node_Id;
      CID  : out Channel_Id);
   --  Create a new channel and store its CID in its name key.

   procedure Create_Host
     (Name : Host_Name_Type;
      Node : Node_Id;
      HID  : out Host_Id);
   --  Create a new host and store its HID in its name key.

   procedure Create_Partition
     (Name : Partition_Name_Type;
      Node : Node_Id;
      PID  : out Partition_Id);
   --  Create a new partition and store its PID in its name key.

   function Get_ALI_Id       (N : Name_Id) return ALI_Id;
   function Get_Channel_Id   (N : Name_Id) return Channel_Id;
   function Get_Conf_Unit_Id (N : Name_Id) return Conf_Unit_Id;
   function Get_Host_Id      (N : Name_Id) return Host_Id;
   function Get_Partition_Id (N : Name_Id) return Partition_Id;
   function Get_Unit_Id      (N : Name_Id) return Unit_Id;

   procedure Set_ALI_Id       (N : Name_Id; A : ALI_Id);
   procedure Set_Channel_Id   (N : Name_Id; C : Channel_Id);
   procedure Set_Conf_Unit_Id (N : Name_Id; U : Conf_Unit_Id);
   procedure Set_Host_Id      (N : Name_Id; H : Host_Id);
   procedure Set_Partition_Id (N : Name_Id; P : Partition_Id);
   procedure Set_Unit_Id      (N : Name_Id; U : Unit_Id);

   function  Get_Tasking (A : ALI_Id) return Character;
   procedure Set_Tasking (A : ALI_Id; T : Character);

   function Get_Rsh_Command return Name_Id;
   function Get_Rsh_Options return Name_Id;

   procedure Initialize;
   --  Initialize the first item of each table to use them as default.

   procedure Update_Most_Recent_Stamp (P : Partition_Id; F : File_Name_Type);
   --  The more recent stamp of files needed to build a partition is
   --  updated.

   procedure Show_Configuration;
   --  Report the current configuration

   function To_Build (U : Conf_Unit_Id) return Boolean;
   --  Is this unit mapped on a partition to build

end XE_Front;
