------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . P A R T I T I O N S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with System.Garlic.Debug;
with System.Garlic.Name_Table;
with System.Garlic.Physical_Location;
with System.Garlic.Protocols;
with System.Garlic.Streams;
with System.Garlic.Types;
with System.Garlic.Utils;

package System.Garlic.Partitions is

   procedure Allocate_PID
     (Partition : out Types.Partition_ID;
      Name      : in String := "";
      Error     : in out Utils.Error_Type);
   --  Allocate a new partition ID. This can need the agreement of the
   --  boot mirrors group. When Name is empty string, the partition is
   --  active because the name of an active partition is not meaningful.

   procedure Register_Passive_Partition
     (Partition      : out Types.Partition_ID;
      Partition_Name : in String;
      Mem_Locations  : in String;
      Error          : in out Utils.Error_Type);

   function Get_Boot_Locations return String;
   --  This function returns all the coordinates of the boot server

   procedure Get_Boot_Partition
     (Partition      : in Types.Partition_ID;
      Boot_Partition : out Types.Partition_ID;
      Error          : in out Utils.Error_Type);
   --  Return the pid of the partition used to boot Partition.

   procedure Get_Net_Location
     (Partition : in Types.Partition_ID;
      Location  : out Physical_Location.Location_Type;
      Error     : in out Utils.Error_Type);
   --  Return the location of a partition

   procedure Get_Mem_Location
     (Partition : in Types.Partition_ID;
      Location  : out Utils.String_Access;
      Error     : in out Utils.Error_Type);
   --  Return the location of a partition

   procedure Get_Name
     (Partition : in Types.Partition_ID;
      Name      : out Name_Table.Name_Id;
      Error     : in out Utils.Error_Type);

   procedure Get_Name
     (Partition : in Types.Partition_ID;
      Name      : out Utils.String_Access;
      Error     : in out Utils.Error_Type);
   --  Return the name of a partition in its coded or plaintext form

   procedure Get_Protocol
     (Partition : in Types.Partition_ID;
      Protocol  : out Protocols.Protocol_Access;
      Error     : in out Utils.Error_Type);
   pragma Inline (Get_Protocol);
   --  Same as above. But for boot partition, then get protocol from
   --  boot server option.

   procedure Get_Reconnection_Policy
     (Partition    : in Types.Partition_ID;
      Reconnection : out Types.Reconnection_Type;
      Error        : in out Utils.Error_Type);
   --  Return policy to use when reconnecting to Partition

   function Global_Termination_Partitions return Types.Partition_List;
   --  Return list of partitions using global termination

   function Known_Partitions return Types.Partition_List;
   --  Return list of partitions dead or alive

   function Local_Termination_Partitions return Types.Partition_List;
   --  Return list of partitions using local termination

   function Online_Partitions return Types.Partition_List;
   --  Return list of partitions alive

   function Has_Local_Termination (Partition : Types.Partition_ID)
     return Boolean;
   --  Return True if partition has a local termination

   procedure Handle_Partition_Request
     (Partition : in Types.Partition_ID;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type;
      Error     : in out Utils.Error_Type);
   --  Handle Partition_Service operations

   procedure Initialize;

   procedure Invalidate_Partition
     (Partition : in Types.Partition_ID);
   --  Invalidate a partition. If this partition was the boot server, then
   --  choose as boot server the first boot mirror. If we choose the
   --  current partition, then reset options to have a valid boot server
   --  (not a slave). Send to boot mirrors group the invalidation request
   --  or to boot server if the current partition is not a boot mirror.

   function N_Boot_Mirrors return Natural;
   --  Number of boot mirrors in the partition info table

   function Next_Boot_Mirror return Types.Partition_ID;
   --  Return the first boot mirror after this partition or else after
   --  Null_PID.

   procedure Send_Partition_Definition
     (Partition      : in Types.Partition_ID;
      Partition_Name : in Utils.String_Access;
      Is_Active_Part : in Boolean;
      Net_Locations  : in Utils.String_Access;
      Mem_Locations  : in Utils.String_Access;
      Termination    : in Types.Termination_Type;
      Reconnection   : in Types.Reconnection_Type;
      Has_Light_PCS  : in Boolean;
      Is_Boot_Mirror : in Boolean;
      Error          : in out Utils.Error_Type);
   --  Send a boot registration to boot server.  We will send a
   --  Define_New_Partition request to the boot partition. This is step
   --  1. This will cause a dialog to be established and a new Partition_ID
   --  to be allocated. The partition location will be registered into the
   --  boot partition's repository. This is step 2. The boot partition
   --  sends the partition table back to the partition. This is step 3. At
   --  this point, Self_PID and Boot_PID is known but startup can be kept
   --  blocking. The partition will continue to ask for the table until
   --  there are two boot mirrors if the option Mirror_Expected is set to
   --  true. This is step 4. Otherwise, startup can complete and
   --  Self_PID_Barrier is open. This is step 5. When a partition is a
   --  potential boot server then it also sends an add partition info
   --  request to the boot partition. This is step 7. Then an all partition
   --  info request will be broadcast. This is step 8.

   procedure Set_Boot_Location
     (Location  : in Physical_Location.Location_Type);
   --  Set effective boot server coordinates

   procedure Set_Online
     (Partition : in Types.Partition_ID;
      Online    : in Boolean);
   --  Indicates whether a communication link has been initialized
   --  with this partition.

   procedure Set_Used_Protocol
     (Partition : in Types.Partition_ID;
      Protocol  : in Protocols.Protocol_Access);
   --  Define the protocol to use to contact a partition when there is
   --  already info on this.

   procedure Shutdown;
   --  Resume tasks waiting for an update of partition info table to
   --  ensure shutdown.

   procedure Dump_Partition_Table
     (Key : in Debug.Debug_Key := Debug.Always);
   --  Dump partition table on standard output for debugging purpose

end System.Garlic.Partitions;
