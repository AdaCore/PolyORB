------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . P A R T I T I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with System.Garlic.Name_Table;
with System.Garlic.Physical_Location;
with System.Garlic.Protocols;
with System.Garlic.Streams;
with System.Garlic.Table;
with System.Garlic.Types;
with System.Garlic.Utils;

package System.Garlic.Partitions is

   type Partition_Info is record
      Location       : Physical_Location.Location_Type;
      Protocol       : Protocols.Protocol_Access;
      Logical_Name   : Utils.String_Access;
      Termination    : Types.Termination_Type;
      Reconnection   : Types.Reconnection_Type;
      Has_Light_PCS  : Boolean;
      Is_Boot_Mirror : Boolean;
      Boot_Partition : Types.Partition_ID;
      Status         : Types.Status_Type;
      Allocated      : Boolean;
   end record;

   --  Allocated      : true when this slot is not empty
   --  Location       : partition physical location
   --  Protocol       : cache for location protocol
   --  Logical_Name   : name of the partition (may be duplicated)
   --  Termination    : termination policy to adopt for this partition
   --  Reconnection   : reconnection policy to adopt for this partition
   --  Has_Light_PCS  : true for a partition which does not receive request
   --  Is_Boot_Mirror : true for a partition which has a copy of PID table
   --  Boot_Partition : pid of the partition used to boot a partition
   --  Status         : partition info status

   Null_Partition : constant Partition_Info :=
     (Allocated      => False,
      Location       => Physical_Location.Null_Location,
      Protocol       => null,
      Logical_Name   => null,
      Termination    => Types.Global_Termination,
      Reconnection   => Types.Rejected_On_Restart,
      Has_Light_PCS  => False,
      Is_Boot_Mirror => False,
      Boot_Partition => Types.Null_PID,
      Status         => Types.None);

   type Request_Kind is
      (Compute_Partition_ID,
       Copy_Partition_Table,
       Define_New_Partition,
       Pull_Partition_Table,
       Push_Partition_Table);

   type Request_Type (Kind : Request_Kind := Pull_Partition_Table) is
      record
         case Kind is
            when Pull_Partition_Table |
                 Copy_Partition_Table =>
               null;
            when Push_Partition_Table |
                 Compute_Partition_ID =>
               Partition : Types.Partition_ID;
            when Define_New_Partition =>
               Info : Partition_Info;
         end case;
      end record;

   package Partitions is new System.Garlic.Table.Complex
     (Index_Type     => Types.Partition_ID,
      Null_Index     => Types.Null_PID,
      First_Index    => Types.Valid_Partition_ID'First,
      Initial_Size   => Natural (Types.Valid_Partition_ID'Last),
      Increment_Size => 0,
      Component_Type => Partition_Info,
      Null_Component => Null_Partition);

   procedure Allocate_PID
     (Partition : out Types.Partition_ID;
      Error     : in out Utils.Error_Type);
   --  Allocate a new partition ID. This can need the agreement of the boot
   --  mirrors group.

   function Boot_Partition
     (Partition : Types.Partition_ID)
      return Types.Partition_ID;
   --  Return the pid of the partition used to boot Partition.

   procedure Dump_Partition_Table;

   function Get_Boot_Server return String;
   --  This function returns the coordinates of the boot server

   procedure Get_Location
     (Partition : in Types.Partition_ID;
      Location  : out Physical_Location.Location_Type;
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

   procedure Get_Partition_Info
     (Partition : in     Types.Partition_ID;
      Info      :    out Partition_Info;
      Error     : in out Utils.Error_Type);
   --  If cached, then return local partition info. Otherwise, on a non
   --  boot mirror, send a request. Wait for info to be available.

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

   function Get_Self_Location return Physical_Location.Location_Type;

   procedure Get_Termination_Policy
     (Partition    : in Types.Partition_ID;
      Termination  : out Types.Termination_Type;
      Error        : in out Utils.Error_Type);
   --  Return the termination policy of a remote partition. Use
   --  Get_Partition_Info.

   procedure Handle_Partition_Request
     (Partition : in Types.Partition_ID;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type;
      Error     : in out Utils.Error_Type);
   --  Handle Partition_Service operations

   procedure Invalidate_Partition
     (Partition : in Types.Partition_ID);
   --  Invalidate a partition. If this partition was the boot server, then
   --  choose as boot server the first boot mirror. If we choose the
   --  current partition, then reset options to have a valid boot server
   --  (not a slave). Send to boot mirrors group the invalidation request
   --  or to boot server if the current partition is not a boot mirror.

   function N_Boot_Mirrors return Natural;
   --  Number of boot mirrors in the partition info table

   procedure Next_Partition
     (Partition : in out Types.Partition_ID);
   --  Find next allocated partition after Partition. If no partition is
   --  available, return Null_PID. If Partition is Null_PID when
   --  Next_Partition is called, then starts from the first slot in the
   --  table.

   function Next_Boot_Mirror (Partition : Types.Partition_ID)
     return Types.Partition_ID;
   --  When called with a partition number that is a boot mirror, return
   --  the next partition which is also a boot mirror. It may return the
   --  same partition number if no other boot mirror is available.

   procedure Read_Partitions
     (Stream : access Streams.Params_Stream_Type);
   --  Marshal partition info table.

   procedure Send_Boot_Request
     (Location : in Physical_Location.Location_Type;
      Error    : in out Utils.Error_Type);
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


   procedure Validate_PID
     (PID  : in out Types.Partition_ID;
      From : in Types.Partition_ID);
   --  Validation occurs when all the boot server agree for a given PID.

   procedure Write_Partitions
     (Stream : access Streams.Params_Stream_Type);
   --  Unmarshal partition info table.

end System.Garlic.Partitions;
