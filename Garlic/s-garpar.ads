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

with System.Garlic.Protocols;
with System.Garlic.Table;
with System.Garlic.Types;
with System.Garlic.Utils;
with System.Garlic.Physical_Location;

private package System.Garlic.Partitions is

   type Partition_Info is record
      Location     : Physical_Location.Location_Type;
      Protocol     : Protocols.Protocol_Access;
      Logical_Name : Utils.String_Access;
      Termination  : Types.Termination_Type;
      Reconnection : Types.Reconnection_Type;
      Light_RTS    : Boolean;
      Boot_Mirror  : Boolean;
      Boot_Server  : Types.Partition_ID;
      Status       : Types.Status_Type;
      Allocated    : Boolean;
   end record;

   --  Allocated    : true when this slot is not empty
   --  Location     : partition physical location
   --  Protocol     : cache for location protocol
   --  Logical_Name : name of the partition (may be duplicated)
   --  Termination  : termination policy to adopt for this partition
   --  Reconnection : reconnection policy to adopt for this partition
   --  Light_RTS    : partition which should not receive request
   --  Status       : partition info status

   Null_Partition : constant Partition_Info :=
     (Allocated    => False,
      Location     => Physical_Location.Null_Location,
      Protocol     => null,
      Logical_Name => null,
      Termination  => Types.Global_Termination,
      Reconnection => Types.Rejected_On_Restart,
      Light_RTS    => False,
      Boot_Mirror  => False,
      Boot_Server  => Types.Null_PID,
      Status       => Types.None);

   type Request_Kind is
      (Add_Partition_Info,
       All_Partition_Info,
       Get_Partition_Info,
       Map_Partition_Info,
       New_Partition_Info,
       Set_Partition_Info);

   --  Add_Partition_Info : Add partition to boot server group
   --  All_Partition_Info : Broadcast partition list to boot server group
   --  Get_Partition_Info : Ask for partition info to a specific boot server
   --  New_Partition_Info : Declare new partition to a specific boot server
   --  Set_Partition_Info : Set partition info on a specific partition

   type Request_Type (Kind : Request_Kind := Get_Partition_Info) is
      record
         case Kind is
            when Add_Partition_Info |
                 All_Partition_Info =>
               null;
            when Get_Partition_Info |
                 Map_Partition_Info |
                 New_Partition_Info |
                 Set_Partition_Info =>
               Partition : Types.Partition_ID;
               case Kind is
                  when Add_Partition_Info |
                       All_Partition_Info |
                       Get_Partition_Info |
                       Map_Partition_Info =>
                     null;
                  when New_Partition_Info |
                       Set_Partition_Info =>
                     Info : Partition_Info;
               end case;
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

   function Allocate_PID return Types.Partition_ID;
   --  Allocate a new partition ID

   procedure Validate_PID
     (PID  : in out Types.Partition_ID;
      From : in Types.Partition_ID);
   --  Validation occurs when all the boot server agree for a given PID.

   procedure Dump_Partition_Info
     (PID  : in Types.Partition_ID;
      Info : in Partition_Info);
   --  Dump a summary of all the information we have on a partition

   function Get_Boot_Server return String;
   --  This function returns the coordinates of the boot server

   function Get_Partition_Info
     (Partition : Types.Partition_ID)
      return Partition_Info;
   --  If cached, then return local partition info. Otherwise, on a non
   --  boot partition send a request. Wait for info to be available.

   function Get_Protocol
     (Partition : Types.Partition_ID)
      return Protocols.Protocol_Access;
   pragma Inline (Get_Protocol);
   --  Same as above. But for boot partition, then get protocol from
   --  boot server option.

   function Get_Self_Location return Physical_Location.Location_Type;

end System.Garlic.Partitions;
