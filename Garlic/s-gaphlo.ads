------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--      S Y S T E M . G A R L I C . P H Y S I C A L _ L O C A T I O N       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
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

with Ada.Streams;
with System.Garlic.Protocols;
with System.Garlic.Types;

package System.Garlic.Physical_Location is

   --  This package implements the mapping between Partition_ID and
   --  physical locations.

   type Location_Type is private;
   --  This type may be exchanged between partitions. It represents
   --  the physical location of a partition.

   Null_Location : constant Location_Type;

   type Locations is array (Positive range <>) of Location_Type;
   --  This represents the whole set of locations for a given partition

   No_Such_Location, Malformed_Location : exception;

   procedure Register_Protocol (P : in Protocols.Protocol_Access);
   --  Register a protocol to be able to use it later

   procedure Register_Partition
     (P : in Types.Partition_ID;
      L : in String);
   --  Set the location of a partition. This may be called several times
   --  in case of replication and recovery on error (fault tolerance).

   procedure Register_Partition
     (P : in Types.Partition_ID;
      L : in Locations);
   --  Idem, but for the whole set

   procedure Unregister_Partition
     (P : in Types.Partition_ID;
      L : in String := "");
   --  Unregister a given location. If no string is given, this means that
   --  the first occurrence of a partition must be unregistered (the one
   --  which was used). This will raise No_Such_Location if there is no
   --  corresponding location for this partition.

   function Get_Partition
     (P : Types.Partition_ID)
     return String;
   --  Get the first occurrence of a partition localization or raise
   --  No_Such_Location.

   function Get_Partition
     (P : Types.Partition_ID)
     return Location_Type;
   --  Get a partition location or raise No_Such_Location

   function Get_Protocol
     (L : Location_Type)
     return Protocols.Protocol_Access;
   --  Get the protocol type of a given location

   function Get_Data
     (L : Location_Type)
     return String;
   --  Return the additionnal data (used by protocols).

   function To_Location
     (L : String)
      return Location_Type;
   --  Conversion function. The syntax of the string can be:
   --    protocol
   --    protocol:
   --    protocol://
   --    protocol://data
   --  If this kind of format is not found, Malformed_Location will be
   --  raised. No_Such_Location will be raised if the given protocol
   --  is not registered.

   function To_Location
     (P : Protocols.Protocol_Access;
      D : String)
     return Location_Type;
   --  From a protocol and a data string, build a whole location

   function To_String
     (L : Location_Type)
      return String;
   --  Conversion function

   procedure Shutdown;
   --  Shutdown every protocol

private

   type Location_Body;

   type Location_Type is access Location_Body;

   procedure Location_Read_Attribute
     (P : access Ada.Streams.Root_Stream_Type'Class;
      L : out Location_Type);

   procedure Location_Write_Attribute
     (P : access Ada.Streams.Root_Stream_Type'Class;
      L : in Location_Type);

   for Location_Type'Read  use Location_Read_Attribute;
   for Location_Type'Write use Location_Write_Attribute;

   Null_Location : constant Location_Type := null;

end System.Garlic.Physical_Location;
