--
--  $Id$
--

with Ada.Finalization;
with Ada.Streams;
with System.Garlic.Protocols;
with System.RPC;

package System.Garlic.Physical_Location is

   --  This package implements the mapping between Partition_ID and
   --  physical locations.

   type Location is private;
   --  This type may be exchanged between partitions. It represents
   --  the physical location of a partition.

   type Locations is array (Positive range <>) of Location;
   --  This represents the whole set of locations for a given partition.

   No_Such_Location, Malformed_Location : exception;

   procedure Register_Protocol (P : in Protocols.Protocol_Access);
   --  Register a protocol to be able to use it later.

   procedure Register_Partition
     (P : in System.RPC.Partition_ID;
      L : in String);
   --  Set the location of a partition. This may be called several times
   --  in case of replication and recovery on error (fault tolerance).

   procedure Register_Partition
     (P : in System.RPC.Partition_ID;
      L : in Locations);
   --  Idem, but for the whole set.

   procedure Unregister_Partition
     (P : in System.RPC.Partition_ID;
      L : in String := "");
   --  Unregister a given location. If no string is given, this means that
   --  the first occurrence of a partition must be unregistered (the one
   --  which was used). This will raise No_Such_Location if there is no
   --  corresponding location for this partition.

   function Get_Partition
     (P : System.RPC.Partition_ID)
     return String;
   --  Get the first occurrence of a partition localization or raise
   --  No_Such_Location.

   function Get_Partition
     (P : System.RPC.Partition_ID)
     return Location;
   --  Get a partition location or raise No_Such_Location.

   function Get_Protocol
     (L : Location)
     return Protocols.Protocol_Access;
   --  Get the protocol type of a given location.

   function Get_Data
     (L : Location)
     return String;
   --  Return the additionnal data (used by protocols).

   function To_Location
     (L : String)
      return Location;
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
     return Location;
   --  From a protocol and a data string, build a whole location.

   function To_String
     (L : Location)
      return String;
   --  Conversion function.

   procedure Shutdown;
   --  Shutdown every protocol.

private

   type String_Ptr is access String;

   type Location is new Ada.Finalization.Controlled with record
      Protocol : Protocols.Protocol_Access;
      Data     : String_Ptr;
   end record;

   procedure Adjust   (L : in out Location);
   procedure Finalize (L : in out Location);

   procedure Location_Read_Attribute
     (P : access Ada.Streams.Root_Stream_Type'Class;
      L : out Location);

   procedure Location_Write_Attribute
     (P : access Ada.Streams.Root_Stream_Type'Class;
      L : in Location);

   for Location'Read  use Location_Read_Attribute;
   for Location'Write use Location_Write_Attribute;

end System.Garlic.Physical_Location;
