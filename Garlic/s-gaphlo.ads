------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--      S Y S T E M . G A R L I C . P H Y S I C A L _ L O C A T I O N       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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
with System.Garlic.Utils;

package System.Garlic.Physical_Location is

   --  This package implements the mapping between Partition_ID and
   --  physical locations.

   type Location_Type is private;
   --  This type may be exchanged between partitions. It represents
   --  the physical location of a partition.

   Null_Location : constant Location_Type;

   No_Such_Location, Malformed_Location : exception;

   procedure Add_First_Missing_Location
     (List     : in Utils.String_Array_Access;
      Current  : in out Natural;
      Protocol : in Protocols.Protocol_Access;
      Data     : in Utils.String_Array_Access);
   --  Add in List the first missing location. Current indicates the
   --  last used slot in List. We may have to call several times
   --  this procedure to include all the location of a given protocol.

   procedure Add_Missing_Locations
     (List     : in Utils.String_Array_Access;
      Current  : in out Natural;
      Protocol : in Protocols.Protocol_Access);
   --  Add in List all the missing location declared by
   --  Protocol. Current indicates the last used slot in List.

   procedure Destroy (Location : in out Location_Type);

   function Get_Protocol
     (L : Location_Type)
     return Protocols.Protocol_Access;
   --  Return the protocol type of a given location

   function Get_Data
     (L : Location_Type)
     return String;
   --  Return the additionnal data (used by protocols)

   function Get_Support_Data
     (L : String)
     return String;
   --  Return the additionnal support data.

   function Get_Support_Name
     (L : String)
     return String;
   --  Return the support name.

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

   function To_Location
     (P : Protocols.Protocol_Access;
      D : String)
     return String;
   --  From a protocol and a data string, build a whole location string

   function To_String
     (L : Location_Type)
      return String;
   --  Conversion function

   function String_To_Location (S : String) return Location_Type
     renames To_Location;
   --  This function renames To_Location because pragma Stream_Convert does
   --  not support overloaded functions.

private

   Max : constant := 64;

   type Location_Type is
      record
         Protocol : Protocols.Protocol_Access;
         Data_Str : String (1 .. Max);
         Data_Len : Natural;
      end record;

   pragma Stream_Convert (Entity => Location_Type,
                          Read   => String_To_Location,
                          Write  => To_String);

   Null_Location : constant Location_Type := (Protocol => null,
                                              Data_Str => (others => ' '),
                                              Data_Len => 0);

end System.Garlic.Physical_Location;
