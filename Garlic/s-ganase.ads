------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . N A M E _ S E R V E R             --
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

with Ada.Unchecked_Deallocation;
with System.Garlic.Heart;
with System.Garlic.Name_Table;
with System.Garlic.Physical_Location;
with System.Garlic.Protocols;
with System.Garlic.Types;

private package System.Garlic.Name_Server is

   pragma Elaborate_Body;

   type Public_Data is record
      Location     : System.Garlic.Physical_Location.Location_Type;
      Name         : System.Garlic.Name_Table.Name_Id;
      Termination  : System.Garlic.Types.Termination_Type;
      Reconnection : System.Garlic.Types.Reconnection_Type;
      Alive        : Boolean := False;
   end record;
   --  This structure represents the public data that can be shared
   --  concerning a partition. The fields are:
   --    Location     : physical location of the partition
   --    Name         : name of the partition (may be duplicated)
   --    Termination  : the way the partition wants termination to be handled
   --    Reconnection : reconnection policy to adopt for this partition
   --    Alive        : false if the partition is known to be dead;
   --                   this field is preset to False so that we can
   --                   detect that the local data has not been initialized

   type Partition_Data is record
      Public   : Public_Data;
      Known    : Boolean;
      Queried  : Boolean;
   end record;
   --  Location holds the location, Name the name of the partition, Known
   --  the fact that we already have information on this partition, and
   --  Queried the fact that the caller has to obtain the information using
   --  another way.

   function Get_Partition_Data (Partition : System.Garlic.Types.Partition_ID)
     return Partition_Data;
   --  Return a partition's data

   function Get_Public_Data (Partition : System.Garlic.Types.Partition_ID)
     return Public_Data;
   --  Return a partition's public data

   type Partition_Data_Array is
      array (System.Garlic.Types.Valid_Partition_ID) of Partition_Data;

   protected type Partition_Map_Type is
      procedure Set_Data
        (Partition : in System.Garlic.Types.Partition_ID;
         Data      : in Partition_Data);
      function Get_Data (Partition : System.Garlic.Types.Partition_ID)
        return Partition_Data;
      entry Wait_For_Data (Partition : in  System.Garlic.Types.Partition_ID;
                           Data      : out Partition_Data);

   private
      entry Queue (Partition : in System.Garlic.Types.Partition_ID;
                   Data      : out Partition_Data);

      Map : Partition_Data_Array;
      New_Data : Boolean := False;
      --  Local barrier

   end Partition_Map_Type;
   --  Data available for a partition. When the data is not available for
   --  a given partition, Wait_For_Data will block, unless the caller *has*
   --  to ask for information about the given partition (in this case,
   --  the Queried field is set to True). If we are on the server, Queried
   --  is never set to True since we wait for the partition itself to
   --  register (this should have occurred already in a normal utilization
   --  since there is no way for a partition to ask for another one if
   --  the name server has not mapped the name of the wanted package on the
   --  partition number).

   type Partition_Map_Access is access Partition_Map_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Partition_Map_Type,
                                      Partition_Map_Access);
   Partition_Map : Partition_Map_Access := new Partition_Map_Type;
   --  Same kludge as above to raise Program_Error at deallocation time ???

   function Get_Boot_Server return String;
   --  This function returns the coordinates of the boot server

   function Get_Boot_Server return System.Garlic.Types.Partition_ID;
   --  Return the partition of the boot server

   function Get_Protocol
     (Partition : System.Garlic.Types.Partition_ID)
      return System.Garlic.Protocols.Protocol_Access;
   pragma Inline (Get_Protocol);
   --  Return the protocol of a partition using a cache whenever possible

end System.Garlic.Name_Server;
