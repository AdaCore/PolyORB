------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--          S Y S T E M . P A R T I T I O N _ I N T E R F A C E             --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with System.RPC;

package System.Partition_Interface is

   type Subprogram_Id is new Natural;
   --  This type is used exclusively by stubs.

   subtype Unit_Name is String;
   type Unit_Name_Access is access Unit_Name;
   --  Name of RCI unit.

   function Get_Local_Partition_ID return RPC.Partition_ID;
   --  Return the Partition_ID of the current partition.

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return RPC.Partition_ID;
   --  Similar in some respects to RCI_Info.Get_Active_Partition_ID.

   function Get_Passive_Partition_ID
     (Name : Unit_Name)
     return RPC.Partition_ID;
   --  Return the Partition_ID of the given shared passive partition.

   function Get_RCI_Package_Receiver
     (Name : Unit_Name)
      return RPC.RPC_Receiver;
   --  Similar in some respects to RCI_Info.Get_RCI_Package_Receiver.

   function Get_Active_Version
      (Name : Unit_Name)
       return String;
   --  Similar in some respects to Get_Active_Partition_ID.

   procedure Register_Receiving_Stub
     (Name     : in Unit_Name;
      Receiver : in RPC.RPC_Receiver;
      Version  : in String := "");
   --  Register the fact that the Name receiving stub is now elaborated.
   --  Register the access value to the package RPC_Receiver procedure.

   procedure Invalidate_Receiving_Stub
     (Name     : in Unit_Name);
   --  Declare this receiving stub as corrupted to the RCI Name Server.

   generic
      Name : String;
   package RCI_Info is
      function Get_RCI_Package_Receiver return System.RPC.RPC_Receiver;
      function Get_Active_Partition_ID  return System.RPC.Partition_ID;
   end RCI_Info;
   --  RCI package information caching.

end System.Partition_Interface;
