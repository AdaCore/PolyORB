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
     (RCI_Name : Unit_Name)
      return RPC.Partition_ID;
   --  Similar in some respects to RCI_Info.Get_Active_Partition_ID.

   function Get_Passive_Partition_ID
     (RCI_Name : Unit_Name)
     return RPC.Partition_ID;
   --  Return the Partition_ID of the given shared passive partition.

   function Get_RCI_Package_Receiver
     (RCI_Name : in Unit_Name)
      return RPC.RPC_Receiver;
   --  Similar in some respects to RCI_Info.Get_RCI_Package_Receiver.

   function Get_Active_Version
      (RCI_Name : in Unit_Name)
       return String;
   --  Similar in some respects to Get_Active_Partition_ID.

   protected type Elaboration_Type is
      procedure Get_RCI_Data
        (Receiver  : out System.RPC.RPC_Receiver;
         Partition : out System.RPC.Partition_ID;
         Done      : out Boolean);
      procedure Set_RCI_Data
        (RCI_Name  : in Unit_Name_Access;
         Receiver  : in System.RPC.RPC_Receiver;
         Partition : in System.RPC.Partition_ID);
      procedure Delete_RCI_Data;
      function  Get_RCI_Name return Unit_Name_Access;
   private
      Active_Partition : System.Rpc.Partition_ID;
      Package_Receiver : System.Rpc.Rpc_Receiver;
      RCI_Package_Name : Unit_Name_Access;
   end Elaboration_Type;
   type Elaboration_Access is access Elaboration_Type;
   --  Protected type provided for future implementation of restartable
   --  partitions.

   procedure Register_Receiving_Stub
     (RCI_Name : in Unit_Name;
      Receiver : in RPC.RPC_Receiver;
      Version  : in String := "");
   --  Register the fact that the RCI_Name receiving stub is now
   --  elaborated.  Register the access value to the package RPC_Receiver
   --  procedure.

   procedure Register_Calling_Stub
     (RCI_Name     : in Unit_Name;
      Partition    : in RPC.Partition_ID;
      Elaboration  : in Elaboration_Access);
   --  Set an access-to-subprogram that is called as soon as the
   --  connection is detected as broken.

   procedure Invalidate_Receiving_Stub
     (RCI_Name  : in Unit_Name;
      Partition : in RPC.Partition_ID);
   --  Declare this receiving stub as corrupted on this partition to
   --  the RCI Name Server.

   function Get_Active_Partition_ID
     (RCI_Name    : in Unit_Name_Access;
      Elaboration : in Elaboration_Access)
      return System.Rpc.Partition_ID;
   --  Similar to previous Get_Active_Partition_ID,
   --  but uses a protected type.

   function Get_RCI_Package_Receiver
     (RCI_Name    : in Unit_Name_Access;
      Elaboration : in Elaboration_Access)
      return System.RPC.RPC_Receiver;
   --  Similar to previous Get_RCI_Package_Receiver,
   --  but uses a protected type.

   generic
      RCI_Name : String;
   package RCI_Info is
      function Get_RCI_Package_Receiver return System.RPC.RPC_Receiver;
      function Get_Active_Partition_ID  return System.RPC.Partition_ID;
   end RCI_Info;
   --  RCI package information caching.

end System.Partition_Interface;
