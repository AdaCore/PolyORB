------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--                            R C I _ I N F O                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            1.7                             --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with System.RPC;
with System.Partition_Interface;

package body RCI_Info is

   Elaboration : System.Partition_Interface.Elaboration_Access
               := new System.Partition_Interface.Elaboration_Type;

   -----------------------------
   -- Get_Active_Partition_Id --
   -----------------------------

   function Get_Active_Partition_Id return System.Rpc.Partition_Id is
   begin
      return System.Partition_Interface.Get_Active_Partition_Id
        (RCI_Name, Elaboration);
   end Get_Active_Partition_Id;

   ------------------------------
   -- Get_Rci_Package_Receiver --
   ------------------------------

   function Get_Rci_Package_Receiver return System.Rpc.Rpc_Receiver is
   begin
      return System.Partition_Interface.Get_Rci_Package_Receiver
        (RCI_Name, Elaboration);
   end Get_Rci_Package_Receiver;

end RCI_Info;
