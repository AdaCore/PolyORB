------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U N I T S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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
with System.Garlic.Table;
with System.Garlic.Heart;
with System.Garlic.Utils;
with System.RPC;

package System.Garlic.Units is

   type Unit_Id is new Natural;
   Null_Unit_Id : constant Unit_Id := 0;

   type Request_List is array (RPC.Partition_ID) of Boolean;
   type Request_Id is (Get_Unit, Set_Unit);

   protected type Cache_Type is

      procedure Get_RCI_Data
        (Receiver  : out RPC.RPC_Receiver;
         Partition : out RPC.Partition_ID;
         Done      : out Boolean);

      procedure Set_RCI_Data
        (Receiver  : in RPC.RPC_Receiver;
         Partition : in RPC.Partition_ID);

   private

      Cache_Consistent : Boolean := False;
      Active_Partition : RPC.Partition_ID;
      Package_Receiver : RPC.RPC_Receiver;

   end Cache_Type;

   type Cache_Access is access Cache_Type;

   type Unit_Status is (Unknown, Queried, Known);

   type Unit_Type is
      record
         Partition : RPC.Partition_ID;
         Receiver  : RPC.RPC_Receiver;
         Version   : Utils.String_Access;
         Cache     : Cache_Access;
         Status    : Unit_Status;
         Pending   : Boolean;
         Requests  : Request_List;
      end record;

   Null_Unit : constant Unit_Type
     := (Partition => System.Garlic.Heart.Null_Partition_ID,
         Receiver  => null,
         Version   => null,
         Cache     => null,
         Status    => Unknown,
         Pending   => False,
         Requests  => (others => False));

   type Request_Type is
      record
         Command   : Request_Id;
         Partition : RPC.Partition_ID;
         Receiver  : RPC.RPC_Receiver;
         Version   : Utils.String_Access;
         Cache     : Cache_Access;
      end record;

   Null_Request : constant Request_Type
     := (Command   => Get_Unit,
         Partition => System.Garlic.Heart.Null_Partition_ID,
         Receiver  => null,
         Version   => null,
         Cache     => null);

   package Units is new System.Garlic.Table.Concurrent
     (Index_Type     => Unit_Id,
      Initial_Size   => 20,
      Increment_Size => 20,
      Component_Type => Unit_Type,
      Null_Component => Null_Unit,
      Parameter_Type => Request_Type);

end System.Garlic.Units;
