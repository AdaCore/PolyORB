------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--              S Y S T E M . P A R T I T I O N _ T Y P ES                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            1.8                             --
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

package System.Partition_Types is

   pragma Remote_Types;

   subtype Unit_Name is String;

   type Version_ID   is new String (1 .. 8);
   --  This type should match Version_String (GNAT/s-vercon.ads)

   type Partition_ID is range 0 .. 63;
   --  This type should match System.RPC.Partition_ID.

   subtype RPC_Receiver is System.RPC.RPC_Receiver;
   --  type RPC_Receiver is access Integer;
   --  XXXXX : type RPC_Receiver is access procedure; (pbs with RT)
   --  This definition cannot match RPC_Receiver in System.RPC.

   Null_PID : constant Partition_ID := Partition_ID'First;
   Null_RCV : constant RPC_Receiver := null;
   Null_VID : constant Version_ID   := (others => Ascii.Nul);

end System.Partition_Types;
