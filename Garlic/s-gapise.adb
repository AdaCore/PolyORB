------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . P I D _ S E R V E R              --
--                                                                          --
--                                 B o d y                                  --
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

with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.Soft_Links; use System.Garlic.Soft_Links;

package body System.Garlic.PID_Server is

   use System.Garlic.Types;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GAPISE", "(s-gapise): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Latest_Partition : Valid_Partition_ID := Server_Partition_ID;

   ---------------------------
   -- Allocate_Partition_ID --
   ---------------------------

   function Allocate_Partition_ID return Partition_ID is
      Partition : Partition_ID;
   begin
      Enter_Critical_Section;
      Latest_Partition := Latest_Partition + 1;
      Partition := Latest_Partition;
      Leave_Critical_Section;
      pragma Debug (D (D_Server,
                       "Allocating partition" & Partition'Img));
      return Partition;
   end Allocate_Partition_ID;

   -----------------------------------
   -- Latest_Allocated_Partition_ID --
   -----------------------------------

   function Latest_Allocated_Partition_ID return Partition_ID is
   begin
      return Latest_Partition;
   end Latest_Allocated_Partition_ID;

end System.Garlic.PID_Server;
