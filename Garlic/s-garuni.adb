------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U N I T S                   --
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

with System.Garlic.Debug;       use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Elaboration;
pragma Elaborate_All (System.Garlic.Elaboration);
pragma Warnings (Off, System.Garlic.Elaboration);
with System.Garlic.Heart;       use System.Garlic.Heart;
with System.Garlic.Soft_Links;  use System.Garlic.Soft_Links;
with System.Garlic.Types;       use System.Garlic.Types;

package body System.Garlic.Units is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARUNI", "(s-garuni): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   ------------------
   -- Get_RCI_Data --
   ------------------

   procedure Get_RCI_Data
     (Cache     : in Cache_Type;
      Receiver  : out Interfaces.Unsigned_64;
      Partition : out Types.Partition_ID;
      Done      : out Boolean)
   is
   begin
      Enter_Critical_Section;
      if not Cache.Cache_Consistent then
         Done      := False;
         Receiver  := 0;
         Partition := Types.Partition_ID'First;
      else
         Done      := True;
         Receiver  := Cache.Package_Receiver;
         Partition := Cache.Active_Partition;
      end if;
      Leave_Critical_Section;
   end Get_RCI_Data;

   ------------------
   -- Set_RCI_Data --
   ------------------

   procedure Set_RCI_Data
     (Cache     : out Cache_Type;
      Receiver  : in Interfaces.Unsigned_64;
      Partition : in Types.Partition_ID) is
   begin
      Enter_Critical_Section;
      Cache := (Cache_Consistent => True,
                Package_Receiver => Receiver,
                Active_Partition => Partition);
      Leave_Critical_Section;
   end Set_RCI_Data;

end System.Garlic.Units;
