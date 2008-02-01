------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              X E _ L I S T                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains all the routines to parse the GNATLS outputs
--  and to load the ALI files.

with XE_Types; use XE_Types;

package XE_List is

   procedure Register_Unit_To_Load (Uname : Unit_Name_Type);

   procedure Load_All_Registered_Units;
      --  All unit names and file names are entered into the Names
      --  table. The Info and Byte fields of these entries are used as
      --  follows:
      --
      --    Unit name           Info field has Unit_Id
      --                        Byte fiels has Partition_Id (*)
      --    Conf. unit name     Info field has ALI_Id
      --                        Byte fiels has Partition_Id (*)
      --    ALI file name       Info field has ALI_Id
      --    Source file name    Info field has ALI_Id
      --
      --  (*) A (normal, RT) unit may be assigned to several partitions.

      --  We want to detect whether these configured units are real
      --  ada units. Set the configured unit name to No_ALI_Id. When
      --  we load an ali file, its unit name is set to its ali id. If
      --  a configured unit name has no ali id, it is not an Ada unit.
      --  Assign byte field of configured unit name to No_Partition_Id
      --  in order to detect units that are multiply assigned.

   procedure Initialize;

end XE_List;
