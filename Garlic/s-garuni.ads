------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U N I T S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with Interfaces;
with System.Garlic.Exceptions;
with System.Garlic.Types;

package System.Garlic.Units is

   pragma Elaborate_Body;

   --  This package needs comments ???

   procedure Get_Partition
     (Unit      : in Types.Unit_Id;
      Partition : out Types.Partition_ID;
      Error     : in out Exceptions.Error_Type);

   procedure Get_Receiver
     (Unit     : in Types.Unit_Id;
      Receiver : out Interfaces.Unsigned_64;
      Error    : in out Exceptions.Error_Type);

   procedure Get_Version
     (Unit    : in Types.Unit_Id;
      Version : out Types.Version_Type;
      Error   : in out Exceptions.Error_Type);

   function Get_Unit_Id (Name : String) return Types. Unit_Id;

   procedure Initialize;

   procedure Invalidate_Partition_Units
     (Partition : in Types.Partition_ID);
   --  Invalidate all the units configured on this partition. The exact
   --  invalidation will depend on the reconnection mode of this
   --  partition. When reconnection mode is Reject_On_Restart or
   --  Fail_Until_Restart, the status of these units will be set to
   --  Invalid. Otherwise, it will be set to Undefined.

   procedure Register_Unit
     (Partition : in Types.Partition_ID;
      Name      : in String;
      Receiver  : in Interfaces.Unsigned_64;
      Version   : in Types.Version_Type);
   --  Register locally this unit. The remote registration is
   --  postponed and will be performed by Register_Units_On_Boot_Server.

   procedure Register_Units_On_Boot_Server
     (Partition : in Types.Partition_ID;
      Error    : in out Exceptions.Error_Type);
   --  Register all the units previously declared by partition. Then,
   --  get back info on these units to check that these units are
   --  valid.

   procedure Shutdown;
   --  Resume tasks waiting for an update of units info table.

end System.Garlic.Units;
