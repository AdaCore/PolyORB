------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . O P T I O N S                  --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

package System.Garlic.Options is

   function Get_Boot_Server return String;
   --  Return value specified by --boot_server command line arg or else
   --         value specified by "BOOT_SERVER" environment var or else
   --         Default if no such environment variable.

   function Get_Connection_Hits return Natural;
   --  Return value specified by --connection_hits command line arg or else
   --         value specified by "CONNECTION_HITS" environment var or else
   --         Default if no such environment variable.

   function Get_Detach return Boolean;
   --  Return True if --detach is present on the command line arg or if
   --  a DETACH environment variable with a non empty value is present.

   function Get_Nolaunch return Boolean;
   --  Return True if --nolaunch is present on the command line arg or if
   --  a NOLAUNCH environment variable with a non empty value is present.

   function Get_Is_Slave return Boolean;
   --  Return True if --slave is present on the command line arg or if
   --  a SLAVE environment variable with a non empty value is present.

   procedure Set_Boot_Server (Default : String);

   procedure Set_Connection_Hits (Default : Natural);

   procedure Set_Detach   (Default : Boolean);

   procedure Set_Nolaunch (Default : Boolean);

   procedure Set_Is_Slave (Default : Boolean);

end System.Garlic.Options;

