------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . S T A R T U P                 --
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

with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Elaboration;
pragma Elaborate_All (System.Garlic.Elaboration);
with System.Garlic.Filters;
with System.Garlic.Heart;             use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Options;           use System.Garlic.Options;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
pragma Elaborate_All (System.Garlic.Physical_Location);
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Protocols.Config;
with System.Garlic.Services;
pragma Elaborate_All (System.Garlic.Services);
with System.Garlic.Soft_Links;
with System.Garlic.Trace;
pragma Elaborate_All (System.Garlic.Trace);
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Units;
pragma Elaborate_All (System.Garlic.Units);
with System.Garlic.Utils;             use System.Garlic.Utils;

package body System.Garlic.Startup is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARSTA", "(s-garsta): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

begin

   pragma Debug (D (D_Elaborate, "Entering partition startup phase"));

   --  Phase (0) (see s-garlic.ads)

   System.Garlic.Options.Initialize;
   System.Garlic.Elaboration.Initialize;

   --  Phase (1) (see s-garlic.ads)

   System.Garlic.Services.Initialize;

   --  Phase (2) (see s-garlic.ads)

   System.Garlic.Heart.Initialize;

   --  Phase (30 (see s-garlic.ads)

   System.Garlic.Protocols.Config.Initialize;

   declare
      Boot_Location : constant Location_Type  := To_Location (Boot_Server.all);
      Boot_Protocol : constant Protocol_Access := Get_Protocol (Boot_Location);
      Boot_Data     : constant String_Access  := Get_Data (Boot_Location);
   begin

      if Boot_Protocol = null then
         pragma Debug (D (D_Elaborate, "No boot protocol, aborting"));
         raise Program_Error;
      end if;

      --  Phase (4) (see s-garlic.ads)

      for I in Config.Protocol_Table'Range loop
         exit when Config.Protocol_Table (I) = null;
         if Config.Protocol_Table (I) = Boot_Protocol then

            --  This call initializes the boot protocol with the data
            --  returned by Options.Boot_Server. For the boot partition,
            --  Boot_Data is incomplete, it will be updated. For a non
            --  boot partition, the location used for the boot protocol
            --  is computed. Note that this may be wrong in a multiple
            --  protocols context.

            Initialize (Boot_Protocol, Boot_Data, True);

            --  Get the location used by the boot protocol for this
            --  partition and store it internally in Heart.

            if Boot_Partition then
               Set_Boot_Location
                 (To_Location (Boot_Protocol, Get_Info (Boot_Protocol)));
            else
               Set_Boot_Location
                 (To_Location (Boot_Protocol, Boot_Data.all));
            end if;
         else
            Initialize (Config.Protocol_Table (I));
         end if;
      end loop;
   end;

   --  Phase (5) (see s-garlic.ads)

   Filters.Initialize;

   Units.Initialize;

   --  Phase (6) (see s-garlic.ads)

   Trace.Initialize;

   --  Phase (7) (see s-garlic.ads)

   Soft_Links.Termination_Initialize;

   --  Phase (8) (see s-garlic.ads)

   --  Let boot server know about this partition

   if Get_My_Partition_ID = Null_PID then
      raise Program_Error;
   end if;

   pragma Debug (D (D_Elaborate, "Startup phase terminated"));

end System.Garlic.Startup;

