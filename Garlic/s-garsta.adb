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

--  Here, you must 'with' and 'pragma Elaborate_All' all the protocols you
--  want to use. Look for other sections called "Protocol section" below
--  to make sure you initialize the protocols you want to use.

with System.Garlic.Debug; use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);

with System.Garlic.Protocols; use System.Garlic.Protocols;
pragma Elaborate_All (System.Garlic.Protocols);

with System.Garlic.Protocols.Config;
pragma Elaborate_All (System.Garlic.Protocols.Config);

with System.Garlic.Options; use System.Garlic.Options;
pragma Elaborate_All (System.Garlic.Options);

with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
pragma Elaborate_All (System.Garlic.Physical_Location);

with System.Garlic.Heart; use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);

with System.Garlic.Termination; use System.Garlic.Termination;
pragma Elaborate_All (System.Garlic.Termination);

with System.Garlic.Types; use System.Garlic.Types;
pragma Elaborate_All (System.Garlic.Types);

with System.Garlic.Services;
pragma Elaborate_All (System.Garlic.Services);

with System.Garlic.Filters;
pragma Elaborate_All (System.Garlic.Filters);

with System.Garlic.Trace;
pragma Elaborate_All (System.Garlic.Trace);

with System.RPC.Initialization;
pragma Elaborate_All (System.RPC.Initialization);

with System.Garlic.Elaboration;
pragma Elaborate_All (System.Garlic.Elaboration);

package body System.Garlic.Startup is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("STARTUP", "(s-garsta): ");
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
   System.Garlic.Protocols.Config.Initialize;

   declare
      Boot_Location : constant Location_Type
        := To_Location (Options.Boot_Server.all);
      Boot_Protocol : constant Protocol_Access := Get_Protocol (Boot_Location);
      Boot_Data     : constant String := Get_Data (Boot_Location);
      Is_Master     : constant Boolean := not Options.Is_Slave;
   begin

      if Boot_Protocol = null then
         pragma Debug (D (D_Elaborate, "No boot protocol, aborting"));
         raise System.RPC.Communication_Error;
      end if;

      --  Phase (3) (see s-garlic.ads)

      Set_Is_Boot_Partition (Is_Master);

      --  Phase (4) (see s-garlic.ads)

      for I in Config.Protocol_Table'Range loop
         exit when Config.Protocol_Table (I) = null;
         if Config.Protocol_Table (I) = Boot_Protocol then
            Set_Boot_Data (Boot_Protocol, True, Boot_Data, Is_Master);
            Set_Is_Boot_Partition (Is_Master);
            Set_My_Location
              (To_Location (Boot_Protocol, Get_Info (Boot_Protocol)));
            if Is_Master then
               Set_Boot_Location
                 (To_Location (Boot_Protocol, Get_Info (Boot_Protocol)));
            else
               Set_Boot_Location (To_Location (Boot_Protocol, Boot_Data));
            end if;
         else
            Set_Boot_Data (Config.Protocol_Table (I));
         end if;
      end loop;

      --  Phase (5)

      Filters.Initialize;

      --  Phase (6)

      Trace.Initialize;

      --  Phase (7) (see s-garlic.ads)

      System.RPC.Initialization.Initialize;
      Termination.Initialize;

   end;

   --  Phase (8) (see s-garlic.ads)

   declare
      --  First, let boot server know about this partition

      pragma Warnings (Off);
      P : constant Types.Partition_ID := Get_My_Partition_ID;
      pragma Warnings (On);

      --  Then, let this partition know about boot server

      pragma Warnings (Off);
      D : constant Partition_Data := Get_Partition_Data (Get_Boot_Server);
      pragma Warnings (On);
   begin
      null;
   end;

   pragma Debug (D (D_Elaborate, "Startup phase terminated"));

end System.Garlic.Startup;

