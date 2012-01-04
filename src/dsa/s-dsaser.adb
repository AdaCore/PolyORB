------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  S Y S T E M . D S A _ S E R V I C E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with PolyORB.DSA_P.Partitions;
pragma Elaborate_All (PolyORB.DSA_P.Partitions);
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.DSA_P.Storages.Config;
pragma Elaborate_All (PolyORB.DSA_P.Storages.Config);
with PolyORB.Termination_Manager.Bootstrap;
pragma Elaborate_All (PolyORB.Termination_Manager.Bootstrap);

package body System.DSA_Services is
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("system.dsa_services");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   use PolyORB.DSA_P.Partitions;
   use System.Partition_Interface;

begin
   --  Check that the PCS is initialized

   pragma Assert (PolyORB.Initialization.Is_Initialized);

   --  Initialize the termination manager

   PolyORB.Termination_Manager.Bootstrap.Initialize_Termination_Manager;

   --  Initialize shared storage supports

   PolyORB.DSA_P.Storages.Config.Initialize_Storages;

   --  Allocate to this partition a local partition ID, unless one has
   --  already been allocated (case of the PID server partition).

   if not Local_PID_Allocated then
      Set_Local_Partition_ID
        (RPC.Partition_ID (Allocate_Partition_ID (Get_Local_Partition_Name)));
   end if;

   --  DSA services are now fully initialized. Incoming remote subprogram calls
   --  will be processed when RPC receivers are activated, once the partition
   --  is completely elaborated (E.5(21)).

   pragma Debug (C, O ("DSA_Services Initialized"));

exception
   when E : others =>
      O ("exception raised during DSA services initialization: "
         & Ada.Exceptions.Exception_Information (E));
      PolyORB.Initialization.Shutdown_World (Wait_For_Completion => False);
      raise;
end System.DSA_Services;
