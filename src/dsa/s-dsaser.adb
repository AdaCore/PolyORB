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
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
