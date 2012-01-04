------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . P A R T I T I O N _ E L A B O R A T I O N         --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2005;

pragma Warnings (Off);

with PolyORB.Exceptions;
pragma Elaborate_All (PolyORB.Exceptions);

with PolyORB.DSA_P.Exceptions;
pragma Elaborate_All (PolyORB.DSA_P.Exceptions);

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.Parameters;
pragma Elaborate_All (PolyORB.Parameters);

with PolyORB.ORB;
pragma Elaborate_All (PolyORB.ORB);

with PolyORB.ORB_Controller.Workers;
pragma Elaborate_All (PolyORB.ORB_Controller.Workers);

with PolyORB.POA;
pragma Elaborate_All (PolyORB.POA);

with PolyORB.QoS;
pragma Elaborate_All (PolyORB.QoS);

with PolyORB.QoS.Term_Manager_Info;
pragma Elaborate_All (PolyORB.QoS.Term_Manager_Info);

with PolyORB.Services.Naming;
pragma Elaborate_All (PolyORB.Services.Naming);

with PolyORB.Services.Naming.Helper;
pragma Elaborate_All (PolyORB.Services.Naming.Helper);

with PolyORB.Services.Naming.NamingContext.Client;
pragma Elaborate_All (PolyORB.Services.Naming.NamingContext.Client);

with PolyORB.Setup.OA.Basic_POA;
pragma Elaborate_All (PolyORB.Setup.OA.Basic_POA);

with PolyORB.Termination_Activity;
pragma Elaborate_All (PolyORB.Termination_Activity);

package PolyORB.Partition_Elaboration is
   pragma Elaborate_Body;

   procedure Configure
     (Set_Conf : access procedure (Section, Key, Value : String));
   --  Set various runtime configuration parameters using the provided callback

   procedure Full_Launch;
   --  Launch the slave partitions when using Ada Starter

   procedure Run_Additional_Tasks;
   --  Run needed additional tasks according to selected
   --  ORB tasking policy.

   --  The body of this package provides further partition-specific
   --  dependencies that are guaranteed to be elaborated before PCS
   --  initialization, in addition to the Full_Launch starter.

end PolyORB.Partition_Elaboration;
