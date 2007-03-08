------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . P A R T I T I O N _ E L A B O R A T I O N         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with PolyORB.Parameters.Partition;
pragma Elaborate_All (PolyORB.Parameters.Partition);

with PolyORB.ORB;
pragma Elaborate_All (PolyORB.ORB);

with PolyORB.ORB_Controller.Workers;
pragma Elaborate_All (PolyORB.ORB_Controller.Workers);

with PolyORB.Parameters;
pragma Elaborate_All (PolyORB.Parameters);

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

with PolyORB.Setup.Base;
pragma Elaborate_All (PolyORB.Setup.Base);

with PolyORB.Setup.OA.Basic_POA;
pragma Elaborate_All (PolyORB.Setup.OA.Basic_POA);

with PolyORB.Termination_Activity;
pragma Elaborate_All (PolyORB.Termination_Activity);

package PolyORB.Partition_Elaboration is
   pragma Elaborate_Body;

   procedure Full_Launch;
   --  Launch the slave partitions when using Ada Starter

   --  We elaborate the body, which contains the distributed specific
   --  elaboration code.
end PolyORB.Partition_Elaboration;
