------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . S E T U P . R T C O R B A                 --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Log.Stderr;
pragma Elaborate_All (PolyORB.Log.Stderr);
pragma Warnings (Off, PolyORB.Log.Stderr);

with PolyORB.ORB.Thread_Pool;
pragma Elaborate_All (PolyORB.ORB.Thread_Pool);
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

--  with PolyORB.ORB_Controller.Half_Sync_Half_Async;
--  pragma Warnings (Off, PolyORB.ORB_Controller.Half_Sync_Half_Async);
--  pragma Elaborate_All (PolyORB.ORB_Controller.Half_Sync_Half_Async);
with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);
pragma Elaborate_All (PolyORB.ORB_Controller.Workers);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities;
pragma Elaborate_All
  (PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities);
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads;
pragma Elaborate_All (PolyORB.Tasking.Profiles.Full_Tasking.Threads);
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations;
pragma Elaborate_All
  (PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations);
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations);

with PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
pragma Elaborate_All (PolyORB.Tasking.Profiles.Full_Tasking.Mutexes);
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Mutexes);

with PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
pragma Elaborate_All
  (PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables);
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables);

with PolyORB.Parameters.File;
pragma Warnings (Off, PolyORB.Parameters.File);
pragma Elaborate_All (PolyORB.Parameters.File);

with PolyORB.References.File;
pragma Warnings (Off, PolyORB.References.File);
pragma Elaborate_All (PolyORB.References.File);

with PolyORB.QoS.Priority;
pragma Elaborate_All (PolyORB.QoS.Priority);
pragma Warnings (Off, PolyORB.QoS.Priority);

with PolyORB.Request_Scheduler.Servant_Lane;
pragma Elaborate_All (PolyORB.Request_Scheduler.Servant_Lane);
pragma Warnings (Off, PolyORB.Request_Scheduler.Servant_Lane);

with PolyORB.Setup.OA.Basic_RT_POA;
pragma Warnings (Off, PolyORB.Setup.OA.Basic_RT_POA);
pragma Elaborate_All (PolyORB.Setup.OA.Basic_RT_POA);

with PolyORB.Setup.IIOP;
pragma Elaborate_All (PolyORB.Setup.IIOP);
pragma Warnings (Off, PolyORB.Setup.IIOP);

with PolyORB.Setup.Access_Points.IIOP;
pragma Elaborate_All (PolyORB.Setup.Access_Points.IIOP);
pragma Warnings (Off, PolyORB.Setup.Access_Points.IIOP);

with PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy;
pragma Elaborate_All
  (PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy);
pragma Warnings
  (Off, PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy);

package body PolyORB.Setup.RTCORBA is
end PolyORB.Setup.RTCORBA;
