------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . S E T U P . R T C O R B A                 --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
