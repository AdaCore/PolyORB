------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.ORB_CONTROLLER.LEADER_FOLLOWERS                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2014, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

--  Leader/Followers ORB Controller for PolyORB ORB main loop.

--  It is a tasking ORB Controller implementation, it supports:
--  multi-tasking ORB only

--  Under this implementation, a task handles the complete processing
--  of an incoming request: from monitors polling up to dispatching
--  the request to servant code.

package PolyORB.ORB_Controller.Leader_Followers is

   type ORB_Controller_Leader_Followers is new ORB_Controller with private;

   type ORB_Controller_Leader_Followers_Access is
     access all ORB_Controller_Leader_Followers'Class;

   overriding procedure Notify_Event
     (O : access ORB_Controller_Leader_Followers;
      E :        Event);

   overriding procedure Schedule_Task
     (O  : access ORB_Controller_Leader_Followers;
      TI : PTI.Task_Info_Access;
      SL : PTM.Mutex_Access);

   overriding procedure Register_Task
     (O  : access ORB_Controller_Leader_Followers;
      TI :        PTI.Task_Info_Access);

   overriding procedure Disable_Polling
     (O : access ORB_Controller_Leader_Followers;
      M : PAE.Asynch_Ev_Monitor_Access);

   overriding procedure Enable_Polling
     (O : access ORB_Controller_Leader_Followers;
      M : PAE.Asynch_Ev_Monitor_Access);

   type ORB_Controller_Leader_Followers_Factory is
     new ORB_Controller_Factory with private;

   overriding function Create
     (OCF : ORB_Controller_Leader_Followers_Factory)
     return ORB_Controller_Access;

private

   type ORB_Controller_Leader_Followers is new ORB_Controller
     with null record;

   type ORB_Controller_Leader_Followers_Factory is
     new ORB_Controller_Factory with null record;

   OCF : constant ORB_Controller_Factory_Access
     := new ORB_Controller_Leader_Followers_Factory;

end PolyORB.ORB_Controller.Leader_Followers;
