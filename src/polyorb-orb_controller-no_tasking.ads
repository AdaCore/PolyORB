------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . O R B _ C O N T R O L L E R . N O _ T A S K I N G     --
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

--  No Tasking ORB Controller for PolyORB ORB main loop.

--  This ORB Controller is dedicated to partition WITHOUT tasking, it
--  supports: mono tasking ORB only

package PolyORB.ORB_Controller.No_Tasking is

   type ORB_Controller_No_Tasking is new ORB_Controller with private;

   type ORB_Controller_No_Tasking_Access is
     access all ORB_Controller_No_Tasking'Class;

   overriding procedure Notify_Event
     (O : access ORB_Controller_No_Tasking;
      E :        Event);

   overriding procedure Schedule_Task
     (O  : access ORB_Controller_No_Tasking;
      TI : PTI.Task_Info_Access;
      SL : PTM.Mutex_Access);

   overriding procedure Disable_Polling
     (O : access ORB_Controller_No_Tasking;
      M : PAE.Asynch_Ev_Monitor_Access);

   overriding procedure Enable_Polling
     (O : access ORB_Controller_No_Tasking;
      M : PAE.Asynch_Ev_Monitor_Access);

   type ORB_Controller_No_Tasking_Factory is
     new ORB_Controller_Factory with private;

   overriding function Create
     (OCF : ORB_Controller_No_Tasking_Factory) return ORB_Controller_Access;

private

   type ORB_Controller_No_Tasking is new ORB_Controller with null record;

   type ORB_Controller_No_Tasking_Factory is
     new ORB_Controller_Factory with null record;

   OCF : constant ORB_Controller_Factory_Access
     := new ORB_Controller_No_Tasking_Factory;

end PolyORB.ORB_Controller.No_Tasking;
