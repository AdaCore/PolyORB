------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B _ C O N T R O L L E R . W O R K E R S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

--  Workers ORB Controller for PolyORB ORB main loop.

--  It is an all-purpose ORB Controller implementation, it supports:
--  multi-tasking and mono-tasking ORB.

package PolyORB.ORB_Controller.Workers is

   type ORB_Controller_Workers is new ORB_Controller with private;

   type ORB_Controller_Workers_Access is
     access all ORB_Controller_Workers'Class;

   procedure Notify_Event
     (O : access ORB_Controller_Workers;
      E :        Event);

   procedure Schedule_Task
     (O  : access ORB_Controller_Workers;
      TI :        PTI.Task_Info_Access);

   procedure Disable_Polling
     (O : access ORB_Controller_Workers;
      M : PAE.Asynch_Ev_Monitor_Access);

   procedure Enable_Polling
     (O : access ORB_Controller_Workers;
      M : PAE.Asynch_Ev_Monitor_Access);

   type ORB_Controller_Workers_Factory is
     new ORB_Controller_Factory with private;

   function Create
     (OCF : ORB_Controller_Workers_Factory) return ORB_Controller_Access;

private

   type ORB_Controller_Workers is new ORB_Controller with null record;

   type ORB_Controller_Workers_Factory is
     new ORB_Controller_Factory with null record;

   OCF : constant ORB_Controller_Factory_Access
     := new ORB_Controller_Workers_Factory;

end PolyORB.ORB_Controller.Workers;
