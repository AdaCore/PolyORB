------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.ORB_CONTROLLER.HALF_SYNC_HALF_ASYNC                --
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

--  Half-Sync/Half-Async ORB Controller for PolyORB ORB main loop

--  It is a tasking ORB Controller implementation, it supports
--  multi-tasking ORB only.

--  Under this implementation, a set of dedicated tasks monitor AES,
--  the other tasks process requests.

package PolyORB.ORB_Controller.Half_Sync_Half_Async is

   type ORB_Controller_Half_Sync_Half_Async is new ORB_Controller with private;

   type ORB_Controller_Half_Sync_Half_Async_Access is
     access all ORB_Controller_Half_Sync_Half_Async'Class;

   overriding procedure Notify_Event
     (O : access ORB_Controller_Half_Sync_Half_Async;
      E :        Event);

   overriding procedure Schedule_Task
     (O  : access ORB_Controller_Half_Sync_Half_Async;
      TI : PTI.Task_Info_Access;
      SL : PTM.Mutex_Access);

   overriding procedure Disable_Polling
     (O : access ORB_Controller_Half_Sync_Half_Async;
      M : PAE.Asynch_Ev_Monitor_Access);

   overriding procedure Enable_Polling
     (O : access ORB_Controller_Half_Sync_Half_Async;
      M : PAE.Asynch_Ev_Monitor_Access);

   type ORB_Controller_Half_Sync_Half_Async_Factory is
     new ORB_Controller_Factory with private;

   overriding function Create
     (OCF : ORB_Controller_Half_Sync_Half_Async_Factory)
     return ORB_Controller_Access;

private

   --  Under this ORB controller implementation, a set of dedicated
   --  tasks monitor AEM, this structure stores their information.

   type Monitoring_Task_Control is record
      Job_Queue : PJ.Job_Queue_Access;
      --  Specific job queue of jobs to be processed by the blocked task.
      --  XXX replace it by an array ?

      CV : PTCV.Condition_Access;

      Idle : Boolean := False;
   end record;

   type MTC_Array is array (Natural range <>) of Monitoring_Task_Control;

   type ORB_Controller_Half_Sync_Half_Async is new ORB_Controller with record
      Monitoring_Tasks : MTC_Array (1 .. Maximum_Number_Of_Monitors);

   end record;

   type ORB_Controller_Half_Sync_Half_Async_Factory is
     new ORB_Controller_Factory with null record;

   OCF : constant ORB_Controller_Factory_Access
     := new ORB_Controller_Half_Sync_Half_Async_Factory;

end PolyORB.ORB_Controller.Half_Sync_Half_Async;
