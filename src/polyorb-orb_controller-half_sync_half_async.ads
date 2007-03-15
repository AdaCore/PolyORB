------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.ORB_CONTROLLER.HALF_SYNC_HALF_ASYNC                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

--  Half-Sync/Half-Async ORB Controller for PolyORB ORB main loop

--  It is a tasking ORB Controller implementation, it supports
--  multi-tasking ORB only.

--  Under this implementation, a set of dedicated tasks monitor AES,
--  the other tasks process requests.

package PolyORB.ORB_Controller.Half_Sync_Half_Async is

   type ORB_Controller_Half_Sync_Half_Async is new ORB_Controller with private;

   type ORB_Controller_Half_Sync_Half_Async_Access is
     access all ORB_Controller_Half_Sync_Half_Async'Class;

   procedure Notify_Event
     (O : access ORB_Controller_Half_Sync_Half_Async;
      E :        Event);

   procedure Schedule_Task
     (O  : access ORB_Controller_Half_Sync_Half_Async;
      TI :        PTI.Task_Info_Access);

   procedure Disable_Polling
     (O : access ORB_Controller_Half_Sync_Half_Async;
      M : PAE.Asynch_Ev_Monitor_Access);

   procedure Enable_Polling
     (O : access ORB_Controller_Half_Sync_Half_Async;
      M : PAE.Asynch_Ev_Monitor_Access);

   type ORB_Controller_Half_Sync_Half_Async_Factory is
     new ORB_Controller_Factory with private;

   function Create
     (OCF : access ORB_Controller_Half_Sync_Half_Async_Factory)
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
