------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . O R B _ C O N T R O L L E R . N O _ T A S K I N G     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  No Tasking ORB Controller for PolyORB ORB main loop.

--  This ORB Controller is dedicated to partition WITHOUT tasking.

--  It supports:
--   * mono tasking ORB only
--   * one asynchronous event monitor

--  $Id$

package PolyORB.ORB_Controller.No_Tasking is

   type ORB_Controller_No_Tasking is new ORB_Controller with private;

   type ORB_Controller_No_Tasking_Access is
     access all ORB_Controller_No_Tasking'Class;

   procedure Register_Task
     (O  : access ORB_Controller_No_Tasking;
      TI :        PTI.Task_Info_Access);

   procedure Unregister_Task
     (O  : access ORB_Controller_No_Tasking;
      TI :        PTI.Task_Info_Access);

   procedure Notify_Event
     (O : access ORB_Controller_No_Tasking;
      E :        Event);

   procedure Schedule_Task
     (O  : access ORB_Controller_No_Tasking;
      TI :        PTI.Task_Info_Access);

   procedure Disable_Polling (O : access ORB_Controller_No_Tasking);

   procedure Enable_Polling (O : access ORB_Controller_No_Tasking);

   procedure Enter_ORB_Critical_Section (O : access ORB_Controller_No_Tasking);

   procedure Leave_ORB_Critical_Section (O : access ORB_Controller_No_Tasking);

   function Is_A_Job_Pending
     (O : access ORB_Controller_No_Tasking)
     return Boolean;

   function Get_Pending_Job
     (O : access ORB_Controller_No_Tasking)
     return PJ.Job_Access;

   function Get_Monitors
     (O : access ORB_Controller_No_Tasking)
     return Monitor_Array;

   type ORB_Controller_No_Tasking_Factory is
     new ORB_Controller_Factory with private;

   function Create
     (OCF : access ORB_Controller_No_Tasking_Factory)
     return ORB_Controller_Access;

private

   type ORB_Controller_No_Tasking is new ORB_Controller with record

      Job_Queue : PJ.Job_Queue_Access;
      --  The queue of jobs to be processed by ORB tasks

      Monitors : Monitor_Array (1 .. 1) := (others => null);
      --  Monitors to be polled

   end record;

   type ORB_Controller_No_Tasking_Factory is
     new ORB_Controller_Factory with null record;

   OCF : constant ORB_Controller_Factory_Access
     := new ORB_Controller_No_Tasking_Factory;

end PolyORB.ORB_Controller.No_Tasking;
