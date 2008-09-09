------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B _ C O N T R O L L E R . W O R K E R S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
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
