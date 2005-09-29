------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . O R B _ C O N T R O L L E R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

package body PolyORB.ORB_Controller is

   use PolyORB.Task_Info;

   My_Factory : ORB_Controller_Factory_Access;

   ----------------------
   -- Is_A_Job_Pending --
   ----------------------

   function Is_A_Job_Pending (O : access ORB_Controller) return Boolean is
   begin
      return not PJ.Is_Empty (O.Job_Queue);
   end Is_A_Job_Pending;

   ---------------------
   -- Get_Pending_Job --
   ---------------------

   function Get_Pending_Job (O : access ORB_Controller) return PJ.Job_Access is
   begin
      pragma Assert (Is_A_Job_Pending (O));
      O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs - 1;

      return PJ.Fetch_Job (O.Job_Queue);
   end Get_Pending_Job;

   ------------------
   -- Get_Monitors --
   ------------------

   function Get_Monitors (O : access ORB_Controller) return Monitor_Array is
      use type PAE.Asynch_Ev_Monitor_Access;

   begin
      if O.Monitors (1) /= null then
         return O.Monitors;
      else
         return Monitor_Array'(1 .. 0 => null);
      end if;
   end Get_Monitors;

   ------------
   -- Create --
   ------------

   procedure Create (O : out ORB_Controller_Access) is
   begin
      pragma Assert (My_Factory /= null);

      O := Create (My_Factory);
   end Create;

   -------------------------------------
   -- Register_ORB_Controller_Factory --
   -------------------------------------

   procedure Register_ORB_Controller_Factory
     (OCF : ORB_Controller_Factory_Access)
   is
   begin
      pragma Assert (My_Factory = null);
      My_Factory := OCF;
   end Register_ORB_Controller_Factory;

   ------------
   -- Status --
   ------------

   function Status (O : access ORB_Controller) return String is
   begin
      return "Tot:" & Natural'Image (O.Registered_Tasks)
        & " U:" & Natural'Image (O.Counters (Unscheduled))
        & " R:" & Natural'Image (O.Counters (Running))
        & " B:" & Natural'Image (O.Counters (Blocked))
        & " I:" & Natural'Image (O.Counters (Idle))
        & "| PJ:" & Natural'Image (O.Number_Of_Pending_Jobs)
        & " AES:" & Natural'Image (O.Number_Of_AES);
   end Status;

   -----------------------------------
   -- ORB_Controller_Counters_Valid --
   -----------------------------------

   function ORB_Controller_Counters_Valid
     (O : access ORB_Controller)
     return Boolean
   is
   begin
      return O.Registered_Tasks =
        O.Counters (Unscheduled)
        +  O.Counters (Idle)
        +  O.Counters (Running)
        +  O.Counters (Blocked)
        +  O.Counters (Terminated);
   end ORB_Controller_Counters_Valid;

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task
     (O  : access ORB_Controller;
      TI :        PTI.Task_Info_Access)
   is
   begin
      pragma Debug (O1 ("Register_Task: enter"));
      pragma Assert (State (TI.all) = Unscheduled);

      Notify_Event (ORB_Controller'Class (O.all)'Access,
        Event'(Kind => Task_Registered, Registered_Task => TI));

      pragma Debug (O2 (Status (O)));
      pragma Debug (O1 ("Register_Task: leave"));
   end Register_Task;

   ---------------------
   -- Unregister_Task --
   ---------------------

   procedure Unregister_Task
     (O  : access ORB_Controller;
      TI :        PTI.Task_Info_Access)
   is
   begin
      pragma Debug (O1 ("Unregister_Task: enter"));
      pragma Assert (State (TI.all) = Terminated);

      Notify_Event (ORB_Controller'Class (O.all)'Access, Task_Unregistered_E);

      pragma Debug (O2 (Status (O)));
      pragma Debug (O1 ("Unregister_Task: leave"));
   end Unregister_Task;

   --------------------------
   -- Get_Idle_Tasks_Count --
   --------------------------

   function Get_Idle_Tasks_Count
     (O : ORB_Controller_Access)
     return Natural is
   begin
      return O.Counters (Idle);
   end Get_Idle_Tasks_Count;

end PolyORB.ORB_Controller;
