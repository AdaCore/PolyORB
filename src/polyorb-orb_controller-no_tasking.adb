------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . O R B _ C O N T R O L L E R . N O _ T A S K I N G     --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Constants;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Utils.Strings;

package body PolyORB.ORB_Controller.No_Tasking is

   use PolyORB.Log;
   use PolyORB.Task_Info;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.orb_controller.no_tasking");
   procedure O1 (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   package L2 is
      new PolyORB.Log.Facility_Log ("polyorb.orb_controller_status");
   procedure O2 (Message : in String; Level : Log_Level := Debug)
     renames L2.Output;

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task
     (O  : access ORB_Controller_No_Tasking;
      TI :        PTI.Task_Info_Access)
   is
   begin
      pragma Debug (O1 ("Register_Task: enter"));

      O.Registered_Tasks := O.Registered_Tasks + 1;
      O.Unscheduled_Tasks := O.Unscheduled_Tasks + 1;

      pragma Assert (O.Registered_Tasks = 1);
      --  At most one task may be registered

      pragma Assert (May_Poll (TI.all));
      --  Under this implementation, there is only one task
      --  registered by the ORB. This task must poll on AES.

      pragma Debug (O1 ("Register_Task: leave"));
      pragma Debug (O2 (Status (O)));
   end Register_Task;

   ---------------------
   -- Disable_Polling --
   ---------------------

   procedure Disable_Polling (O : access ORB_Controller_No_Tasking)
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);

   begin
      --  Under this implementation, there is at most one task in the
      --  partition. Thus, there cannot be one task polling while
      --  another requests polling to be disabled.

      null;
   end Disable_Polling;

   --------------------
   -- Enable_Polling --
   --------------------

   procedure Enable_Polling (O : access ORB_Controller_No_Tasking)
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);

   begin
      --  Under this implementation, there is at most one task in the
      --  partition. Thus, there cannot be one task polling while
      --  another requests polling to be disabled.

      null;
   end Enable_Polling;

   ------------------
   -- Notify_Event --
   ------------------

   procedure Notify_Event
     (O : access ORB_Controller_No_Tasking;
      E :        Event)
   is
      use type PAE.Asynch_Ev_Monitor_Access;

   begin
      pragma Debug (O1 ("Notify_Event: " & Event_Kind'Image (E.Kind)));

      case E.Kind is

         when End_Of_Check_Sources =>

            --  A task completed polling on a monitor

            O.Blocked_Tasks := O.Blocked_Tasks - 1;
            O.Unscheduled_Tasks := O.Unscheduled_Tasks + 1;

         when Event_Sources_Added =>

            --  An AES has been added to monitored AES list

            O.Number_Of_AES := O.Number_Of_AES + 1;

            if O.Monitors (1) = null then

               --  There was no monitor registred yet, register new monitor

               O.Monitors (1) := E.Add_In_Monitor;

            else

               --  Under this implementation, there can be at most one
               --  monitor. Ensure this assertion is correct.

               pragma Assert (E.Add_In_Monitor = O.Monitors (1));
               null;

            end if;

         when Event_Sources_Deleted =>

            --  An AES has been removed from monitored AES list

            pragma Assert (O.Monitors (1) /= null);
            null;
            --  O.Number_Of_AES := O.Number_Of_AES - 1;

         when Job_Completed =>

            --  A task has completed the execution of a job

            O.Running_Tasks := O.Running_Tasks - 1;
            O.Unscheduled_Tasks := O.Unscheduled_Tasks + 1;

         when ORB_Shutdown =>

            --  ORB shutdiwn has been requested

            O.Shutdown := True;

         when Queue_Event_Job =>

            --  Queueing an event also implies we delete one source

            --  O.Number_Of_AES := O.Number_Of_AES - 1;

            --  Queue event to main job queue

            O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs + 1;
            PJ.Queue_Job (O.Job_Queue, E.Event_Job);

         when Queue_Request_Job =>

            --  XXX Should we allow the use of a request scheduler for
            --  this policy ?

            --  Queue event to main job queue

            O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs + 1;
            PJ.Queue_Job (O.Job_Queue, E.Request_Job);

         when Request_Result_Ready =>

            --  Nothing to do. The task will be notified the next time
            --  it asks for scheduling.

            null;
      end case;

      pragma Debug (O2 (Status (O)));
   end Notify_Event;

   -------------------
   -- Schedule_Task --
   -------------------

   procedure Schedule_Task
     (O  : access ORB_Controller_No_Tasking;
      TI :        PTI.Task_Info_Access)
   is
   begin
      pragma Debug (O1 ("Schedule_Task: enter"));

      pragma Assert (PTI.State (TI.all) = Unscheduled);

      --  Update counters

      O.Unscheduled_Tasks := O.Unscheduled_Tasks - 1;

      --  Recompute TI status

      if Exit_Condition (TI.all)
        or else O.Shutdown
      then
         pragma Debug (O1 ("Task is now terminated"));
         pragma Debug (O2 (Status (O)));

         Set_State_Terminated (TI.all);

      elsif O.Number_Of_Pending_Jobs > 0 then
         O.Running_Tasks := O.Running_Tasks + 1;
         O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs - 1;

         pragma Debug (O1 ("Task is now running a job"));
         pragma Debug (O2 (Status (O)));

         Set_State_Running (TI.all, PJ.Fetch_Job (O.Job_Queue));

      elsif O.Number_Of_AES > 0 then
         O.Blocked_Tasks := O.Blocked_Tasks + 1;

         pragma Debug (O1 ("Task is now blocked"));
         pragma Debug (O2 (Status (O)));

         Set_State_Blocked
           (TI.all,
            O.Monitors (1),
            PolyORB.Constants.Forever);

      end if;
   end Schedule_Task;

   ---------------------
   -- Unregister_Task --
   ---------------------

   procedure Unregister_Task
     (O  : access ORB_Controller_No_Tasking;
      TI :        PTI.Task_Info_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (TI);
      pragma Warnings (On);

   begin
      pragma Debug (O1 ("Unregister_Task: enter"));

      O.Registered_Tasks := O.Registered_Tasks - 1;

      pragma Debug (O2 (Status (O)));
      pragma Debug (O1 ("Unregister_Task: leave"));
   end Unregister_Task;

   --------------------------------
   -- Enter_ORB_Critical_Section --
   --------------------------------

   procedure Enter_ORB_Critical_Section
     (O : access ORB_Controller_No_Tasking)
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);

   begin
      --  Under this implementation, there is at most one task in the
      --  partition. Thus, there is no need for critical section.

      null;
   end Enter_ORB_Critical_Section;

   --------------------------------
   -- Leave_ORB_Critical_Section --
   --------------------------------

   procedure Leave_ORB_Critical_Section
     (O : access ORB_Controller_No_Tasking)
   is
      pragma Warnings (Off);
      pragma Unreferenced (O);
      pragma Warnings (On);

   begin
      --  Under this implementation, there is at most one task in the
      --  partition. Thus, there is no need for critical section.

      null;
   end Leave_ORB_Critical_Section;

   ----------------------
   -- Is_A_Job_Pending --
   ----------------------

   function Is_A_Job_Pending
     (O : access ORB_Controller_No_Tasking)
     return Boolean
   is
   begin
      return not PJ.Is_Empty (O.Job_Queue);
   end Is_A_Job_Pending;

   ---------------------
   -- Get_Pending_Job --
   ---------------------

   function Get_Pending_Job
     (O : access ORB_Controller_No_Tasking)
     return PJ.Job_Access
   is
   begin
      pragma Assert (Is_A_Job_Pending (O));
      O.Number_Of_Pending_Jobs := O.Number_Of_Pending_Jobs - 1;

      return PJ.Fetch_Job (O.Job_Queue);
   end Get_Pending_Job;

   ------------------
   -- Get_Monitors --
   ------------------

   function Get_Monitors
     (O : access ORB_Controller_No_Tasking)
     return Monitor_Array
   is
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

   function Create
     (OCF : access ORB_Controller_No_Tasking_Factory)
     return ORB_Controller_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (OCF);
      pragma Warnings (On);

      OC : ORB_Controller_No_Tasking_Access;
      RS : PRS.Request_Scheduler_Access;

   begin
      PRS.Create (RS);
      OC := new ORB_Controller_No_Tasking (RS);

      OC.Job_Queue := PolyORB.Jobs.Create_Queue;

      return ORB_Controller_Access (OC);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_ORB_Controller_Factory (OCF);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb_controller.no_tasking",
       Conflicts => Empty,
       Depends   => +"orb.no_tasking",
       Provides  => +"orb_controller",
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.ORB_Controller.No_Tasking;
