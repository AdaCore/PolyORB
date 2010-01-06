------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . O R B _ C O N T R O L L E R . N O _ T A S K I N G     --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Tags;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.ORB_Controller.No_Tasking is

   use PolyORB.Task_Info;
   use PolyORB.Asynch_Ev;

   ---------------------
   -- Disable_Polling --
   ---------------------

   procedure Disable_Polling
     (O : access ORB_Controller_No_Tasking;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      pragma Unreferenced (O);
      pragma Unreferenced (M);

   begin
      --  Under this implementation, there is at most one task in the
      --  partition. Thus, there cannot be one task polling while another
      --  requests polling to be disabled.

      null;
   end Disable_Polling;

   --------------------
   -- Enable_Polling --
   --------------------
   procedure Enable_Polling
     (O : access ORB_Controller_No_Tasking;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      pragma Unreferenced (O);
      pragma Unreferenced (M);

   begin
      --  Under this implementation, there is at most one task in the
      --  partition. Thus, there cannot be one task polling while another
      --  requests polling to be disabled.

      null;
   end Enable_Polling;

   ------------------
   -- Notify_Event --
   ------------------

   procedure Notify_Event (O : access ORB_Controller_No_Tasking; E : Event) is
   begin
      pragma Debug (C1, O1 ("Notify_Event: " & Event_Kind'Image (E.Kind)));

      case E.Kind is

         when End_Of_Check_Sources =>
            declare
               AEM_Index : constant Natural := Index (O.all, E.On_Monitor);
            begin
               --  A task completed polling on a monitor

               pragma Debug (C1, O1 ("End of check sources on monitor #"
                                 & Natural'Image (AEM_Index)
                                 & Ada.Tags.External_Tag
                                 (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));

               --  Reset TI

               O.AEM_Infos (AEM_Index).TI := null;
            end;

         when Event_Sources_Added =>
            declare
               AEM_Index : Natural := Index (O.all, E.Add_In_Monitor);
            begin
               if AEM_Index = 0 then
                  --  This monitor was not yet registered, register it
                  pragma Debug (C1, O1 ("Adding new monitor"));

                  for J in O.AEM_Infos'Range loop
                     if O.AEM_Infos (J).Monitor = null then
                        O.AEM_Infos (J).Monitor := E.Add_In_Monitor;
                        AEM_Index := J;
                        exit;
                     end if;
                  end loop;
               end if;
               pragma Debug (C1, O1 ("Added monitor at index:" & AEM_Index'Img
                                 & " " & Ada.Tags.External_Tag
                                 (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));
            end;

         when Event_Sources_Deleted =>

            --  An AES has been removed from monitored AES list

            null;

         when Job_Completed =>

            --  A task has completed the execution of a job

            null;

         when ORB_Shutdown =>

            --  ORB shutdown has been requested

            O.Shutdown := True;

         when Queue_Event_Job =>

            --  Queue event to main job queue

            PJ.Queue_Job (O.Job_Queue, E.Event_Job);

         when Queue_Request_Job =>

            --  XXX Should we allow the use of a request scheduler for
            --  this policy ?

            --  Queue event to main job queue

            PJ.Queue_Job (O.Job_Queue, E.Request_Job);

         when Request_Result_Ready =>

            --  Nothing to do. The task will be notified the next time it asks
            --  for scheduling.

            null;

         when Idle_Awake =>

            --  No task should go idle. Receiving this event denotes an
            --  internal error.

            raise Program_Error;

         when Task_Registered =>
            null;
            --  Under this implementation, there is only one task registered
            --  with the ORB.

         when Task_Unregistered =>
            Note_Task_Unregistered (O);
      end case;

      pragma Debug (C2, O2 (Status (O.all)));
   end Notify_Event;

   -------------------
   -- Schedule_Task --
   -------------------

   procedure Schedule_Task
     (O  : access ORB_Controller_No_Tasking;
      TI : PTI.Task_Info_Access)
   is
   begin
      pragma Debug (C1, O1 ("Schedule_Task: enter"));

      if State (TI.all) = Terminated then
         pragma Debug (C1, O1 ("Schedule_Task: task is terminated"));
         return;
      end if;

      Set_State_Unscheduled (O.Summary, TI.all);

      --  Recompute TI status

      if Exit_Condition (TI.all)
        or else (O.Shutdown
                 and then not Has_Pending_Job (O)
                 and then TI.Kind = Permanent)
      then
         Set_State_Terminated (O.Summary, TI.all);

         pragma Debug (C1, O1 ("Task is now terminated"));
         pragma Debug (C2, O2 (Status (O.all)));

      elsif Has_Pending_Job (O) then
         Set_State_Running (O.Summary, TI.all, PJ.Fetch_Job (O.Job_Queue));

         pragma Debug (C1, O1 ("Task is now running a job"));
         pragma Debug (C2, O2 (Status (O.all)));

      else
         declare
            AEM_Index : constant Natural := Need_Polling_Task (O);
         begin
            pragma Assert (AEM_Index /= 0);

            O.AEM_Infos (AEM_Index).Polling_Scheduled := False;
            O.AEM_Infos (AEM_Index).TI := TI;

            Set_State_Blocked
              (O.Summary,
               TI.all,
               O.AEM_Infos (AEM_Index).Monitor,
               O.AEM_Infos (AEM_Index).Polling_Timeout);

            pragma Debug (C1, O1 ("Task is now blocked on monitor"
                              & Natural'Image (AEM_Index)
                              & " " & Ada.Tags.External_Tag
                              (O.AEM_Infos (AEM_Index).Monitor.all'Tag)));

            pragma Debug (C2, O2 (Status (O.all)));
         end;
      end if;
   end Schedule_Task;

   ------------
   -- Create --
   ------------

   function Create
     (OCF : ORB_Controller_No_Tasking_Factory) return ORB_Controller_Access
   is
      pragma Unreferenced (OCF);
      OC : ORB_Controller_No_Tasking_Access;
      RS : PRS.Request_Scheduler_Access;
   begin
      PRS.Create (RS);
      OC := new ORB_Controller_No_Tasking (RS);
      Initialize (ORB_Controller (OC.all));
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
       Provides  => +"orb_controller!",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.ORB_Controller.No_Tasking;
