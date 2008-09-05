------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.ORB_CONTROLLER.LEADER_FOLLOWERS                  --
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

with PolyORB.Annotations;
with PolyORB.Asynch_Ev;
with PolyORB.Initialization;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Utils.Strings;

package body PolyORB.ORB_Controller.Leader_Followers is

   use PolyORB.Annotations;
   use PolyORB.Asynch_Ev;
   use PolyORB.Jobs;
   use PolyORB.Task_Info;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Threads.Annotations;

   type LF_Task_Note is new PolyORB.Annotations.Note with record
      TI  : Thread_Id;
      Job : Job_Access;
   end record;

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task
     (O  : access ORB_Controller_Leader_Followers;
      TI :        PTI.Task_Info_Access)
   is
   begin
      pragma Debug (C1, O1 ("Register_Task (LF): enter"));

      Register_Task (ORB_Controller (O.all)'Access, TI);
      Set_Note (Get_Current_Thread_Notepad.all,
                LF_Task_Note'(Note with TI => Id (TI.all), Job => null));

      pragma Debug (C1, O1 ("Register_Task (LF): leave"));
   end Register_Task;

   ---------------------
   -- Disable_Polling --
   ---------------------

   procedure Disable_Polling
     (O : access ORB_Controller_Leader_Followers;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      AEM_Index : constant Natural := Index (O.all, M);

   begin
      --  Force all tasks currently waiting on this monitor to abort

      if O.AEM_Infos (AEM_Index).TI /= null then
         pragma Debug (C1, O1 ("Disable_Polling: Aborting polling task"));
         PTI.Request_Abort_Polling (O.AEM_Infos (AEM_Index).TI.all);
         PolyORB.Asynch_Ev.Abort_Check_Sources
           (Selector (O.AEM_Infos (AEM_Index).TI.all).all);

         pragma Debug (C1, O1 ("Disable_Polling: waiting abort is complete"));
         O.AEM_Infos (AEM_Index).Polling_Abort_Counter
           := O.AEM_Infos (AEM_Index).Polling_Abort_Counter + 1;

         Wait (O.AEM_Infos (AEM_Index).Polling_Completed, O.ORB_Lock);

         O.AEM_Infos (AEM_Index).Polling_Abort_Counter
           := O.AEM_Infos (AEM_Index).Polling_Abort_Counter - 1;

         pragma Debug (C1, O1 ("Disable_Polling: aborting done"));
      end if;
   end Disable_Polling;

   --------------------
   -- Enable_Polling --
   --------------------

   procedure Enable_Polling
     (O : access ORB_Controller_Leader_Followers;
      M : PAE.Asynch_Ev_Monitor_Access)
   is
      AEM_Index : constant Natural := Index (O.all, M);

   begin
      pragma Debug (C1, O1 ("Enable_Polling"));

      if O.AEM_Infos (AEM_Index).Polling_Abort_Counter = 0 then

         --  Allocate one task to poll on AES

         Try_Allocate_One_Task (O, Allow_Transient => True);
      end if;
   end Enable_Polling;

   ------------------
   -- Notify_Event --
   ------------------

   procedure Notify_Event
     (O : access ORB_Controller_Leader_Followers;
      E :        Event)
   is
      use type PRS.Request_Scheduler_Access;
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

               if O.AEM_Infos (AEM_Index).Polling_Abort_Counter > 0 then

                  --  This task has been aborted by one or more tasks,
                  --  we broadcast them.

                  Broadcast (O.AEM_Infos (AEM_Index).Polling_Completed);
               end if;
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

               if O.AEM_Infos (AEM_Index).TI /= null
                 and then not O.AEM_Infos (AEM_Index).Polling_Scheduled
               then
                  --  No task is currently polling, allocate one

                  O.AEM_Infos (AEM_Index).Polling_Scheduled := True;
                  Try_Allocate_One_Task (O, Allow_Transient => True);
               end if;
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

            --  Awake all idle tasks

            Awake_All_Idle_Tasks (O.Idle_Tasks);

            --  Unblock blocked tasks

            for J in O.AEM_Infos'Range loop
               if O.AEM_Infos (J).TI /= null then
                  PTI.Request_Abort_Polling (O.AEM_Infos (J).TI.all);
                  PolyORB.Asynch_Ev.Abort_Check_Sources
                    (Selector (O.AEM_Infos (J).TI.all).all);
               end if;
            end loop;

         when Queue_Event_Job =>

            declare
               Note : LF_Task_Note;
            begin
               Get_Note (Get_Current_Thread_Notepad.all, Note);

               if Note.TI = E.By_Task
                 and then Note.Job = null
               then

                  --  Queue event directly into task attribute

                  Set_Note
                    (Get_Current_Thread_Notepad.all,
                     LF_Task_Note'(Annotations.Note
                                   with TI => E.By_Task, Job => E.Event_Job));
               else

                  --  Queue event to main job queue

                  PJ.Queue_Job (O.Job_Queue, E.Event_Job);
                  Try_Allocate_One_Task (O, Allow_Transient => True);
               end if;
            end;

         when Queue_Request_Job =>

            declare
               Job_Queued : Boolean := False;
            begin
               if O.RS /= null then
                  Leave_ORB_Critical_Section (O);
                  Job_Queued := PRS.Try_Queue_Request_Job
                    (O.RS, E.Request_Job, E.Target);
                  Enter_ORB_Critical_Section (O);
               end if;

               if not Job_Queued then
                  --  Default : Queue event to main job queue

                  declare
                     Note : LF_Task_Note;

                  begin
                     Get_Note
                       (Get_Current_Thread_Notepad.all,
                        Note);

                     if Note.TI = Current_Task
                       and then Note.Job = null
                     then
                        --  Queue event directly into task attribute

                        pragma Debug (C1, O1 ("Queue request in task area"));

                        Set_Note
                          (Get_Current_Thread_Notepad.all,
                           LF_Task_Note'(Annotations.Note
                                         with TI => Note.TI,
                                         Job => E.Request_Job));
                     else
                        PJ.Queue_Job (O.Job_Queue, E.Request_Job);
                        Try_Allocate_One_Task (O, Allow_Transient => True);
                     end if;
                  end;
               end if;
            end;

         when Request_Result_Ready =>

            --  A Request has been completed and a response is
            --  available. We must forward it to requesting task. We
            --  ensure this task will stop its current action and ask
            --  for rescheduling.

            case State (E.Requesting_Task.all) is
               when Running =>

                  --  We cannot abort a running task. We let it
                  --  complete its job and ask for rescheduling.

                  null;

               when Blocked =>

                  --  We abort this task. It will then leave Blocked
                  --  state and ask for rescheduling.

                  declare
                     Sel : Asynch_Ev_Monitor_Access
                       renames Selector (E.Requesting_Task.all);

                  begin
                     pragma Debug (C1, O1 ("About to abort block"));

                     pragma Assert (Sel /= null);
                     Abort_Check_Sources (Sel.all);

                     pragma Debug (C1, O1 ("Aborted."));
                  end;

               when Idle =>

                  --  We awake this task. It will then leave Idle
                  --  state and ask for rescheduling.

                  pragma Debug (C1, O1 ("Signal requesting task"));

                  Signal (Condition (E.Requesting_Task.all));

               when Terminated
                 | Unscheduled =>

                  --  Nothing to do

                  null;
            end case;

         when Idle_Awake =>
            --  A task has left Idle state
            Remove_Idle_Task (O.Idle_Tasks, E.Awakened_Task);

         when Task_Registered =>
            null;

         when Task_Unregistered =>
            Note_Task_Unregistered (O);

      end case;

      pragma Debug (C2, O2 (Status (O.all)));
   end Notify_Event;

   -------------------
   -- Schedule_Task --
   -------------------

   procedure Schedule_Task
     (O  : access ORB_Controller_Leader_Followers;
      TI : PTI.Task_Info_Access)
   is
      Note : LF_Task_Note;

   begin
      pragma Debug (C1, O1 ("Schedule_Task "
        & PTI.Image (TI.all) & ": enter"));

      if State (TI.all) = Terminated then
         pragma Debug (C1, O1 ("Schedule_Task: task is terminated"));
         return;
      end if;

      Set_State_Unscheduled (O.Summary, TI.all);

      Get_Note (Get_Current_Thread_Notepad.all, Note);

      --  Recompute TI status

      if Exit_Condition (TI.all)
        or else (O.Shutdown
                 and then not Has_Pending_Job (O)
                 and then TI.Kind = Permanent)
      then
         Set_State_Terminated (O.Summary, TI.all);

         pragma Debug (C1, O1 ("Task is now terminated"));
         pragma Debug (C2, O2 (Status (O.all)));

      elsif Note.Job /= null then

         --  Process locally queued job

         declare
            Job : constant Job_Access := Note.Job;
         begin
            Set_Note
              (Get_Current_Thread_Notepad.all,
               LF_Task_Note'(Annotations.Note
                             with TI => Note.TI, Job => null));

            Set_State_Running (O.Summary, TI.all, Job);
         end;

      elsif not PJ.Is_Empty (O.Job_Queue) then

         --  Process job

         Set_State_Running (O.Summary, TI.all, PJ.Fetch_Job (O.Job_Queue));

      elsif May_Poll (TI.all) then
         declare
            AEM_Index : constant Natural := Need_Polling_Task (O);
         begin
            if AEM_Index > 0 then
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
            end if;
         end;
      end if;

      if PTI.State (TI.all) = Unscheduled then
         Set_State_Idle
           (O.Summary,
            TI.all,
            Insert_Idle_Task (O.Idle_Tasks, TI),
            O.ORB_Lock);

         pragma Debug (C1, O1 ("Task is now idle"));
         pragma Debug (C2, O2 (Status (O.all)));
      end if;
   end Schedule_Task;

   ------------
   -- Create --
   ------------

   function Create
     (OCF : access ORB_Controller_Leader_Followers_Factory;
      Borrow_Transient_Tasks : Boolean)
     return ORB_Controller_Access
   is
      pragma Unreferenced (OCF);

      OC : ORB_Controller_Leader_Followers_Access;
      RS : PRS.Request_Scheduler_Access;

   begin
      PRS.Create (RS);
      OC := new ORB_Controller_Leader_Followers (RS, Borrow_Transient_Tasks);

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
      (Name      => +"orb_controller.leader_followers",
       Conflicts => +"orb.no_tasking",
       Depends   => +"tasking.condition_variables"
         & "tasking.mutexes"
         & "request_scheduler?",
       Provides  => +"orb_controller!",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.ORB_Controller.Leader_Followers;
