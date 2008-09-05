------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . O R B _ C O N T R O L L E R                --
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

with PolyORB.Constants;
with PolyORB.Parameters;

package body PolyORB.ORB_Controller is

   use PolyORB.Task_Info;

   My_Factory : ORB_Controller_Factory_Access;

   --------------------
   -- Terminate_Task --
   --------------------

   procedure Terminate_Task
     (O  : access ORB_Controller; TI : in out PTI.Task_Info)
   is
   begin
      Set_State_Terminated (O.Summary, TI);
   end Terminate_Task;

   ------------
   -- Create --
   ------------

   procedure Create
     (O : out ORB_Controller_Access; Borrow_Transient_Tasks : Boolean)
   is
   begin
      pragma Assert (My_Factory /= null);

      O := Create (My_Factory, Borrow_Transient_Tasks);
   end Create;

   --------------------------------
   -- Enter_ORB_Critical_Section --
   --------------------------------

   procedure Enter_ORB_Critical_Section (O : access ORB_Controller) is
   begin
      pragma Debug (C1, O1 ("Enter_ORB_Critical_Section"));
      PTM.Enter (O.ORB_Lock);
   end Enter_ORB_Critical_Section;

   ------------------
   -- Get_Monitors --
   ------------------

   function Get_Monitors (O : access ORB_Controller) return Monitor_Array is
      use type PAE.Asynch_Ev_Monitor_Access;

      Result : Monitor_Array (1 .. O.AEM_Infos'Length);
      Last   : Natural := 0;

   begin
      for J in O.AEM_Infos'Range loop
         if O.AEM_Infos (J).Monitor /= null then
            Last := Last + 1;
            Result (Last) := O.AEM_Infos (J).Monitor;
         end if;
      end loop;

      return Result (1 .. Last);
   end Get_Monitors;

   ---------------------
   -- Get_Pending_Job --
   ---------------------

   function Get_Pending_Job (O : access ORB_Controller) return PJ.Job_Access is
   begin
      if not Has_Pending_Job (O) then
         return null;
      end if;
      return PJ.Fetch_Job (O.Job_Queue);
   end Get_Pending_Job;

   ---------------------
   -- Get_Tasks_Count --
   ---------------------

   function Get_Tasks_Count
     (OC    : ORB_Controller;
      Kind  : PTI.Any_Task_Kind  := PTI.Any;
      State : PTI.Any_Task_State := PTI.Any) return Natural
   is
   begin
      return Get_Count (OC.Summary, Kind, State);
   end Get_Tasks_Count;

   ---------------------
   -- Has_Pending_Job --
   ---------------------

   function Has_Pending_Job (O : access ORB_Controller) return Boolean is
   begin
      return not PJ.Is_Empty (O.Job_Queue);
   end Has_Pending_Job;

   -----------
   -- Index --
   -----------

   function Index
     (O : ORB_Controller;
      M : PAE.Asynch_Ev_Monitor_Access) return Natural
   is
      use type PAE.Asynch_Ev_Monitor_Access;

   begin
      for J in O.AEM_Infos'Range loop
         if O.AEM_Infos (J).Monitor = M then
            return J;
         end if;
      end loop;

      return 0;
   end Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (OC : in out ORB_Controller) is
      use PolyORB.Parameters;

      Polling_Interval : constant Natural
        := Get_Conf ("orb_controller",
                     "polyorb.orb_controller.polling_interval",
                     0);

      Polling_Timeout : constant Natural
        := Get_Conf ("orb_controller",
                     "polyorb.orb_controller.polling_timeout",
                     0);

   begin
      PTM.Create (OC.ORB_Lock);

      for J in OC.AEM_Infos'Range loop
         PTCV.Create (OC.AEM_Infos (J).Polling_Completed);
      end loop;

      OC.Idle_Tasks := new Idle_Tasks_Manager;
      OC.Job_Queue := PolyORB.Jobs.Create_Queue;

      for J in OC.AEM_Infos'Range loop
         if Polling_Interval = 0 then
            OC.AEM_Infos (J).Polling_Interval := PolyORB.Constants.Forever;
         else
            OC.AEM_Infos (J).Polling_Interval := Polling_Interval * 0.01;
         end if;

         if Polling_Timeout = 0 then
            OC.AEM_Infos (J).Polling_Timeout := PolyORB.Constants.Forever;
         else
            OC.AEM_Infos (J).Polling_Timeout := Polling_Timeout * 0.01;
         end if;
      end loop;
   end Initialize;

   ---------------------------
   -- Is_Locally_Terminated --
   ---------------------------

   function Is_Locally_Terminated
     (O                      : access ORB_Controller;
      Expected_Running_Tasks : Natural := 1) return Boolean
   is
      use PolyORB.Tasking.Threads;
   begin
      pragma Debug (C2, O2 ("Is_Locally_Terminated: " & Status (O.all)));

      if Get_Count (O.Summary, Kind => Transient) > 0
        or else Get_Count (O.Summary, State => Running)
                  > Expected_Running_Tasks
        or else Get_Count (O.Summary, State => Unscheduled) > 0
        or else Has_Pending_Job (O)
      then
         return False;
      end if;

      return (Awake_Count
               - Independent_Count
               - Get_Count (O.Summary, State => Idle)
               - Get_Count (O.Summary, State => Blocked)
               = Expected_Running_Tasks);
   end Is_Locally_Terminated;

   --------------------------------
   -- Leave_ORB_Critical_Section --
   --------------------------------

   procedure Leave_ORB_Critical_Section (O : access ORB_Controller) is
   begin
      pragma Debug (C1, O1 ("Leave_ORB_Critical_Section"));
      PTM.Leave (O.ORB_Lock);
   end Leave_ORB_Critical_Section;

   -----------------------
   -- Need_Polling_Task --
   -----------------------

   function Need_Polling_Task (O : access ORB_Controller) return Natural is
      use type PAE.Asynch_Ev_Monitor_Access;

      function Needs_Polling (Index : Natural) return Boolean;
      --  True when polling is required for the AEM at the given index

      -------------------
      -- Needs_Polling --
      -------------------

      function Needs_Polling (Index : Natural) return Boolean is
      begin
         return True
           and then O.AEM_Infos (Index).Monitor /= null
           and then PAE.Has_Sources (O.AEM_Infos (Index).Monitor.all)
           and then O.AEM_Infos (Index).Polling_Abort_Counter = 0
           and then O.AEM_Infos (Index).TI = null;
      end Needs_Polling;

   begin
      --  To promote fairness among AEM, we retain the value of the last
      --  last monitored AEM, and test it iff no other AEM need polling.

      --  Check whether any AEM but the last monitored needs a polling task

      for J in O.AEM_Infos'Range loop
         if J /= O.Last_Monitored_AEM and then Needs_Polling (J) then
            O.Last_Monitored_AEM := J;
            return J;
         end if;
      end loop;

      --  Check whether the last monitored AEM needs a polling task

      if Needs_Polling (O.Last_Monitored_AEM) then
         return O.Last_Monitored_AEM;
      end if;

      --  No AEM needs polling

      return 0;
   end Need_Polling_Task;

   ----------------------------
   -- Note_Task_Unregistered --
   ----------------------------

   procedure Note_Task_Unregistered (O : access ORB_Controller'Class) is
      use PTCV;
   begin
      if Get_Count (O.Summary) = 0
        and then O.Shutdown
        and then O.Shutdown_CV /= null
      then
         Broadcast (O.Shutdown_CV);
      end if;
   end Note_Task_Unregistered;

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

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task
     (O  : access ORB_Controller;
      TI : PTI.Task_Info_Access)
   is
   begin
      pragma Debug (C1, O1 ("Register_Task: enter"));
      pragma Assert (State (TI.all) = Unscheduled);

      Task_Created (O.Summary, TI.all);
      Notify_Event (ORB_Controller'Class (O.all)'Access,
        Event'(Kind => Task_Registered, Registered_Task => TI));

      pragma Debug (C2, O2 (Status (O.all)));
      pragma Debug (C1, O1 ("Register_Task: leave"));
   end Register_Task;

   ------------
   -- Status --
   ------------

   function Status (O : ORB_Controller) return String is
      use PolyORB.Tasking.Threads;

      function Counters_For_State (S : Any_Task_State) return String;
      --  Return the task counters for state S

      function Counters_For_State (S : Any_Task_State) return String is
         State_Name : constant String := S'Img;

         function Counter_For_Kind (K : Task_Kind) return String;
         --  Return the task counter for kind K and state S

         ----------------------
         -- Counter_For_Kind --
         ----------------------

         function Counter_For_Kind (K : Task_Kind) return String is
            Kind_Name : constant String := K'Img;
            Count     : constant String :=
                          Natural'Image (Get_Count (O.Summary, K, S));
         begin
            pragma Assert (Count (1) = ' ');
            return Count (2 .. Count'Last) & Kind_Name (1);
         end Counter_For_Kind;

      begin
         return State_Name (1) & ": "
           & Counter_For_Kind (Permanent) & "/" & Counter_For_Kind (Transient);
      end Counters_For_State;

   begin
      return Counters_For_State (Any)
        & " " & Counters_For_State (Unscheduled)
        & " " & Counters_For_State (Running)
        & " " & Counters_For_State (Blocked)
        & " " & Counters_For_State (Idle)
        & " " & Counters_For_State (Terminated)
        & " | PJ:" & Natural'Image (PJ.Length (O.Job_Queue))
        & " | Tra:" & Natural'Image (Get_Count (O.Summary, Kind => Transient))
        & " Awk:" & Natural'Image (Awake_Count)
        & " Ind:" & Natural'Image (Independent_Count);
   end Status;

   -------------------
   -- Shutting_Down --
   -------------------

   function Shutting_Down (O : ORB_Controller) return Boolean is
   begin
      return O.Shutdown;
   end Shutting_Down;

   ---------------------
   -- Unregister_Task --
   ---------------------

   procedure Unregister_Task
     (O  : access ORB_Controller;
      TI : PTI.Task_Info_Access)
   is
   begin
      pragma Debug (C1, O1 ("Unregister_Task: enter"));
      pragma Assert (State (TI.all) = Terminated);
      Task_Removed (O.Summary, TI.all);
      Notify_Event
        (ORB_Controller'Class (O.all)'Access,
         Event'(Kind => Task_Unregistered));
      pragma Debug (C2, O2 (Status (O.all)));
      pragma Debug (C1, O1 ("Unregister_Task: leave"));
   end Unregister_Task;

   ---------------------------
   -- Try_Allocate_One_Task --
   ---------------------------

   procedure Try_Allocate_One_Task
     (O : access ORB_Controller; Allow_Transient : Boolean)
   is
   begin
      pragma Debug (C1, O1 ("Try_Allocate_One_Task: enter"));

      if Get_Count (O.Summary, State => Unscheduled) > 0 then

         --  Some tasks are not scheduled. We assume one of them will
         --  be allocated to handle current event.

         pragma Debug (C1, O1 ("Unassigned task will handle event"));
         null;

      elsif Get_Count (O.Summary, State => Idle) > 0 then

         Try_Awake_One_Idle_Task (O.Idle_Tasks, Allow_Transient);
         --  Note that there might not be any idle tasks at this point, because
         --  the count can be too high, because it is incremented when an idle
         --  task awakens and send the Idle_Awake event, whereas a task is
         --  removed from the list of idle tasks earlier, when we decide to
         --  awaken one.

      else
         pragma Debug (C1, O1 ("No idle tasks"));
         null;

      end if;

      pragma Debug (C1, O1 ("Try_Allocate_One_Task: end"));
   end Try_Allocate_One_Task;

   -------------------------
   -- Wait_For_Completion --
   -------------------------

   procedure Wait_For_Completion (O : access ORB_Controller) is
      use PTCV;
   begin
      pragma Assert (O.Shutdown);
      if O.Shutdown_CV = null then
         Create (O.Shutdown_CV);
      end if;

      while Get_Count (O.Summary) > 0 loop
         Wait (O.Shutdown_CV, O.ORB_Lock);
      end loop;
   end Wait_For_Completion;

end PolyORB.ORB_Controller;
