------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T A S K I N G . I D L E _ T A S K S _ M A N A G E R S   --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Log;

package body PolyORB.Tasking.Idle_Tasks_Managers is

   use PolyORB.Log;
   use PolyORB.Task_Info;
   use PolyORB.Tasking.Condition_Variables;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.tasking.idle_tasks_manager");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure Awake_One_Idle_Task
     (ITM : access Idle_Tasks_Manager; Kind : Task_Kind);
   --  Awake one idle task of the specified Kind; there must be at least one

   function Allocate_CV
     (ITM : access Idle_Tasks_Manager) return PTCV.Condition_Access;
   pragma Inline (Allocate_CV);
   --  Return one condition variable

   -----------------
   -- Allocate_CV --
   -----------------

   function Allocate_CV
     (ITM : access Idle_Tasks_Manager) return Condition_Access
   is
      use type CV_Lists.List;
      Result : Condition_Access;

   begin
      if not CV_Lists.Is_Empty (ITM.Free_CV) then
         --  Use an existing CV, from Free_CV list

         CV_Lists.Extract_First (ITM.Free_CV, Result);

      else
         --  else allocate a new one

         Create (Result);
      end if;

      return Result;
   end Allocate_CV;

   ---------------------
   -- Awake_Idle_Task --
   ---------------------

   procedure Awake_Idle_Task
     (ITM : access Idle_Tasks_Manager;
      TI  : Task_Info_Access)
   is
   begin
      pragma Debug (C, O ("Awake_Idle_Task "
                          & TI.Kind'Img & " " & Image (TI.all)
                          & ": enter"));
      List_Detach (TI, ITM.Idle_Task_Lists (TI.Kind));
      Signal (Condition (TI.all));
      pragma Debug (C, O ("Awake_Idle_Task: leave"));
   end Awake_Idle_Task;

   -------------------------
   -- Awake_One_Idle_Task --
   -------------------------

   procedure Awake_One_Idle_Task
     (ITM  : access Idle_Tasks_Manager;
      Kind : Task_Kind)
   is
   begin
      pragma Assert (not Is_Empty (ITM.Idle_Task_Lists (Kind)));
      Awake_Idle_Task
        (ITM, List_First (ITM.Idle_Task_Lists (Kind)).all'Access);
   end Awake_One_Idle_Task;

   -------------------------
   -- Awake_One_Idle_Task --
   -------------------------

   function Awake_One_Idle_Task
     (ITM             : access Idle_Tasks_Manager;
      Allow_Transient : Boolean) return Boolean
   is
   begin
      --  The choice between Kinds is arbitrary, unless Allow_Transient is
      --  False. It's simplest to pick the first Permanent one, unless there is
      --  none, in which case we try Transient.

      if not Is_Empty (ITM.Idle_Task_Lists (Permanent)) then
         Awake_One_Idle_Task (ITM, Permanent);
         return True;

      elsif Allow_Transient
        and then not Is_Empty (ITM.Idle_Task_Lists (Transient))
      then
         Awake_One_Idle_Task (ITM, Transient);
         return True;

      else
         --  Failed to find an appropriate idle task
         return False;
      end if;
   end Awake_One_Idle_Task;

   --------------------------
   -- Awake_All_Idle_Tasks --
   --------------------------

   procedure Awake_All_Idle_Tasks (ITM : access Idle_Tasks_Manager) is
   begin
      pragma Debug (C, O ("Awake_All_Idle_Tasks: enter"));

      --  Awaken tasks, looping until both Kind lists are empty

      for Kind in Task_Kind loop
         while not Is_Empty (ITM.Idle_Task_Lists (Kind)) loop
            Awake_One_Idle_Task (ITM, Kind);
         end loop;
      end loop;

      pragma Debug (C, O ("Awake_All_Idle_Tasks: leave"));
   end Awake_All_Idle_Tasks;

   ----------------------
   -- Remove_Idle_Task --
   ----------------------

   procedure Remove_Idle_Task
     (ITM : access Idle_Tasks_Manager;
      TI  : PTI.Task_Info_Access)
   is
   begin
      --  TI has been detached from the idle list if it is being awakened by
      --  the ITM or by another task through Awake_Idle_Task, but may still
      --  be attached if it is terminating now as a result of a spare tasks
      --  policy limit. In the former case, the call to List_Detach below is
      --  a no-op.

      List_Detach (TI, ITM.Idle_Task_Lists (TI.Kind));

      --  This procedure is called back by the ORB once an idle task
      --  has returned from Idle. The caller guarantees that it will
      --  update its task state to some value other than Idle within
      --  the same critical section, so we can now safely take over
      --  the condition variable to reuse it (it won't be used by another
      --  task trying to signal TI anymore).

      --  Should limit the growth of the CV_List to some reasonable size???

      CV_Lists.Append (ITM.Free_CV, Condition (TI.all));
   end Remove_Idle_Task;

   ----------------------
   -- Insert_Idle_Task --
   ----------------------

   function Insert_Idle_Task
     (ITM : access Idle_Tasks_Manager;
      TI  :        PTI.Task_Info_Access) return PTCV.Condition_Access
   is
      Result : constant PTCV.Condition_Access := Allocate_CV (ITM);
   begin
      List_Attach (TI, ITM.Idle_Task_Lists (TI.Kind));
      return Result;
   end Insert_Idle_Task;

end PolyORB.Tasking.Idle_Tasks_Managers;
