------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T A S K I N G . I D L E _ T A S K S _ M A N A G E R S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2009, Free Software Foundation, Inc.          --
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

   -------------------------
   -- Awake_One_Idle_Task --
   -------------------------

   procedure Awake_One_Idle_Task
     (ITM : access Idle_Tasks_Manager; Kind : Task_Kind)
   is
      pragma Debug (C, O ("Awake one idle task"));
      pragma Assert (not Is_Empty (ITM.Idle_Task_Lists (Kind)));

      Task_To_Awake : constant access PTI.Task_Info :=
                        List_First (ITM.Idle_Task_Lists (Kind));
   begin
      --  Signal one idle task, and put its CV in Free_CV list

      List_Detach (Task_To_Awake, ITM.Idle_Task_Lists (Kind));
      Signal (Condition (Task_To_Awake.all));
      CV_Lists.Append (ITM.Free_CV, Condition (Task_To_Awake.all));
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
      --  Awaken tasks, looping until both Kind lists are empty

      for Kind in Task_Kind loop
         while not Is_Empty (ITM.Idle_Task_Lists (Kind)) loop
            Awake_One_Idle_Task (ITM, Kind);
         end loop;
      end loop;
   end Awake_All_Idle_Tasks;

   ----------------------
   -- Remove_Idle_Task --
   ----------------------

   procedure Remove_Idle_Task
     (ITM : access Idle_Tasks_Manager;
      TI  :        PTI.Task_Info_Access)
   is
   begin
      List_Detach (TI, ITM.Idle_Task_Lists (TI.Kind));
   end Remove_Idle_Task;

   ----------------------
   -- Insert_Idle_Task --
   ----------------------

   function Insert_Idle_Task
     (ITM  : access Idle_Tasks_Manager;
      TI  :        PTI.Task_Info_Access)
     return PTCV.Condition_Access
   is
      Result : constant PTCV.Condition_Access := Allocate_CV (ITM);

   begin
      List_Attach (TI, ITM.Idle_Task_Lists (TI.Kind));

      return Result;
   end Insert_Idle_Task;

end PolyORB.Tasking.Idle_Tasks_Managers;
