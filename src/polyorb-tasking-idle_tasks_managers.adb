------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T A S K I N G . I D L E _ T A S K S _ M A N A G E R S   --
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

   function Allocate_CV
     (ITM : access Idle_Tasks_Manager)
     return PTCV.Condition_Access;
   pragma Inline (Allocate_CV);
   --  Return one CV

   -----------------
   -- Allocate_CV --
   -----------------

   function Allocate_CV
     (ITM : access Idle_Tasks_Manager)
     return Condition_Access
   is
      use type CV_Lists.List;

      Result : Condition_Access;

   begin
      if ITM.Free_CV /= CV_Lists.Empty then

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

   procedure Awake_One_Idle_Task (ITM : access Idle_Tasks_Manager) is
      use type Task_Lists.List;

      Task_To_Awake : Task_Info_Access;

   begin
      if ITM.Idle_Task_List /= Task_Lists.Empty then
         pragma Debug (C, O ("Awake one idle task"));

         --  Signal one idle task, and puts its CV in Free_CV list

         Task_Lists.Extract_First (ITM.Idle_Task_List, Task_To_Awake);
         List_Attach (Task_To_Awake.all, Task_Lists.Last (ITM.Idle_Task_List));
         Signal (Condition (Task_To_Awake.all));
         CV_Lists.Append (ITM.Free_CV, Condition (Task_To_Awake.all));
      end if;
   end Awake_One_Idle_Task;

   ----------------------
   -- Remove_Idle_Task --
   ----------------------

   procedure Remove_Idle_Task
     (ITM : access Idle_Tasks_Manager;
      TI  :        PTI.Task_Info_Access)
   is
   begin
      List_Detach (TI.all, ITM.Idle_Task_List);
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
      Task_Lists.Prepend (ITM.Idle_Task_List, TI);
      List_Attach (TI.all, Task_Lists.First (ITM.Idle_Task_List));

      return Result;
   end Insert_Idle_Task;

end PolyORB.Tasking.Idle_Tasks_Managers;
