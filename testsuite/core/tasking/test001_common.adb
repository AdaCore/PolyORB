with PolyORB.Initialization;

with PolyORB.Report;

with PolyORB.Profiles.Full_Tasking;
pragma Elaborate_All (PolyORB.Profiles.Full_Tasking);
pragma Warnings (Off, PolyORB.Profiles.Full_Tasking);

with PolyORB.Tasking.Threads;

package body Test001_Common is

   use PolyORB.Tasking.Threads;

   My_Thread_Factory  : Thread_Factory_Access;

   type Generic_Runnable is new Runnable with record
      P  : Parameterless_Procedure;
   end record;
   procedure Run (R : access Generic_Runnable);

   R  : aliased Generic_Runnable;

   type Do_Nothing_Controller is new Runnable_Controller with null record;
   --  Simple controller that does nothing...

   procedure Test_Task;
   --  Body of the task.

   ---------------------
   -- Initialize_Test --
   ---------------------

   procedure Initialize_Test is
   begin
      My_Thread_Factory := Get_Thread_Factory;
      R.P := Test_Task'Access;
   end Initialize_Test;

   ---------
   -- Run --
   ---------

   procedure Run (R : access Generic_Runnable) is
   begin
      R.P.all;
   end Run;

   ---------------
   -- Test_Task --
   ---------------

   procedure Test_Task is
   begin
--      PolyORB.Report.Output ("Enter task: "
--                     & Image (Get_Current_Thread_Id (My_Thread_Factory)),
--                     True);
      delay 1.0;
--      PolyORB.Report.Output ("End task: "
--                     & Image (Get_Current_Thread_Id (My_Thread_Factory)),
--                     True);
   end Test_Task;

   ------------------------
   -- Test_Task_Creation --
   ------------------------

   procedure Test_Task_Creation (Nb_Of_Tasks : Natural := 1000)
   is
      use PolyORB.Tasking.Threads;

--      RA : Runnable_Access;
      C  : constant Runnable_Controller_Access := new Do_Nothing_Controller;
   begin
      for J in 1 .. Nb_Of_Tasks loop

--       RA := R (J)'Access;
         declare
            pragma Warnings (Off);
            T : constant Thread_Access := Run_In_Task
              (TF => My_Thread_Factory,
               R  => R'Access,
               C  => C);
            pragma Unreferenced (T);
            pragma Warnings (On);
         begin
            null;
         end;
      end loop;
   end Test_Task_Creation;

end Test001_Common;
