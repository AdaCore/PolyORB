with PolyORB.Initialization;

with Report;

with PolyORB.Profiles.Full_Tasking;
pragma Elaborate_All (PolyORB.Profiles.Full_Tasking);
pragma Warnings (Off, PolyORB.Profiles.Full_Tasking);

with PolyORB.Tasking.Threads;

package body Test001_Common is

   use PolyORB.Tasking.Threads;

   My_Thread_Factory  : Thread_Factory_Access;

   Number_Of_Tasks : constant Integer := 1000;
   --  Number of tasks to be created.

   subtype Task_Index is Integer range 1 .. Number_Of_Tasks;

   type Generic_Runnable is new Runnable with record
      P  : Parameterless_Procedure;
   end record;

   procedure Run (R : access Generic_Runnable);

   type Generic_Runnable_Arr is array (Task_Index) of aliased Generic_Runnable;
   R  : Generic_Runnable_Arr;

   type Do_Nothing_Controller is new Runnable_Controller with null record;
   --  Simple controller that does nothing...

   ---------------------
   -- Initialize_Test --
   ---------------------

   procedure Initialize_Test is
   begin
      My_Thread_Factory := Get_Thread_Factory;
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

   procedure Test_Task;

   procedure Test_Task is
   begin
--      Report.Output ("Enter task: "
--                     & Image (Get_Current_Thread_Id (My_Thread_Factory)),
--                     True);
      delay 1.0;
--      Report.Output ("End task: "
--                     & Image (Get_Current_Thread_Id (My_Thread_Factory)),
--                     True);
   end Test_Task;

   ------------------------
   -- Test_Task_Creation --
   ------------------------

   procedure Test_Task_Creation
   is
      use PolyORB.Tasking.Threads;

      RA : Runnable_Access;
      C  : constant Runnable_Controller_Access := new Do_Nothing_Controller;
   begin
      for J in Task_Index'Range loop
         R (J).P := Test_Task'Access;
         RA := R (J)'Access;
         declare
            pragma Warnings (Off);
            T : constant Thread_Access := Run_In_Task
              (TF => My_Thread_Factory,
               R  => RA,
               C  => C);
            pragma Unreferenced (T);
            pragma Warnings (On);
         begin
            null;
         end;
      end loop;
   end Test_Task_Creation;

end Test001_Common;
