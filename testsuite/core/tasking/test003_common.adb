with PolyORB.Initialization;

with Report;

with PolyORB.Profiles.Full_Tasking;
pragma Elaborate_All (PolyORB.Profiles.Full_Tasking);
pragma Warnings (Off, PolyORB.Profiles.Full_Tasking);

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Advanced_Mutexes;

package body Test003_Common is

   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Advanced_Mutexes;

   My_Thread_Factory  : Thread_Factory_Access;

   Number_Of_Tasks : constant Integer := 4;
   --  Number of tasks to be created.

   subtype Task_Index is Integer range 1 .. Number_Of_Tasks;

   type Generic_Runnable is new Runnable with record
      Id : Natural;
      P  : Parameterless_Procedure;
   end record;

   procedure Run (R : access Generic_Runnable);

   type Generic_Runnable_Arr is array (Task_Index) of aliased Generic_Runnable;
   R  : Generic_Runnable_Arr;

   type Do_Nothing_Controller is new Runnable_Controller with null record;
   --  Simple controller that does nothing...

   Global_AM : Adv_Mutex_Access;

   ---------------------
   -- Initialize_Test --
   ---------------------

   procedure Initialize_Test is
   begin
      My_Thread_Factory := Get_Thread_Factory;
      Create (Global_AM);
   end Initialize_Test;

   ---------
   -- Run --
   ---------

   procedure Run (R : access Generic_Runnable) is
   begin
      R.P.all;
   end Run;

   ---------------
   -- Wait_Task --
   ---------------

   procedure Wait_Task;

   procedure Wait_Task is
   begin
      Report.Output ("Enter task: "
                     & Image (Get_Current_Thread_Id (My_Thread_Factory)),
                     True);
      Enter (Global_AM);
      Report.Output ("Task "
                     & Image (Get_Current_Thread_Id (My_Thread_Factory))
                     & " entered.",
                     True);
      delay 10.0;
      Leave (Global_AM);
      Report.Output ("End task: "
                     & Image (Get_Current_Thread_Id (My_Thread_Factory)),
                     True);
   end Wait_Task;

   -------------
   -- Test_AM --
   -------------

   procedure Test_AM
   is
      use PolyORB.Tasking.Threads;

      RA : Runnable_Access;
      C  : constant Runnable_Controller_Access := new Do_Nothing_Controller;
   begin
      for J in Task_Index'Range loop
         R (J).P := Wait_Task'Access;
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
      Wait_Task;
   end Test_AM;

end Test003_Common;
