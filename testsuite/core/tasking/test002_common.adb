with PolyORB.Report;

with PolyORB.Profiles.Full_Tasking;
pragma Elaborate_All (PolyORB.Profiles.Full_Tasking);
pragma Warnings (Off, PolyORB.Profiles.Full_Tasking);

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;

package body Test002_Common is

   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;

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

   Global_CV : Condition_Access;
   --  CV shared by different threads.
   --  XXX Check thread safety !

   ---------------------
   -- Initialize_Test --
   ---------------------

   procedure Initialize_Test is
   begin
      My_Thread_Factory := Get_Thread_Factory;
      Create (Global_CV);
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

      My_Mutex : Mutex_Access;
   begin
      Create (My_Mutex);
      Enter (My_Mutex);
      Wait (Global_CV, My_Mutex);
      PolyORB.Report.Output ("End task: "
                             & Image
                             (Get_Current_Thread_Id (My_Thread_Factory)),
                             True);
      Leave (My_Mutex);
   end Wait_Task;

   -------------
   -- Test_CV --
   -------------

   procedure Test_CV
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
      PolyORB.Report.Output ("Wait before signal", True);
      delay 4.0;
      Signal (Global_CV);
      delay 4.0;
      PolyORB.Report.Output ("Wait before signal", True);
      delay 4.0;
      Signal (Global_CV);
      delay 4.0;
      PolyORB.Report.Output ("End signals", True);
      Broadcast (Global_CV);
      PolyORB.Report.Output ("Broadcast", True);

   end Test_CV;

end Test002_Common;
