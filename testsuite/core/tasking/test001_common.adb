with PolyORB.Utils.Report;

with PolyORB.Profiles.Full_Tasking;
pragma Elaborate_All (PolyORB.Profiles.Full_Tasking);
pragma Warnings (Off, PolyORB.Profiles.Full_Tasking);

with PolyORB.Tasking.Threads;

with System;

package body Test001_Common is

   use PolyORB.Tasking.Threads;
   use PolyORB.Utils.Report;

   My_Thread_Factory  : Thread_Factory_Access;

   type Generic_Runnable is new Runnable with record
      P  : Parameterless_Procedure;
   end record;
   procedure Run (R : access Generic_Runnable);

   R, R2  : aliased Generic_Runnable;

   type Do_Nothing_Controller is new Runnable_Controller with null record;
   --  Simple controller that does nothing...

   C  : constant Runnable_Controller_Access := new Do_Nothing_Controller;

   procedure Test_Task;
   --  Body of the task.

   procedure Test_Task2;
   --  Body of the task.

   ---------------------
   -- Initialize_Test --
   ---------------------

   procedure Initialize_Test is
   begin
      My_Thread_Factory := Get_Thread_Factory;
      R.P := Test_Task'Access;
      R2.P := Test_Task2'Access;
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
      delay 1.0;
   end Test_Task;

   ----------------
   -- Test_Task2 --
   ----------------

   procedure Test_Task2 is
   begin
      delay 10.0;
   end Test_Task2;

   ------------------------
   -- Test_Task_Creation --
   ------------------------

   procedure Test_Task_Creation
     (Nb_Of_Tasks : Natural := 1000) is
   begin
      New_Test ("Generate test with"
                & Natural'Image (Nb_Of_Tasks) & " tasks");

      for J in 1 .. Nb_Of_Tasks loop
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
      Output ("Done", True);
   end Test_Task_Creation;

   --------------------------
   -- Test_Task_Priorities --
   --------------------------

   procedure Test_Task_Priorities
   is
      P_In  : constant System.Any_Priority := 3;
      P_Out : System.Any_Priority;

      T : constant Thread_Access := Run_In_Task
        (TF => My_Thread_Factory,
         Name => "",
         Default_Priority  => 10,
         R  => R2'Access,
         C  => C);
   begin
      delay 1.0;
      --  It is required to wait some time before modifying the priority.

      Set_Priority (My_Thread_Factory, Get_Thread_Id (T), P_In);
      P_Out := Get_Priority (My_Thread_Factory, Get_Thread_Id (T));
      Output ("Test priority" & P_Out'Img, P_In = P_Out);
   end Test_Task_Priorities;

end Test001_Common;
