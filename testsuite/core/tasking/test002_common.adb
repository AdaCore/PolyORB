------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 2 _ C O M M O N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

with PolyORB.Utils.Report;

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;

package body Test002_Common is

   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Utils.Report;

   My_Thread_Factory  : Thread_Factory_Access;

   Number_Of_Tasks : constant Integer := 4;
   --  Number of tasks to be created

   subtype Task_Index is Integer range 1 .. Number_Of_Tasks;

   type Generic_Runnable is new Runnable with record
      Id : Natural;
      P  : Parameterless_Procedure;
   end record;

   procedure Run (R : access Generic_Runnable);

   type Generic_Runnable_Arr is array (Task_Index) of Runnable_Access;
   R  : Generic_Runnable_Arr;

   type Do_Nothing_Controller is new Runnable_Controller with null record;
   --  Simple controller that does nothing

   Global_CV : Condition_Access;
   --  CV shared by different threads.
   --  XXX Check thread safety !

   Task_Waiting : Natural := 0;
   --  Number of tasks waiting

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

      Task_Waiting := Task_Waiting + 1;

      Wait (Global_CV, My_Mutex);

      Output ("End task: "
              & Image
              (Get_Current_Thread_Id (My_Thread_Factory)),
              True);

      Task_Waiting := Task_Waiting - 1;
      Leave (My_Mutex);
   end Wait_Task;

   -------------
   -- Test_CV --
   -------------

   procedure Test_CV is
   begin
      New_Test ("Condition Variables");

      for J in Task_Index'Range loop
         R (J) := new Generic_Runnable;
         Generic_Runnable (R (J).all).P := Wait_Task'Access;

         declare
            pragma Warnings (Off);
            T : constant Thread_Access := Run_In_Task
              (TF => My_Thread_Factory,
               R  => R (J),
               C  => new Do_Nothing_Controller);
            pragma Unreferenced (T);
            pragma Warnings (On);
         begin
            null;
         end;
      end loop;

      Output ("Wait before testing", True);
      delay 4.0;
      Output ("All task waiting", Task_Waiting = Task_Index'Last);

      Signal (Global_CV);
      delay 4.0;
      Output ("One task awaken", Task_Waiting = Task_Index'Last - 1);

      Signal (Global_CV);
      delay 4.0;
      Output ("Another task awaken", Task_Waiting = Task_Index'Last - 2);

      Broadcast (Global_CV);
      delay 4.0;
      Output ("Broadcast: all tasks are awaken", Task_Waiting = 0);

      End_Report;
   end Test_CV;

end Test002_Common;
