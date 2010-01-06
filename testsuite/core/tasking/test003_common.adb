------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 3 _ C O M M O N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Report;

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Advanced_Mutexes;

package body Test003_Common is

   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Advanced_Mutexes;

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

   Global_AM : Adv_Mutex_Access;

   Round : Natural := Task_Index'Last;

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
      Output
        ("Enter task: "
         & Image (Get_Current_Thread_Id (My_Thread_Factory)),
         True);
      Enter (Global_AM);
      Output
        ("Task "
         & Image (Get_Current_Thread_Id (My_Thread_Factory))
         & " entered AM.",
         True);
      delay 1.0;

      Enter (Global_AM);
      Output
        ("Task "
         & Image (Get_Current_Thread_Id (My_Thread_Factory))
         & " entered AM (2).",
         True);
      Round := Round - 1;
      Leave (Global_AM);

      Leave (Global_AM);
      Output
        ("End task: "
         & Image (Get_Current_Thread_Id (My_Thread_Factory)),
         True);
   end Wait_Task;

   -------------
   -- Test_AM --
   -------------

   procedure Test_AM is
   begin
      New_Test ("Tasks entering/leaving Advanced Mutex");

      for J in Task_Index'Range loop
         R (J) := new Generic_Runnable;
         Generic_Runnable (R (J).all).P := Wait_Task'Access;

         declare
            T : constant Thread_Access :=
                  Run_In_Task (TF => My_Thread_Factory, R  => R (J));
            pragma Unreferenced (T);
         begin
            null;
         end;
      end loop;

      delay 1.5 * Task_Index'Last;
      Output ("All tasks entered and left AM", Round = 0);
      End_Report;
   end Test_AM;

end Test003_Common;
