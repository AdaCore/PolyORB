------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 1 _ C O M M O N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Utils.Report;

with PolyORB.Setup.Tasking.Full_Tasking;
pragma Elaborate_All (PolyORB.Setup.Tasking.Full_Tasking);
pragma Warnings (Off, PolyORB.Setup.Tasking.Full_Tasking);

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
