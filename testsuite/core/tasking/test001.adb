------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 1                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Thread testsuite

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Tasking.Profiles.Full_Tasking.Threads;
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities);

with PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Mutexes);

with PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables);

with PolyORB.Initialization;
with PolyORB.Utils.Report;

with Test001_Common;

procedure Test001 is
   use Ada.Command_Line;
   use Ada.Exceptions;
   use Ada.Text_IO;
   use PolyORB.Utils.Report;

   use Test001_Common;

   Nb_Of_Tasks : Natural := 1000;

begin
   if Ada.Command_Line.Argument_Count = 1 then
      begin
         Nb_Of_Tasks := Natural'Value (Ada.Command_Line.Argument (1));
      exception
         when others =>
            null;
      end;
   end if;

   PolyORB.Initialization.Initialize_World;
   Initialize_Test;
   Test_Task_Creation (Nb_Of_Tasks);
   Test_Task_Priorities;
   End_Report;

exception
   when E : others =>
      Output ("FATAL Error, exception raised", False);

      New_Line;
      Put_Line ("Got "
                & Exception_Name (E)
                & " " & Exception_Message (E));

      End_Report;
end Test001;
