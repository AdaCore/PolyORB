------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 0 _ C O M M O N                        --
--                                                                          --
--                                 S p e c                                  --
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

--  Main tests procedure for the tasking package.
--  This package does NOT register the tasking profile,

with PolyORB.Parameters;
pragma Elaborate_All (PolyORB.Parameters);

package Test000_Common is

   use PolyORB.Parameters;

   Number_Of_Tasks : constant Integer :=
     Get_Conf ("test", "tasking.number_of_tasks", 2);
   --  Number of tasks used in the tests.

   Delay_Used      : constant Float :=
     Float (Get_Conf ("test", "tasking.delay_used", 1));
   --  Some delay are used in the test (between 1 and 4 per task per tests).
   --  This constant is the time they wait, in seconds.

   ---------------------
   -- Test procedures --
   ---------------------

   procedure Initialize;
   --  Initialize the package.

   procedure Test_Threads;
   --  Test the thread fonctionnalities.

   procedure Test_Synchronisations;
   --  Test the POSIX-like synchronisations objects.
   --  Based on Test_Monitors.

   procedure Test_Mutexes;
   --  Test the mutexes functionnalities.

   procedure End_Tests;
   --  Signal  the end of the tests.

end Test000_Common;
