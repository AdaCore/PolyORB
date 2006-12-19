------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 0 _ C O M M O N                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

   procedure Test_Watchers;
   --  Test the watcher functionnalities.

   procedure Test_Mutexes;
   --  Test the mutexes functionnalities.

   procedure End_Tests;
   --  Signal  the end of the tests.

end Test000_Common;
