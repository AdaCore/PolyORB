------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . T A S K I N G . N O _ T A S K I N G      --
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

with PolyORB.Tasking.Profiles.No_Tasking.Threads.Annotations;
pragma Elaborate_All (PolyORB.Tasking.Profiles.No_Tasking.Threads.Annotations);
pragma Warnings (Off, PolyORB.Tasking.Profiles.No_Tasking.Threads.Annotations);

with PolyORB.Tasking.Profiles.No_Tasking.Threads;
pragma Elaborate_All (PolyORB.Tasking.Profiles.No_Tasking.Threads);
pragma Warnings (Off, PolyORB.Tasking.Profiles.No_Tasking.Threads);

with PolyORB.Tasking.Profiles.No_Tasking.Mutexes;
pragma Elaborate_All (PolyORB.Tasking.Profiles.No_Tasking.Mutexes);
pragma Warnings (Off, PolyORB.Tasking.Profiles.No_Tasking.Mutexes);

with PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables;
pragma Elaborate_All
  (PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables);
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.No_Tasking.Condition_Variables);

package body PolyORB.Setup.Tasking.No_Tasking is

end PolyORB.Setup.Tasking.No_Tasking;
