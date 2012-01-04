------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E T U P . T A S K I N G . R A V E N S C A R       --
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

--  You should instantiate this package to set up a ravenscar profile.

with System;

with PolyORB.Tasking.Profiles.Ravenscar.Threads.Annotations;

with PolyORB.Tasking.Profiles.Ravenscar.Threads;
with PolyORB.Tasking.Profiles.Ravenscar.Mutexes;
with PolyORB.Tasking.Profiles.Ravenscar.Condition_Variables;

generic
   Number_Of_Application_Tasks    : Integer;
   --  Number of tasks created by the user.

   Number_Of_System_Tasks         : Integer;
   --  Number of tasks created by the PolyORB run-time library.

   Number_Of_Conditions           : Integer;
   --  Number of preallocated conditions.

   Number_Of_Mutexes              : Integer;
   --  Number of preallocated mutexes.

   Task_Priority                  : System.Priority;
   --  Priority of the tasks of the pool.

   Storage_Size                   : Natural;
   --  Stack size of the system tasks.

package PolyORB.Setup.Tasking.Ravenscar is

   package Threads_Package is
      new PolyORB.Tasking.Profiles.Ravenscar.Threads
     (Number_Of_Application_Tasks,
      Number_Of_System_Tasks,
      Task_Priority,
      Storage_Size);

   package Thread_Annotations_Package is new Threads_Package.Annotations;

   package Conditions_Package is
      new PolyORB.Tasking.Profiles.Ravenscar.Condition_Variables
     (Threads_Package,
      Number_Of_Conditions);

   package Mutexes_Package is
      new PolyORB.Tasking.Profiles.Ravenscar.Mutexes
     (Threads_Package,
      Number_Of_Mutexes);

end PolyORB.Setup.Tasking.Ravenscar;
