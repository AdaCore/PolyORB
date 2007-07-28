------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E T U P . T A S K I N G . R A V E N S C A R       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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
   --  Priority affected to the tasks of the pool.

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
