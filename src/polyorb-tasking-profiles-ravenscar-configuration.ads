------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . P R O F I L E S              --
--             . R A V E N S C A R . C O N F I G U R A T I O N              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Declare some constants used for the configuration of the tasking API
--  under the Ravenscar profile.

with PolyORB.Configuration;

package PolyORB.Tasking.Profiles.Ravenscar.Configuration is

   Number_Of_Threads         : constant Integer;
   --  Number of preallocated tasks.

   Number_Of_Conditions      : constant Integer;
   --  Number of preallocated conditions.

   Number_Of_Mutexes         : constant Integer;
   --  Number of preallocated mutexes.

private

   Default_Threads          : constant Integer := 20;
   --  Default value for Number_Of_Threads.

   Default_Conditions       : constant Integer := 40;
   --  Default value for Number_Of_Conditions.

   Default_Mutexes          : constant Integer := 40;
   --  Default value for Number_Of_Mutexes.

   Number_Of_Threads : constant Integer := PolyORB.Configuration.Get_Conf
     ("tasking", "polyorb.ravenscar.number_of_threads",
      Default_Threads);

   Number_Of_Mutexes   : constant Integer := PolyORB.Configuration.Get_Conf
     ("tasking", "polyorb.ravenscar.number_of_mutexes",
      Default_Mutexes);

   Number_Of_Conditions : constant Integer := PolyORB.Configuration.Get_Conf
     ("tasking", "polyorb.ravenscar.number_of_conditions",
      Default_Conditions);

end PolyORB.Tasking.Profiles.Ravenscar.Configuration;
