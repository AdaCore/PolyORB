------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . S E T U P . R A V E N S C A R _ T P _ S E R V E R     --
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

--  Elaborate a complete server with the ``thread pool'' tasking
--  policy and the Ravenscar tasking profile.

with System;

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);
pragma Elaborate_All (PolyORB.ORB.Thread_Pool);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);
pragma Elaborate_All (PolyORB.ORB_Controller.Workers);

with PolyORB.Setup.Tasking.Ravenscar;
pragma Elaborate_All (PolyORB.Setup.Tasking.Ravenscar);

with PolyORB.Setup.Server;
pragma Elaborate_All (PolyORB.Setup.Server);
pragma Warnings (Off, PolyORB.Setup.Server);

package body PolyORB.Setup.Ravenscar_TP_Server is

   package Ravenscar_Profile_Instance is
      new PolyORB.Setup.Tasking.Ravenscar
     (Number_Of_Application_Tasks => 4,
      Number_Of_System_Tasks      => 20,
      Number_Of_Conditions        => 1_000,
      Number_Of_Mutexes           => 1_000,
      Task_Priority               => System.Default_Priority,
      Storage_Size                => 262_144);

   pragma Unreferenced (Ravenscar_Profile_Instance);
   --  There is no direct reference on this package: it only
   --  initializes hooks

end PolyORB.Setup.Ravenscar_TP_Server;
