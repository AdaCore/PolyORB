------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . S E T U P . R A V E N S C A R _ T P _ S E R V E R     --
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
