------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . T E S T . T H R E A D _ P O O L _ P O A          --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Set up a test server with the Thread_Pool tasking policy.

with PolyORB.Initialization;
with PolyORB.Setup.Test_SOA;
with PolyORB.Setup.Test_POA;

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);

with PolyORB.Setup.Tasking.Full_Tasking;
pragma Warnings (Off, PolyORB.Setup.Tasking.Full_Tasking);

procedure PolyORB.Test.Thread_Pool_POA is
begin
   PolyORB.Initialization.Initialize_World;
   PolyORB.Setup.Test_POA.Initialize_Test_Object;
   PolyORB.Setup.Test_SOA.Run_Test;
end PolyORB.Test.Thread_Pool_POA;
