------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              S E R V E R _ T H R E A D _ P O O L _ H A H S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with Server_Common;

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

with PolyORB.Setup.Server;
pragma Warnings (Off, PolyORB.Setup.Server);

with PolyORB.Setup.Tasking.Full_Tasking;
pragma Warnings (Off, PolyORB.Setup.Tasking.Full_Tasking);

with PolyORB.ORB_Controller.Half_Sync_Half_Async;
pragma Warnings (Off, PolyORB.ORB_Controller.Half_Sync_Half_Async);

procedure Server_Thread_Pool_HAHS is
begin
   Server_Common.Launch_Server;
end Server_Thread_Pool_HAHS;
