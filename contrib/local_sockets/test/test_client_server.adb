------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   T E S T _ C L I E N T _ S E R V E R                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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



with LS_Setup;
pragma Warnings (Off, LS_Setup);
pragma Elaborate_All (LS_Setup);

with PolyORB.Tasking.Threads;
with PolyORB.Initialization;
with Test_Client_Server_Pkg; use Test_Client_Server_Pkg;
with Ada.Real_Time;  use Ada.Real_Time;

------------------------
-- Test_Client_Server --
------------------------

procedure Test_Client_Server is
begin
   PolyORB.Initialization.Initialize_World;
   PolyORB.Tasking.Threads.Create_Task (Test_Client_Server_Pkg.Server'Access);
   delay until Clock + Milliseconds (500);
   PolyORB.Tasking.Threads.Create_Task (Client'Access);
end Test_Client_Server;
