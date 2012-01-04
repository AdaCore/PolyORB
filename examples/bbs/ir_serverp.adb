------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           I R _ S E R V E R P                            --
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

with Server;
with Do_Nothing;
pragma Warnings (Off, Server);

with PolyORB.If_Descriptors;
with PolyORB.If_Descriptors.CORBA_IR;
with PolyORB.POA_Config.Proxies;
pragma Warnings (Off, PolyORB.POA_Config.Proxies);

with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Initialization;

pragma Warnings (Off);
with PolyORB.Setup.Thread_Pool_Server;
with PolyORB.POA_Config.RACWs;
pragma Warnings (On);

procedure Ir_Serverp is
begin
   Do_Nothing;
   PolyORB.Initialization.Initialize_World;
   PolyORB.If_Descriptors.Default_If_Descriptor
     := new PolyORB.If_Descriptors.CORBA_IR.IR_If_Descriptor;
   PolyORB.ORB.Run (PolyORB.Setup.The_ORB, May_Poll => True);
end Ir_Serverp;
