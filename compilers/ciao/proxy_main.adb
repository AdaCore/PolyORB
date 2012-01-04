------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           P R O X Y _ M A I N                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
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

--  Main procedure for the proxy partition.
--  Portable Object Adapter

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.ORB; use CORBA.ORB;
with PortableServer;
with PortableServer.POA;
with PortableServer.POAManager;

--  Debug
with Ada.Text_IO; use Ada.Text_IO;

procedure Proxy_Main
is
   Root_POA : constant PortableServer.POA.Ref
     := PortableServer.POA.To_Ref
         (Resolve_Initial_References (To_CORBA_String ("RootPOA")));
   POA_Manager : constant PortableServer.POAManager.Ref
     := PortableServer.POA.Get_the_POAManager (Root_POA);

begin
   Put_Line ("Starting proxy.");

   PortableServer.POAManager.Activate (POA_Manager);
   CORBA.ORB.Run;

exception
   when others =>
      Put_Line ("Proxy caught exception, exiting.");
end Proxy_Main;
