----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  Main procedure for the proxy partition.
--  $Id$

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
