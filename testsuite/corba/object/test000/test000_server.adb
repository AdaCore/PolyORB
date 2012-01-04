------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 0 _ S E R V E R                        --
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

with Ada.Text_IO; use Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;

with PortableServer;
with PortableServer.POA;
with PortableServer.POA.Helper;
with PortableServer.POAManager;

with PolyORB.CORBA_P.Server_Tools;

with Test_Interface.Impl;

--  Setup server node: use no tasking default configuration

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Test000_Server is
begin
   CORBA.ORB.Initialize ("ORB");

   begin
      declare
         Root_POA : PortableServer.POA.Local_Ref;
         Obj : constant CORBA.Impl.Object_Ptr
                 := new Test_Interface.Impl.Object;
         Ref : CORBA.Object.Ref;
         Res : Boolean;
         pragma Unreferenced (Res);
      begin
         begin

            --  This should raise Inv_Objref

            Res := CORBA.Object.Non_Existent (Ref);
            raise Program_Error;
         exception
            when CORBA.Inv_Objref =>
               null;
         end;

         --  Retrieve Root POA

         Root_POA := PortableServer.POA.Helper.To_Local_Ref
           (CORBA.ORB.Resolve_Initial_References
            (CORBA.ORB.To_CORBA_String ("RootPOA")));

         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (Root_POA));

         PolyORB.CORBA_P.Server_Tools.Initiate_Servant
           (PortableServer.Servant (Obj), Ref);

         Res := CORBA.Object.Non_Existent (Ref);

         Put_Line ("'"
            & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref))
            & "'");

         CORBA.ORB.Run;
      end;
   end;
end Test000_Server;
