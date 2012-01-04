------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with CORBA.Object;
with CORBA.ORB;

with PolyORB.CORBA_P.Server_Tools;

with PortableServer.POA.Helper;
with PortableServer.POAManager;

with Test.DomainManager.Impl;
with Test.DomainManager.Skel;
pragma Warnings (Off, Test.DomainManager.Skel);

with Test.Echo.Impl;
with Test.Echo.Skel;
pragma Warnings (Off, Test.Echo.Skel);

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Server is
   Ref : CORBA.Object.Ref;

begin
   CORBA.ORB.Initialize ("ORB");

   PortableServer.POAManager.Activate
     (PortableServer.POA.Get_The_POAManager
      (PortableServer.POA.Helper.To_Local_Ref
       (CORBA.ORB.Resolve_Initial_References
        (CORBA.ORB.To_CORBA_String ("RootPOA")))));

   declare
      Obj : constant Test.DomainManager.Impl.Object_Ptr
        := new Test.DomainManager.Impl.Object;
      Ref : CORBA.Object.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Ref);
      CORBA.ORB.Register_Initial_Reference
        (CORBA.ORB.To_CORBA_String ("PolyORBPolicyDomainManager"), Ref);
   end;

   declare
      Obj : constant Test.Echo.Impl.Object_Ptr := new Test.Echo.Impl.Object;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Ref);
      Ada.Text_IO.Put_Line
        (''' & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref))
         & ''');
   end;

   CORBA.ORB.Run;
end Server;
