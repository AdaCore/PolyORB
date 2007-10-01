------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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
