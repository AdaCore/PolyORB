------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 1                               --
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

with CORBA.ORB;
with PortableInterceptor.ORBInitializer.Register;
with PortableInterceptor.ORBInitializer.Initialize_All;
with PortableServer.POA.Helper;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Report;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with Test001_Globals;
with Test001_Interface.Helper;
with Test001_Interface.Impl;
with Test001_ORB_Initializer.Impl;

procedure Test001 is
   use Test001_Globals;

   Aux : CORBA.Long;
   pragma Warnings (Off, Aux);

begin
   CORBA.ORB.Initialize ("ORB");

   declare
      Ptr : constant Test001_ORB_Initializer.Impl.Object_Ptr
        := new Test001_ORB_Initializer.Impl.Object;
      Ref : PortableInterceptor.ORBInitializer.Local_Ref;
   begin
      PortableInterceptor.ORBInitializer.Set
        (Ref, PolyORB.Smart_Pointers.Entity_Ptr (Ptr));

      PortableInterceptor.ORBInitializer.Register (Ref);
   end;

   PortableInterceptor.ORBInitializer.Initialize_All;

   PolyORB.CORBA_P.Server_Tools.Initiate_Server (True);

   declare
      Root_POA : PortableServer.POA.Local_Ref;
   begin
      Root_POA := PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      declare
         Id : constant PortableServer.ObjectId
           := PortableServer.POA.Activate_Object
               (Root_POA, new Test001_Interface.Impl.Object);
      begin
         Test_ObjectId := PortableInterceptor.ObjectId (Id);
         Test_Object :=
           Test001_Interface.Helper.To_Ref
            (PortableServer.POA.Id_To_Reference (Root_POA, Id));
      end;

      declare
         Id : constant PortableServer.ObjectId
           := PortableServer.POA.Activate_Object
               (Root_POA, new Test001_Interface.Impl.Object);
      begin
         Test_Forward_Object :=
           Test001_Interface.Helper.To_Ref
            (PortableServer.POA.Id_To_Reference (Root_POA, Id));
      end;
   end;

   PolyORB.Utils.Report.New_Test ("Request Information");

   --  Scenario 1: normal flow
   --  Client.Send_Request => Server.Receive_Request_Service_Contexts =>
   --  Server.Receive_Request => Server.Send_Reply => Client.Receive_Reply

   Enable_Test_Point (Send_Request) := True;
   Enable_Test_Point (Receive_Request_Service_Contexts) := True;
   Enable_Test_Point (Receive_Request) := True;
   Enable_Test_Point (Send_Reply) := True;
   Enable_Test_Point (Receive_Reply) := True;
   Aux := Test001_Interface.Func (Test_Object, 10);
   Enable_Test_Point (Send_Request) := False;
   Enable_Test_Point (Receive_Request_Service_Contexts) := False;
   Enable_Test_Point (Receive_Request) := False;
   Enable_Test_Point (Send_Reply) := False;
   Enable_Test_Point (Receive_Reply) := False;

   --  Scenario 2: exception flow
   --  Server.Send_Exception => Client.Receive_Exception

   Enable_Test_Point (Send_Exception) := True;
   Enable_Test_Point (Receive_Exception) := True;
   Raise_Test_Exception := True;

   begin
      Aux := Test001_Interface.Func (Test_Object, 10);
   exception
      when Test001_Interface.Test_Exception =>
         null;
   end;

   Enable_Test_Point (Send_Exception) := False;
   Enable_Test_Point (Receive_Exception) := False;
   Raise_Test_Exception := False;

   --  Scenario 3: location forwarding flow
   --  Server.Send_Other => Client.Receive_Other

   Enable_Test_Point (Send_Other) := True;
   Enable_Test_Point (Receive_Other) := True;
   Forward_Location := True;
   Aux := Test001_Interface.Func (Test_Object, 10);
   Enable_Test_Point (Send_Other) := False;
   Enable_Test_Point (Receive_Other) := False;

   PolyORB.Utils.Report.End_Report;

   CORBA.ORB.Shutdown (False);
end Test001;
