------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 1                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.ORB;
with PortableInterceptor.ORBInitializer.Register;
with PortableInterceptor.ORBInitializer.Initialize_All;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Report;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);
pragma Elaborate_All (PolyORB.Setup.Thread_Pool_Server);

with Test001_Globals;
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

   PolyORB.CORBA_P.Server_Tools.Initiate_Servant
     (new Test001_Interface.Impl.Object, Test_Object);

   PolyORB.Utils.Report.New_Test ("Request Information");

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

   PolyORB.Utils.Report.End_Report;

   CORBA.ORB.Shutdown (False);
end Test001;
