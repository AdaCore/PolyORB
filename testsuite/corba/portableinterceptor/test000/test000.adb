------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
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

with CORBA;
with CORBA.Object;
with CORBA.ORB;
with PortableInterceptor.ORBInitializer.Register;
with PortableInterceptor.ORBInitializer.Initialize_All;
with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);
with PolyORB.Tasking.Threads;
pragma Warnings (Off, PolyORB.Tasking.Threads);
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Report;          use PolyORB.Utils.Report;

with Test000_Globals;               use Test000_Globals;
with Test000_Idl;                   use Test000_Idl;
with Test000_Idl.ClientInterceptor; use Test000_Idl.ClientInterceptor;
with Test000_Idl.ServerInterceptor; use Test000_Idl.ServerInterceptor;
with Test000_Idl.TestInterface;
with Test000_Idl.TestInterface.Impl;
with Test000_Idl.TestInterface.Helper;
with Test000_Idl.ORBInitializer;
with Test000_Idl.ORBInitializer.Impl;

procedure Test000 is

   procedure Init_Test;

   procedure Init_Test is
      use PolyORB.CORBA_P.Server_Tools;
   begin
      CORBA.ORB.Initialize ("ORB");

      declare
         Ptr : constant Test000_Idl.ORBInitializer.Impl.Object_Ptr
           := new Test000_Idl.ORBInitializer.Impl.Object;
         Ref : Test000_Idl.ORBInitializer.Local_Ref;
      begin
         Test000_Idl.ORBInitializer.Set
          (Ref, PolyORB.Smart_Pointers.Entity_Ptr (Ptr));
         PortableInterceptor.ORBInitializer.Register
          (PortableInterceptor.ORBInitializer.Local_Ref (Ref));
      end;
      PortableInterceptor.ORBInitializer.Initialize_All;

      Initiate_Server (True);
   end Init_Test;

begin
   New_Test ("Interceptor Flow");
   Init_Test;

   declare
      use PolyORB.CORBA_P.Server_Tools;

      Ptr : Test000_Idl.TestInterface.Impl.Object_Ptr;
      Ref : CORBA.Object.Ref;
   begin
      Ptr := new Test000_Idl.TestInterface.Impl.Object;
      Test000_Idl.TestInterface.Impl.Init (Ptr, "1");
      Initiate_Servant (PortableServer.Servant (Ptr), Ref);
      Test000_Globals.Object_1 :=
        Test000_Idl.TestInterface.Helper.To_Ref (Ref);
   end;

   --  Client Scenario 1: normal processing.

   Test000_Globals.Clear_Log;
   Test000_Globals.Enable_Client_Interceptors;
   declare
      Log : constant Log_Array
        := ((Client, 'A', Client_Send_Request),
            (Client, 'B', Client_Send_Request),
            (Client, 'C', Client_Send_Request),
            (Object, '1'),
            (Client, 'C', Client_Receive_Reply),
            (Client, 'B', Client_Receive_Reply),
            (Client, 'A', Client_Receive_Reply));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      if Get_Log = Log then
         Output ("Client Scenario 1", True);
      else
         Output ("Client Scenario 1", False);
      end if;
   exception
      when others =>
         Output ("Client Scenario 1", False);
   end;
   Test000_Globals.Disable_Client_Interceptors;

   --  Client Scenario 2: exception arrives from server.

   Test000_Globals.Clear_Log;
   Test000_Idl.TestInterface.Raise_Exception (Test000_Globals.Object_1);
   Test000_Globals.Enable_Client_Interceptors;
   declare
      Log : constant Log_Array
        := ((Client, 'A', Client_Send_Request),
            (Client, 'B', Client_Send_Request),
            (Client, 'C', Client_Send_Request),
            (Object, '1'),
            (Client, 'C', Client_Receive_Exception),
            (Client, 'B', Client_Receive_Exception),
            (Client, 'A', Client_Receive_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Client Scenario 2", False);
   exception
      when Test000_Idl.TestInterface.TestException =>
         if Get_Log = Log then
            Output ("Client Scenario 2", True);
         else
            Output ("Client Scenario 2", False);
         end if;

      when others =>
         Output ("Client Scenario 2", False);
   end;
   Test000_Globals.Disable_Client_Interceptors;
   Test000_Idl.TestInterface.Process_Normal (Test000_Globals.Object_1);

   --  Client Scenario 3: B.send_request raises exception.

   Test000_Globals.Clear_Log;
   Test000_Idl.ClientInterceptor.Set_Behavior
    (Test000_Globals.Client_B, Client_Send_Request, Raise_Exception);
   Test000_Globals.Enable_Client_Interceptors;
   declare
      Log : constant Log_Array
        := ((Client, 'A', Client_Send_Request),
            (Client, 'B', Client_Send_Request),
            (Client, 'A', Client_Receive_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Client Scenario 3", False);
   exception
      when CORBA.No_Permission =>
         if Get_Log = Log then
            Output ("Client Scenario 3", True);
         else
            Output ("Client Scenario 3", False);
         end if;

      when others =>
         Output ("Client Scenario 3", False);
   end;
   Test000_Globals.Disable_Client_Interceptors;
   Test000_Idl.ClientInterceptor.Set_Behavior
    (Test000_Globals.Client_B, Client_Send_Request, Do_Nothing);

   --  Client Scenario 4: B.receive_reply raises exception.

   Test000_Globals.Clear_Log;
   Test000_Idl.ClientInterceptor.Set_Behavior
    (Test000_Globals.Client_B, Client_Receive_Reply, Raise_Exception);
   Test000_Globals.Enable_Client_Interceptors;
   declare
      Log : constant Log_Array
        := ((Client, 'A', Client_Send_Request),
            (Client, 'B', Client_Send_Request),
            (Client, 'C', Client_Send_Request),
            (Object, '1'),
            (Client, 'C', Client_Receive_Reply),
            (Client, 'B', Client_Receive_Reply),
            (Client, 'A', Client_Receive_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Client Scenario 4", False);
   exception
      when CORBA.No_Permission =>
         if Get_Log = Log then
            Output ("Client Scenario 4", True);
         else
            Output ("Client Scenario 4", False);
         end if;

      when others =>
         Output ("Client Scenario 4", False);
   end;
   Test000_Globals.Disable_Client_Interceptors;
   Test000_Idl.ClientInterceptor.Set_Behavior
    (Test000_Globals.Client_B, Client_Receive_Reply, Do_Nothing);

   --  Client Scenario 5: exception arrives from server and
   --  B.receive_exception raise another exception.

   Test000_Globals.Clear_Log;
   Test000_Idl.TestInterface.Raise_Exception (Test000_Globals.Object_1);
   Test000_Idl.ClientInterceptor.Set_Behavior
    (Test000_Globals.Client_B, Client_Receive_Exception, Raise_Exception);
   Test000_Globals.Enable_Client_Interceptors;
   declare
      Log : constant Log_Array
        := ((Client, 'A', Client_Send_Request),
            (Client, 'B', Client_Send_Request),
            (Client, 'C', Client_Send_Request),
            (Object, '1'),
            (Client, 'C', Client_Receive_Exception),
            (Client, 'B', Client_Receive_Exception),
            (Client, 'A', Client_Receive_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Client Scenario 5", False);
   exception
      when CORBA.No_Permission =>
         if Get_Log = Log then
            Output ("Client Scenario 5", True);
         else
            Output ("Client Scenario 5", False);
         end if;

      when others =>
         Output ("Client Scenario 5", False);
   end;
   Test000_Globals.Disable_Client_Interceptors;
   Test000_Idl.ClientInterceptor.Set_Behavior
    (Test000_Globals.Client_B, Client_Receive_Exception, Do_Nothing);
   Test000_Idl.TestInterface.Process_Normal (Test000_Globals.Object_1);

   --  Server Scenario 1: normal execution.

   Test000_Globals.Clear_Log;
   Test000_Globals.Enable_Server_Interceptors;
   declare
      Log : constant Log_Array
        := ((Server, 'A', Server_Receive_Request_Service_Contexts),
            (Server, 'B', Server_Receive_Request_Service_Contexts),
            (Server, 'C', Server_Receive_Request_Service_Contexts),
            (Server, 'A', Server_Receive_Request),
            (Server, 'B', Server_Receive_Request),
            (Server, 'C', Server_Receive_Request),
            (Object, '1'),
            (Server, 'C', Server_Send_Reply),
            (Server, 'B', Server_Send_Reply),
            (Server, 'A', Server_Send_Reply));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      if Get_Log = Log then
         Output ("Server Scenario 1", True);
      else
         Output ("Server Scenario 1", False);
      end if;
   exception
      when others =>
         Output ("Server Scenario 1", False);
   end;
   Test000_Globals.Disable_Server_Interceptors;

   --  Server Scenario 2: exception.

   Test000_Globals.Clear_Log;
   Test000_Idl.TestInterface.Raise_Exception (Test000_Globals.Object_1);
   Test000_Globals.Enable_Server_Interceptors;
   declare
      Log : constant Log_Array
        := ((Server, 'A', Server_Receive_Request_Service_Contexts),
            (Server, 'B', Server_Receive_Request_Service_Contexts),
            (Server, 'C', Server_Receive_Request_Service_Contexts),
            (Server, 'A', Server_Receive_Request),
            (Server, 'B', Server_Receive_Request),
            (Server, 'C', Server_Receive_Request),
            (Object, '1'),
            (Server, 'C', Server_Send_Exception),
            (Server, 'B', Server_Send_Exception),
            (Server, 'A', Server_Send_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Server Scenario 2", False);
   exception
      when Test000_Idl.TestInterface.TestException =>
         if Get_Log = Log then
            Output ("Server Scenario 2", True);
         else
            Output ("Server Scenario 2", False);
         end if;

      when others =>
         Output ("Server Scenario 2", False);
   end;
   Test000_Globals.Disable_Server_Interceptors;
   Test000_Idl.TestInterface.Process_Normal (Test000_Globals.Object_1);

   --  Server Scenario 3: exception.

   Test000_Globals.Clear_Log;
   Test000_Idl.ServerInterceptor.Set_Behavior
    (Test000_Globals.Server_B,
     Server_Receive_Request_Service_Contexts,
     Raise_Exception);
   Test000_Globals.Enable_Server_Interceptors;
   declare
      Log : constant Log_Array
        := ((Server, 'A', Server_Receive_Request_Service_Contexts),
            (Server, 'B', Server_Receive_Request_Service_Contexts),
            (Server, 'A', Server_Send_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Server Scenario 3", False);
   exception
      when CORBA.No_Permission =>
         if Get_Log = Log then
            Output ("Server Scenario 3", True);
         else
            Output ("Server Scenario 3", False);
         end if;

      when others =>
         Output ("Server Scenario 3", False);
   end;
   Test000_Globals.Disable_Server_Interceptors;
   Test000_Idl.ServerInterceptor.Set_Behavior
    (Test000_Globals.Server_B,
     Server_Receive_Request_Service_Contexts,
     Do_Nothing);

   --  Server Scenario 4: B.Receive_Request raise exception.

   Test000_Globals.Clear_Log;
   Test000_Idl.ServerInterceptor.Set_Behavior
    (Test000_Globals.Server_B, Server_Receive_Request, Raise_Exception);
   Test000_Globals.Enable_Server_Interceptors;
   declare
      Log : constant Log_Array
        := ((Server, 'A', Server_Receive_Request_Service_Contexts),
            (Server, 'B', Server_Receive_Request_Service_Contexts),
            (Server, 'C', Server_Receive_Request_Service_Contexts),
            (Server, 'A', Server_Receive_Request),
            (Server, 'B', Server_Receive_Request),
            (Server, 'C', Server_Send_Exception),
            (Server, 'B', Server_Send_Exception),
            (Server, 'A', Server_Send_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Server Scenario 4", False);
   exception
      when CORBA.No_Permission =>
         if Get_Log = Log then
            Output ("Server Scenario 4", True);
         else
            Output ("Server Scenario 4", False);
         end if;

      when others =>
         Output ("Server Scenario 4", False);
   end;
   Test000_Globals.Disable_Server_Interceptors;
   Test000_Idl.ServerInterceptor.Set_Behavior
    (Test000_Globals.Server_B, Server_Receive_Request, Do_Nothing);

   --  Server Scenario 5: B.Send_Reply raise exception.

   Test000_Globals.Clear_Log;
   Test000_Idl.ServerInterceptor.Set_Behavior
    (Test000_Globals.Server_B, Server_Send_Reply, Raise_Exception);
   Test000_Globals.Enable_Server_Interceptors;
   declare
      Log : constant Log_Array
        := ((Server, 'A', Server_Receive_Request_Service_Contexts),
            (Server, 'B', Server_Receive_Request_Service_Contexts),
            (Server, 'C', Server_Receive_Request_Service_Contexts),
            (Server, 'A', Server_Receive_Request),
            (Server, 'B', Server_Receive_Request),
            (Server, 'C', Server_Receive_Request),
            (Object, '1'),
            (Server, 'C', Server_Send_Reply),
            (Server, 'B', Server_Send_Reply),
            (Server, 'A', Server_Send_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Server Scenario 5", False);
   exception
      when CORBA.No_Permission =>
         if Get_Log = Log then
            Output ("Server Scenario 5", True);
         else
            Output ("Server Scenario 5", False);
         end if;

      when others =>
         Output ("Server Scenario 5", False);
   end;
   Test000_Globals.Disable_Server_Interceptors;
   Test000_Idl.ServerInterceptor.Set_Behavior
    (Test000_Globals.Server_B, Server_Send_Reply, Do_Nothing);

   --  Server Scenario 6: target raise exception and B.Send_Exception
   --  raise another exception.

   Test000_Globals.Clear_Log;
   Test000_Idl.TestInterface.Raise_Exception (Test000_Globals.Object_1);
   Test000_Idl.ServerInterceptor.Set_Behavior
    (Test000_Globals.Server_B, Server_Send_Exception, Raise_Exception);
   Test000_Globals.Enable_Server_Interceptors;
   declare
      Log : constant Log_Array
        := ((Server, 'A', Server_Receive_Request_Service_Contexts),
            (Server, 'B', Server_Receive_Request_Service_Contexts),
            (Server, 'C', Server_Receive_Request_Service_Contexts),
            (Server, 'A', Server_Receive_Request),
            (Server, 'B', Server_Receive_Request),
            (Server, 'C', Server_Receive_Request),
            (Object, '1'),
            (Server, 'C', Server_Send_Exception),
            (Server, 'B', Server_Send_Exception),
            (Server, 'A', Server_Send_Exception));
   begin
      Test000_Idl.TestInterface.Proc (Test000_Globals.Object_1);
      Output ("Server Scenario 6", False);
   exception
      when CORBA.No_Permission =>
         if Get_Log = Log then
            Output ("Server Scenario 6", True);
         else
            Output ("Server Scenario 6", False);
         end if;

      when others =>
         Output ("Server Scenario 6", False);
   end;
   Test000_Globals.Disable_Server_Interceptors;
   Test000_Idl.ServerInterceptor.Set_Behavior
    (Test000_Globals.Server_B, Server_Send_Exception, Do_Nothing);
   Test000_Idl.TestInterface.Process_Normal (Test000_Globals.Object_1);

   End_Report;
   CORBA.ORB.Shutdown (False);
end Test000;
