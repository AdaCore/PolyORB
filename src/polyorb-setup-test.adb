------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . S E T U P . T E S T                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Set up a test ORB.

--  $Id$

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with CORBA;
--  For To_Standard_String (display of stringified IOR).

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Filters.Slicers;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects;
with PolyORB.ORB.Interface;
with PolyORB.Protocols;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

--  A transport mechanism: sockets.

with PolyORB.Sockets;
with PolyORB.Transport.Sockets;

--  Some protocol personalities:

--  Stupid test protocol
with PolyORB.Binding_Data.Test;
with PolyORB.Protocols.Echo;

--  GIOP
with PolyORB.Binding_Data.IIOP;
with PolyORB.Protocols.GIOP;

--  SRP
with PolyORB.Binding_Data.SRP;
with PolyORB.Protocols.SRP;

--  SOAP
with PolyORB.Binding_Data.SOAP;
with PolyORB.Filters.HTTP;
with PolyORB.Protocols.SOAP_Pr;

--  Utility.
with PolyORB.References.IOR;

--  Our application object.
with PolyORB.Test_Object;

package body PolyORB.Setup.Test is

   use PolyORB.Binding_Data;
   use PolyORB.Filters;
   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Transport;
   use PolyORB.Transport.Sockets;

   Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;
   My_Servant : Servant_Access;

   ---------------------------------------------
   -- Common data for all test access points. --
   ---------------------------------------------

   type Decorated_Access_Point is record
      Socket  : Socket_Type;
      Address : Sock_Addr_Type;

      SAP : Transport_Access_Point_Access;
      PF  : Profile_Factory_Access;
   end record;

   procedure Initialize_Socket
     (DAP  : in out Decorated_Access_Point;
      Port : Port_Type);
   --  Initialize DAP.Socket and bind it to a free port,
   --  Port if possible.

   procedure Initialize_Socket
     (DAP  : in out Decorated_Access_Point;
      Port : Port_Type) is
   begin
      Put ("Socket...");

      Create_Socket (DAP.Socket);

      DAP.Address.Addr := Addresses
        (Get_Host_By_Name ("localhost"), 1);
      DAP.Address.Port := Port;

      --  Allow reuse of local addresses.

      Set_Socket_Option
        (DAP.Socket,
         Socket_Level,
         (Reuse_Address, True));

      Create
        (Socket_Access_Point (DAP.SAP.all),
         DAP.Socket,
         DAP.Address);
      if DAP.PF /= null then
         Create_Factory (DAP.PF.all, DAP.SAP);
      else
         Put_Line (" (null profile factory!)");
      end if;
      Put_Line (" done.");
   end Initialize_Socket;

   --  The 'test' access point.

   Test_Access_Point : Decorated_Access_Point
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.Test.Test_Profile_Factory);

   Echo_Protocol : aliased Protocols.Echo.Echo_Protocol;

   --  The 'GIOP' access point.

   GIOP_Access_Point : Decorated_Access_Point
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.IIOP.IIOP_Profile_Factory);

   GIOP_Protocol  : aliased Protocols.GIOP.GIOP_Protocol;
   Slicer_Factory : aliased Filters.Slicers.Slicer_Factory;

   --  The 'SRP' access point.

   SRP_Access_Point : Decorated_Access_Point
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.SRP.SRP_Profile_Factory);

   SRP_Protocol  : aliased Protocols.SRP.SRP_Protocol;

   --  The 'SOAP' access point.

   SOAP_Access_Point : Decorated_Access_Point
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.SOAP.SOAP_Profile_Factory);
   HTTP_Filter   : aliased PolyORB.Filters.HTTP.HTTP_Filter_Factory;
   SOAP_Protocol : aliased Protocols.SOAP_Pr.SOAP_Protocol;
   --  XXX
   --  It is not a very satisfying thing to have to chain
   --  HTTP_Filter and SOAP_Protocol explicitly on the server
   --  side. On the client side, this is done in Binding_Data.SOAP
   --  (as an effect of binding a SOAP object reference).
   --  Since Binding_Data encapsulates the association of a protocol
   --  with a complete transport stack, it should also provide
   --  the corresponding server-side primitive (eg as a constant
   --  filter chain created at initialisation.)

   procedure Initialize_Test_Server
     (SL_Init : Parameterless_Procedure;
      TP : ORB.Tasking_Policy_Access) is
   begin

      -------------------------------
      -- Initialize all subsystems --
      -------------------------------

      Put ("Initializing subsystems...");

      SL_Init.all;
      Put (" soft-links");
      --  Setup soft links.

      PolyORB.Smart_Pointers.Initialize;
      Put (" smart-pointers");
      --  Depends on Soft_Links.

      -------------------------------------------
      -- Initialize personality-specific stuff --
      -------------------------------------------

      PolyORB.Binding_Data.IIOP.Initialize;
      Put (" binding-iiop");

      PolyORB.Binding_Data.SOAP.Initialize;
      Put (" binding-soap");

      --------------------------
      -- Create ORB singleton --
      --------------------------

      Setup.The_ORB := new ORB.ORB_Type (TP);

      PolyORB.ORB.Create (Setup.The_ORB.all);
      Put (" ORB");

      Put_Line (" done");
   end Initialize_Test_Server;

   procedure Initialize_Test_Access_Points is
   begin

      --------------------------------------
      -- Create server (listening) socket --
      --------------------------------------

      Put ("Creating Test/Echo access point... ");
      Initialize_Socket (Test_Access_Point, 9998);
      Register_Access_Point
        (ORB    => The_ORB,
         TAP    => Test_Access_Point.SAP,
         Chain  => Echo_Protocol'Unchecked_Access,
         PF     => Test_Access_Point.PF);
      --  Register socket with ORB object, associating a protocol
      --  to the transport service access point.

      ---------------------------------------------
      -- Create server (listening) socket - GIOP --
      ---------------------------------------------

      Put ("Creating GIOP access point...");

      Initialize_Socket (GIOP_Access_Point, 10000);
      Chain_Factories ((0 => Slicer_Factory'Unchecked_Access,
                        1 => GIOP_Protocol'Unchecked_Access));
      Register_Access_Point
        (ORB    => The_ORB,
         TAP    => GIOP_Access_Point.SAP,
         Chain  => Slicer_Factory'Unchecked_Access,
         PF     => GIOP_Access_Point.PF);

      --------------------------------------------
      -- Create server (listening) socket - SRP --
      --------------------------------------------

      Put ("Creating SRP access point... ");
      Initialize_Socket (SRP_Access_Point, 10002);
      Register_Access_Point
        (ORB    => The_ORB,
         TAP    => SRP_Access_Point.SAP,
         Chain  => SRP_Protocol'Unchecked_Access,
         PF     => SRP_Access_Point.PF);
      --  Register socket with ORB object, associating a protocol
      --  to the transport service access point.

      ---------------------------------------------
      -- Create server (listening) socket - SOAP --
      ---------------------------------------------

      Put ("Creating SOAP access point... ");
      Initialize_Socket (SOAP_Access_Point, 8080);
      Chain_Factories
        ((0 => HTTP_Filter'Unchecked_Access,
          1 => SOAP_Protocol'Unchecked_Access));
      Register_Access_Point
        (ORB    => The_ORB,
         TAP    => SOAP_Access_Point.SAP,
         Chain  => HTTP_Filter'Unchecked_Access,
         PF     => SOAP_Access_Point.PF);
      --  Register socket with ORB object, associating a protocol
      --  to the transport service access point.
   end Initialize_Test_Access_Points;

   procedure Initialize_Test_Object is
   begin
      ----------------------------------
      -- Create simple object adapter --
      ----------------------------------

      Put ("Creating object adapter...");
      Obj_Adapter := new Obj_Adapters.Simple.Simple_Obj_Adapter;
      Obj_Adapters.Create (Obj_Adapter);
      --  Create object adapter

      Set_Object_Adapter (The_ORB, Obj_Adapter);
      --  Link object adapter with ORB.

      My_Servant := new Test_Object.My_Object;
      --  Create application server object.

      declare
         My_Id : Object_Id_Access
           := new Object_Id'(Obj_Adapters.Export (Obj_Adapter, My_Servant));
         --  Register it with the SOA.
      begin
         Obj_Adapters.Simple.Set_Interface_Description
           (Obj_Adapters.Simple.Simple_Obj_Adapter (Obj_Adapter.all),
            My_Id.all, Test_Object.If_Desc);
         --  Set object description.

         Create_Reference (The_ORB, My_Id, "IDL:Echo:1.0", My_Ref);
         --  Obtain object reference.

         Put_Line ("Registered object: " & Image (My_Id.all));
         Put_Line ("Reference is     : " & References.Image (My_Ref));
         begin
            Put_Line ("IOR is           : "
                      & CORBA.To_Standard_String
                      (References.IOR.Object_To_String
                       ((Ref => My_Ref))));
         exception
            when E : others =>
               Put_Line ("Warning: Object_To_String raised:");
               Put_Line (Ada.Exceptions.Exception_Information (E));
         end;
      end;
   end Initialize_Test_Object;

   procedure Run_Test is
   begin
      --  Check if we simply run the ORB to accept remote acccess
      --  or if we run a local request to the ORB
      if Ada.Command_Line.Argument_Count = 1
        and then Ada.Command_Line.Argument (1) = "local" then

         ---------------------------------------
         -- Create a local request to the ORB --
         ---------------------------------------

         declare
            use PolyORB.Any;
            use PolyORB.Any.NVList;
            use PolyORB.Components;
            use PolyORB.ORB.Interface;
            use PolyORB.Requests;
            use PolyORB.Types;

            Req : Request_Access;
            Args : Any.NVList.Ref;
            Result : Any.NamedValue;

            procedure Create_WaitAndEchoString_Request
              (Arg1 : String;
               Arg2 : Integer);


            procedure Create_WaitAndEchoString_Request
              (Arg1 : String;
               Arg2 : Integer)
            is
            begin
               Create (Args);
               Add_Item
                 (Args,
                  To_PolyORB_String ("waitAndEchoString"),
                  To_Any (To_PolyORB_String (Arg1)),
                  ARG_IN);
               Add_Item
                 (Args,
                  To_PolyORB_String ("waitAndEchoString"),
                  To_Any (Long (Arg2)),
                  ARG_IN);

               Put ("Creating servant request...  ");
               Create_Request
                 (My_Ref,
                  "waitAndEchoString",
                  Args,
                  Result,
                  Req);
               Put_Line ("Done...");

               Emit_No_Reply
                 (Component_Access (The_ORB),
                  Queue_Request'(Request   => Req,
                                 Requestor => null));
               --  Requesting_Task => null));
            end Create_WaitAndEchoString_Request;
         begin
            Create_WaitAndEchoString_Request
              ("request number 1 : wait 3 seconds", 3);
            Create_WaitAndEchoString_Request
              ("request number 2 : wait 2 seconds", 2);
            Create_WaitAndEchoString_Request
              ("request number 3 : wait 2 seconds", 2);
            Create_WaitAndEchoString_Request
              ("request number 4 : wait 2 seconds", 2);

            Run (The_ORB,
                 (Condition =>
                    Req.Completed'Access,
                  Task_Info => Req.Requesting_Task'Access),
                 May_Poll => True);
            --  Execute the ORB.
         end;

      else

         Run (The_ORB, May_Poll => True);
         --  Execute the ORB.

      end if;
   end Run_Test;

end PolyORB.Setup.Test;
