--  Set up a test ORB.

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with CORBA;
--  For To_CORBA_String, used in References.IOR.

with Droopi.Filters;
with Droopi.Filters.Slicers;
with Droopi.Log;
with Droopi.No_Tasking;
with Droopi.Obj_Adapters.Simple;
with Droopi.Objects;
with Droopi.ORB.Task_Policies;

with Droopi.Binding_Data.Test;
with Droopi.Binding_Data.IIOP;
with Droopi.Binding_Data.SRP;

with Droopi.Protocols;
with Droopi.Protocols.Echo;
with Droopi.Protocols.GIOP;
with Droopi.Protocols.SRP;

with Droopi.References;
with Droopi.References.IOR;

with Droopi.Smart_Pointers;
with Droopi.Sockets;
with Droopi.Test_Object;
with Droopi.Transport.Sockets;

procedure Droopi.Setup.Test
is
   use Droopi.Binding_Data;
   use Droopi.Filters;
   use Droopi.Objects;
   use Droopi.ORB;
   use Droopi.Sockets;
   use Droopi.Transport;
   use Droopi.Transport.Sockets;

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
      Put ("Creating socket...");

      Create_Socket (DAP.Socket);

      DAP.Address.Addr := Addresses (Get_Host_By_Name ("localhost"), 1);
      DAP.Address.Port := Port;

      --  Allow reuse of local addresses.

      Set_Socket_Option
        (DAP.Socket,
         Socket_Level,
         (Reuse_Address, True));

      loop
         begin
            Put (" binding to port" & DAP.Address.Port'Img & "...");
            Bind_Socket (DAP.Socket, DAP.Address);
            Put_Line ("done.");
            exit;
         exception
            when Droopi.Sockets.Socket_Error =>
               --  Address already in use.
               DAP.Address.Port := DAP.Address.Port + 1;
            when others =>
               raise;
         end;
      end loop;

      Listen_Socket (DAP.Socket);

      Create (Socket_Access_Point (DAP.SAP.all), DAP.Socket);
      Create_Factory (DAP.PF.all, DAP.SAP);
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

begin

   -------------------------------
   -- Initialize all subsystems --
   -------------------------------

   Put ("Initializing subsystems...");

   Droopi.Log.Initialize;
   Put (" logging");
   --  Logging subsystem. Start this one first so we can debug
   --  problems in others.

   Droopi.No_Tasking.Initialize;
   Put (" no-tasking");
   --  Setup soft links.

   Droopi.Smart_Pointers.Initialize;
   Put (" smart-pointers");
   --  Depends on Soft_Links.

   Setup.The_ORB := new ORB.ORB_Type
     (Tasking_Policy_Access'(new Task_Policies.No_Tasking));
   Droopi.ORB.Create (Setup.The_ORB.all);
   --  Create ORB singleton.

   Put (" ORB");

   Put_Line (" done");

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

   ----------------------------------
   -- Create simple object adapter --
   ----------------------------------

   Put ("Creating object adapter...");
   Obj_Adapter := new Obj_Adapters.Simple.Simple_Obj_Adapter;
   Obj_Adapters.Create (Obj_Adapter.all);
   --  Create object adapter

   Set_Object_Adapter (The_ORB, Obj_Adapter);
   --  Link object adapter with ORB.

   My_Servant := new Test_Object.My_Object;
   --  Create application server object.

   Put_Line (" done.");

   declare
      My_Id : aliased Object_Id
        := Obj_Adapters.Export (Obj_Adapter, My_Servant);
      --  Register it with the SOA.

      My_Ref : Droopi.References.Ref;

   begin
      Obj_Adapters.Simple.Set_Interface_Description
        (Obj_Adapters.Simple.Simple_Obj_Adapter (Obj_Adapter.all),
         My_Id, Test_Object.If_Desc);
      --  Set object description.

      Create_Reference (The_ORB, My_Id'Unchecked_Access, My_Ref);
      --  Obtain object reference.

      Put_Line ("Registered object: " & Image (My_Id));
      Put_Line ("Reference is     : " & References.Image (My_Ref));
      begin
         Put_Line ("IOR is           : "
                   & CORBA.To_Standard_String
                   (References.IOR.Object_To_String
                    ((Ref => My_Ref,
                      Type_Id => CORBA.To_CORBA_String
                      ("IDL:Echo_Type")))));
      exception
         when E : others =>
            Put_Line ("Warning: Object_To_String raised:");
            Put_Line (Ada.Exceptions.Exception_Information (E));
      end;

      Run (The_ORB, May_Poll => True);
      --  Execute the ORB.
   end;

end Droopi.Setup.Test;
