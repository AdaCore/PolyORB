--  Set up a test ORB.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with Droopi.Filters;
with Droopi.Log;
with Droopi.No_Tasking;
with Droopi.Obj_Adapters.Simple;
with Droopi.ORB.Task_Policies;
with Droopi.Protocols;
with Droopi.Protocols.Echo;
with Droopi.Smart_Pointers;
with Droopi.Sockets;
with Droopi.Transport.Sockets;

procedure Droopi.Setup.Test
is
   use Droopi.ORB;
   use Droopi.Sockets;
   use Droopi.Transport;
   use Droopi.Transport.Sockets;

   Server : Socket_Type;
   Addr : Sock_Addr_Type;

   Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;

   SAP : constant Transport_Access_Point_Access
     := new Socket_Access_Point;

begin
   -------------------------------
   -- Initialize all subsystems --
   -------------------------------

   Put_Line ("@@1");
   Droopi.Log.Initialize;
   --  Logging subsystem. Start this one first so we can debug
   --  problems in others.

   Put_Line ("@@2");
   Droopi.No_Tasking.Initialize;
   --  Setup soft links.

   Put_Line ("@@3");
   Droopi.Smart_Pointers.Initialize;
   --  Depends on Soft_Links.

   Put_Line ("@@4");
   Setup.The_ORB := new ORB.ORB_Type
     (Tasking_Policy_Access'(new Task_Policies.No_Tasking));
   Droopi.ORB.Create (Setup.The_ORB.all);
   --  Create ORB singleton.

   --------------------------------------
   -- Create server (listening) socket --
   --------------------------------------

   Create_Socket (Server);

   Addr.Addr := Addresses (Get_Host_By_Name ("localhost"), 1);
   Addr.Port := 9998;

   --  Allow reuse of local addresses.

   Set_Socket_Option
     (Server,
      Socket_Level,
      (Reuse_Address, True));

   loop
      begin
         Put_Line ("Binding to port" & Addr.Port'Img);
         Bind_Socket (Server, Addr);
         exit;
      exception
         when Droopi.Sockets.Socket_Error =>
            --  Address already in use.
            Addr.Port := Addr.Port + 1;
         when others =>
            raise;
      end;
   end loop;

   Listen_Socket (Server);

   Create (Socket_Access_Point (SAP.all), Server);
   Register_Access_Point
     (The_ORB,
      TAP => SAP,
      Chain => new Filters.Factory_Chain'
      (This => new Protocols.Echo.Echo_Protocol,
       Upper => null));

   ----------------------------------
   -- Create simple object adapter --
   ----------------------------------

   Obj_Adapter := new Obj_Adapters.Simple.Simple_Obj_Adapter;
   Obj_Adapters.Create (Obj_Adapter.all);

--  XXX remove:
--     Insert_Socket
--       (The_ORB, Active_Socket'
--        (The_ORB => The_ORB,
--         Kind => Listening_Sk,
--         Socket => Server,
--         Chain => new Filters.Factory_Chain'
--         (This => new Protocols.Echo.Echo_Protocol,
--          Upper => null)));

   --  Register socket with ORB object, associating a protocol
   --  to the transport service access point.

   Run (The_ORB, May_Poll => True);
   --  Execute the ORB.

end Droopi.Setup.Test;
