--  Set up a test ORB.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with Droopi.Filters;
with Droopi.Log;
with Droopi.ORB.Task_Policies;
with Droopi.No_Tasking;
with Droopi.Refs;
with Droopi.Protocols;
with Droopi.Protocols.Echo;
with Droopi.Sockets;

procedure Droopi.Setup.Test
is
   use Droopi.ORB;
   use Droopi.Sockets;

   Server : Socket_Type;
   Addr : Sock_Addr_Type;

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
   Droopi.Refs.Initialize;
   --  Depends on Soft_Links.

   Put_Line ("@@4");
   The_ORB := Droopi.ORB.Create_ORB
     (Tasking_Policy_Access'(new Task_Policies.No_Tasking));
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

   Insert_Socket
     (The_ORB, Active_Socket'
      (The_ORB => The_ORB,
       Kind => Listening_Sk,
       Socket => Server,
       Chain => new Filters.Factory_Chain'
       (This => new Protocols.Echo.Echo_Protocol,
        Upper => null)));

   --  Register socket with ORB object, associating a protocol
   --  to the transport service access point.

   Run (The_ORB, May_Poll => True);
   --  Execute the ORB.

end Droopi.Setup.Test;
