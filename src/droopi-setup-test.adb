--  Set up a test ORB.

--  $Id$

with Droopi.Log;
with Droopi.ORB.Task_Policies;
with Droopi.No_Tasking;
with Droopi.Sockets;

procedure Droopi.Setup.Test
is
   use Droopi.ORB;
   use Droopi.Sockets;

   Server : Socket_Type;
   Addr : Sock_Addr_Type;

begin
   -------------------------------
   -- Initialize sll aubsystems --
   -------------------------------

   Droopi.Log.Initialize;
   --  Logging subsystem.

   Droopi.No_Tasking.Initialize;
   --  Setup soft links.

   The_ORB := Droopi.ORB.Create_ORB
     (Tasking_Policy_Access'(new Task_Policies.No_Tasking));
   --  Create ORB singleton.

   --------------------------------------
   -- Create server (listening) socket --
   --------------------------------------

   Create_Socket (Server);

   Addr.Addr := Addresses (Get_Host_By_Name ("localhost"), 1);
   Addr.Port := 9998;

   Bind_Socket (Server, Addr);
   Listen_Socket (Server);


   Insert_Socket
     (The_ORB, Active_Socket'(Kind => Listening_Sk,
                              Socket => Server,
                              Protocol => null));
   --  Register socket with ORB object.

   Run (The_ORB, May_Poll => True);
   --  Execute the ORB.

end Droopi.Setup.Test;
