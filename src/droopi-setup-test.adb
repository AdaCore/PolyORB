--  Set up a test ORB.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with Droopi.Binding_Data.Test;
with Droopi.Log;
with Droopi.No_Tasking;
with Droopi.Obj_Adapters.Simple;
with Droopi.Objects;
with Droopi.ORB.Task_Policies;
with Droopi.Protocols;
with Droopi.Protocols.Echo;
with Droopi.References;
with Droopi.Smart_Pointers;
with Droopi.Sockets;
with Droopi.Test_Object;
with Droopi.Transport.Sockets;

procedure Droopi.Setup.Test
is
   use Droopi.Binding_Data;
   use Droopi.Objects;
   use Droopi.ORB;
   use Droopi.Sockets;
   use Droopi.Transport;
   use Droopi.Transport.Sockets;

   Server : Socket_Type;
   Addr : Sock_Addr_Type;

   Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;

   My_Servant : Servant_Access;

   SAP : constant Transport_Access_Point_Access
     := new Socket_Access_Point;
   SAP_PF : constant Binding_Data.Profile_Factory_Access
     := new Binding_Data.Test.Test_Profile_Factory;

begin
   -------------------------------
   -- Initialize all subsystems --
   -------------------------------

   Put ("Initializing subsystems...");

   Droopi.Log.Initialize;
   --  Logging subsystem. Start this one first so we can debug
   --  problems in others.

   Put (" logging");

   Droopi.No_Tasking.Initialize;
   --  Setup soft links.

   Put (" no-tasking");

   Droopi.Smart_Pointers.Initialize;
   --  Depends on Soft_Links.

   Put (" smart-pointers");

   Setup.The_ORB := new ORB.ORB_Type
     (Tasking_Policy_Access'(new Task_Policies.No_Tasking));
   Droopi.ORB.Create (Setup.The_ORB.all);
   --  Create ORB singleton.

   Put (" ORB");

   Put_Line (" done");

   --------------------------------------
   -- Create server (listening) socket --
   --------------------------------------

   Put ("Creating socket...");

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
         Put (" binding to port" & Addr.Port'Img & "...");
         Bind_Socket (Server, Addr);
         Put_Line ("done.");
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
   Create_Factory (SAP_PF.all, SAP);
   Register_Access_Point
     (ORB    => The_ORB,
      TAP    => SAP,
      Chain  => new Protocols.Echo.Echo_Protocol,
      PF     => SAP_PF);
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
      My_Id : constant Object_Id
        := Obj_Adapters.Export (Obj_Adapter, My_Servant);
      --  Register it with the SOA.

      My_Ref : Droopi.References.Ref;
      pragma Warnings (Off, My_Ref);
      --  XXX not referenced!

   begin
      Obj_Adapters.Simple.Set_Interface_Description
        (Obj_Adapters.Simple.Simple_Obj_Adapter (Obj_Adapter.all),
         My_Id, Test_Object.If_Desc);
      --  Set object description.

      --  Create_Reference (ORB, My_Id, My_Ref);
      --  Obtain object reference.

      Put_Line ("Registered object: " & Image (My_Id));
      Run (The_ORB, May_Poll => True);
      --  Execute the ORB.
   end;

end Droopi.Setup.Test;
