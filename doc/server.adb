with Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;

with PortableServer.POA;
with PortableServer.POAManager;

with Echo.Impl;

--  Setup server node: use no tasking default configuration

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Server is
begin
   CORBA.ORB.Initialize ("ORB");

   declare
      Root_POA : PortableServer.POA.Ref;

      Ref : CORBA.Object.Ref;

      Obj : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

   begin
      --  Retrieve Root POA

      Root_POA := PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      --  Set up new object

      Ref := PortableServer.POA.Servant_To_Reference
        (Root_POA, PortableServer.Servant (Obj));

      --  Output IOR

      Ada.Text_IO.Put_Line
        ("'"
         & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref))
         & "'");

      --  Launch the server

      CORBA.ORB.Run;
   end;
end Server;
