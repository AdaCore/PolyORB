with CORBA; use CORBA;
with CORBA.Object;
with CORBA.ORB;
with CORBA.BOA;
with Ada.Text_IO;
with Echo;
with Echo.Impl;
with CosNaming;               use CosNaming;
with CosNaming.NamingContext; use CosNaming.NamingContext;

procedure Server is
   Init_Server  : CORBA.Object.Ref;
   Root_Context : NamingContext.Ref;
   Object_Name  : Name;
   MyImpl       : Echo.Impl.Object;
   MyEcho       : Echo.Ref;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");

   BOA.Object_Is_Ready (MyImpl);

   Init_Server  := ORB.Resolve_Initial_References
     (ORB.To_CORBA_String ("NameService"));
   Root_Context := NamingContext.To_Ref (Init_Server);

   Object_Name  := To_Sequence
     ((1 => (Id     => CosNaming.To_CORBA_String ("echo"),
             Kind   => CosNaming.To_CORBA_String ("object"))));

   MyEcho := Echo.To_Ref (MyImpl);

   begin
      Bind (Root_Context, Object_Name, CORBA.Object.Ref (MyEcho));
   exception when AlreadyBound =>
      Rebind (Root_Context, Object_Name, CORBA.Object.Ref (MyEcho));
   end;
   BOA.Impl_Is_Ready;
end Server;



