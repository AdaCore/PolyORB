with Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.Object;
with Echo;
with COSNaming;               use COSNaming;
with COSNaming.NamingContext; use COSNaming.NamingContext;
with Report; use Report;

procedure Client is
   use IDL_Sequence_NameComponent;

   Init_Server  : CORBA.Object.Ref;
   Root_Context : NamingContext.Ref;
   Object_Name  : Name;
   MyEcho       : Echo.Ref;
begin
   ORB.Init ("omniORB2");
   Object_Name := CosNaming.To_Sequence
     ((1 => (Id   => CosNaming.To_CORBA_String ("echo"),
             Kind => CosNaming.To_CORBA_String ("object"))));

   Init_Server  := ORB.Resolve_Initial_References
     (ORB.To_CORBA_String ("NameService"));
   Root_Context := NamingContext.To_Ref (Init_Server);

   if Is_Nil (Root_Context) then
      Ada.Text_IO.Put_Line ("Failed to narrow root context");
      return;
   end if;

   MyEcho := Echo.To_Ref (Resolve (Root_Context, Object_Name));

   if Echo.Is_Nil (MyEcho) then
      Ada.Text_IO.Put_Line ("Unable to get Echo object");
      return;
   end if;

   Output ("test naming service",
           Echo.EchoString (MyEcho, To_CORBA_String ("Hello AdaBroker!")) =
           To_CORBA_String ("Hello AdaBroker!"));
end Client;




