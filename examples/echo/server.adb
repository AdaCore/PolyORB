with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.BOA;
with Ada.Text_IO;
with Echo;
with Echo.Impl;

procedure Server is
   Myecho : Echo.Impl.Object;
   IOR    : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");
   BOA.Object_Is_Ready (Myecho);
   IOR := ORB.Object_To_String (Echo.To_Ref (MyEcho));
   Ada.Text_IO.Put_Line ("'" & To_Standard_String (IOR) & "'");
   BOA.Impl_Is_Ready;
end Server;
