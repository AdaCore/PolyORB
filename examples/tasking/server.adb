with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.BOA;
with Ada.Text_IO;
with Sema;
with Sema.Impl;

procedure Server is
   MySema : Sema.Impl.Object;
   IOR    : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");
   BOA.Object_Is_Ready (MySema);
   IOR := ORB.Object_To_String (Sema.To_Ref (MySema));
   Ada.Text_IO.Put_Line ("'" & To_Standard_String (IOR) & "'");
   BOA.Impl_Is_Ready;
end Server;
