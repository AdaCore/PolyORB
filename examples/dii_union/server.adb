with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.BOA;
with Ada.Text_IO;
with Modifier;
with Modifier.Impl;

procedure Server is
   Myst : Modifier.Impl.Object;
   IOR    : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");
   BOA.Object_Is_Ready (Myst);
   IOR := ORB.Object_To_String (Modifier.To_Ref (Myst));
   Ada.Text_IO.Put_Line ("'" & To_Standard_String (IOR) & "'");
   BOA.Impl_Is_Ready;
end Server;
