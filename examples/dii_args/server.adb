with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.BOA;
with Ada.Text_IO;
with Args;
with Args.Impl;

procedure Server is
   Myargs : Args.Impl.Object;
   IOR    : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");
   BOA.Object_Is_Ready (Myargs);
   IOR := ORB.Object_To_String (Args.To_Ref (Myargs));
   Ada.Text_IO.Put_Line ("'" & To_Standard_String (IOR) & "'");
   BOA.Impl_Is_Ready;
end Server;
