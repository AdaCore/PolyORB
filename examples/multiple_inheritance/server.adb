with CORBA; use CORBA;
with CORBA.Orb;
with CORBA.Boa;
with Ada.Text_IO;
with Tank.Impl;
with Weapon.Impl;
with Vehicle.Impl;

procedure Server is
   Myecho : Tank.Impl.Object;
   IOR : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");
   BOA.Object_Is_Ready (Myecho);

   IOR := ORB.Object_To_String (Tank.To_Ref (Myecho));
   Ada.Text_IO.Put_Line("'" & To_Standard_String (IOR) & "'");

   BOA.Impl_Is_Ready;
end Server;
