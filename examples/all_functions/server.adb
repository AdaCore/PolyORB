with CORBA; use CORBA;
with CORBA.Orb; use CORBA.Orb;
with CORBA.Boa; use CORBA.Boa;
with Ada.Text_IO;
with All_Functions;
with All_Functions.Impl;

procedure Server is
   Myobj : All_Functions.Impl.Object;
   IOR : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");
   BOA.Object_Is_Ready (Myobj);

   IOR := ORB.Object_To_String (All_Functions.To_Ref (Myobj));
   Ada.Text_IO.Put_Line ("'" & To_Standard_String (IOR) & "'");

   Impl_Is_Ready;
end Server;
