with CORBA; use CORBA;
with CORBA.ORB;
with CORBA.BOA;
with Ada.Text_IO;
with All_types;
with All_types.Impl;

procedure server is
   MyAll_Types : All_Types.Impl.Object;
   IOR : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");

   BOA.Object_Is_Ready (MyAll_Types);
   IOR := ORB.Object_To_String (All_types.To_Ref (MyAll_Types));
   Ada.Text_IO.Put_Line ("'" & To_Standard_String (IOR) & "'");

   BOA.Impl_Is_Ready;
end Server;
