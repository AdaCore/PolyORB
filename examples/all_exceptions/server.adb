with CORBA;       use CORBA;
with CORBA.ORB;
with CORBA.BOA;
with Ada.Text_IO;
with All_exceptions;
with All_exceptions.Impl;

procedure server is
   MyAll_exceptions : All_exceptions.Impl.Object;
   IOR : CORBA.String;
begin
   ORB.Init ("omniORB2");
   BOA.Init ("omniORB2_BOA");
   BOA.Object_Is_Ready (MyAll_exceptions);

   IOR := ORB.Object_To_String (All_Exceptions.To_Ref (MyAll_exceptions));
   Ada.Text_IO.Put_Line ("'" & To_Standard_String (IOR) & "'");

   BOA.Impl_Is_Ready;
end Server;
