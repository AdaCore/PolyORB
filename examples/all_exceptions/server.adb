with CORBA;       use CORBA;
with CORBA.ORB;   use CORBA.ORB;
with CORBA.Boa;   use CORBA.Boa;
with Ada.Text_IO; use Ada.Text_IO;
with All_exceptions;
with All_exceptions.Impl;

procedure server is
   ORB : CORBA.ORB.Object := CORBA.ORB.ORB_Init("omniORB2");
   BOA : CORBA.BOA.Object := CORBA.ORB.BOA_Init(ORB, "omniORB2_BOA");
   MyAll_exceptions : All_exceptions.Impl.Object;
   IOR : CORBA.String;
begin
   Object_Is_Ready(BOA, MyAll_exceptions);

   IOR := Object_To_String (All_Exceptions.To_Ref (MyAll_exceptions));
   Put_Line("'" & To_Standard_String(IOR) & "'");

   Implementation_Is_Ready(BOA);
end;
