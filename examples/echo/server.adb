with CORBA; use CORBA;
with CORBA.ORB; use CORBA.ORB;
with CORBA.BOA; use CORBA.BOA;
with Text_IO; use Text_IO;
with Echo;
with Echo.Impl;
with AdaBroker.Exceptions;

procedure server is
   -- Initialisation of The ORB
   ORB : CORBA.ORB.Object := CORBA.ORB.ORB_Init("omniORB2");

   -- Initialisation of the BOA
   BOA : CORBA.BOA.Object := CORBA.ORB.BOA_Init(ORB, "omniORB2_BOA");
   Myecho : Echo.Impl.Object;

   IOR : CORBA.String;

begin
   Put_Line ("ORB init");
   ORB := CORBA.ORB.ORB_Init ("omniORB2");

   Put_Line ("BOA init");
   BOA := CORBA.ORB.BOA_Init (ORB, "omniORB2_BOA");

   Put_Line ("Object is ready");
   Object_Is_Ready (BOA, Myecho);

   -- displays the IOR

   Put_Line ("Object To String");
   IOR := Object_To_String (Echo.To_Ref (MyEcho));

   Put_Line ("'" & To_Standard_String (IOR) & "'");

   Implementation_Is_Ready (BOA);
end Server;
