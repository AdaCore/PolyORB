with Cyclic_M.Cyclic_I.Impl;
with Cyclic_M.Test_Cyclic.Impl;

with CORBA.ORB;

with PolyORB.CORBA_P.CORBALOC;     use PolyORB.CORBA_P.CORBALOC;
with PolyORB.CORBA_P.Server_Tools; use PolyORB.CORBA_P.Server_Tools;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with Ada.Text_IO;

procedure Server is
   Ref_1 : Cyclic_M.Cyclic_I.Ref;
   Ref_2 : Cyclic_M.Test_Cyclic.Ref;

begin
   CORBA.ORB.Initialize ("ORB");

   Initiate_Servant (new Cyclic_M.Cyclic_I.Impl.Object, Ref_1);
   Initiate_Servant (new Cyclic_M.Test_Cyclic.Impl.Object, Ref_2);

   Ada.Text_IO.Put_Line ("Starting Server...");
   Ada.Text_IO.Put_Line
     ("'"
      & CORBA.To_Standard_String (Object_To_Corbaloc (Ref_1))
      & "'");
   Ada.Text_IO.Put_Line
     ("'"
      & CORBA.To_Standard_String (Object_To_Corbaloc (Ref_2))
      & "'");

   Initiate_Server;
end Server;
