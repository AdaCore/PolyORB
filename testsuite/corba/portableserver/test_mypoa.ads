with CORBA.Object;
with PortableServer.POA;

package Test_MyPOA is

   type My_POA_Ref is new PortableServer.POA.Ref with null record;

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return My_POA_Ref;

   procedure Run_Test_MyPOA;

end Test_MyPOA;
