with CORBA.Impl;
with CORBA.ORB;
with CORBA.Policy;

with PolyORB.Utils.Report;

with Echo.Helper;
with Echo.Impl;

package body Test_MyPOA is

   use CORBA;

   use PolyORB.Utils.Report;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return My_POA_Ref
   is
      Result : My_POA_Ref;

   begin
      Set (Result, CORBA.Object.Entity_Of (Self));

      return Result;
   end To_Ref;

   ----------------
   -- Test_MyPOA --
   ----------------

   procedure Run_Test_MyPOA
   is
      MyPOA : My_POA_Ref;

      Root_POA : constant PortableServer.POA.Ref
        := PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Policies : CORBA.Policy.PolicyList;

      Obj : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      Obj_Ref : Echo.Ref;

   begin
      New_Test ("User defined child of POA");

      MyPOA := To_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("My_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Attach user defined child of POA", True);

      Obj_Ref := Echo.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (PortableServer.POA.Ref (MyPOA), PortableServer.Servant (Obj)));

      Output ("Attach servant", True);

      Output ("Test invocation on servant",
              "Hello Ada World !" =
              To_Standard_String
              (Echo.echoString
               (Obj_Ref, To_CORBA_String ("Hello Ada World !"))));

   end Run_Test_MyPOA;

end Test_MyPOA;
