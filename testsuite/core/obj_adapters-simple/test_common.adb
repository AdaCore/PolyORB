with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.Types;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.ORB;
with PolyORB.Setup;

with PolyORB.Report;
with Test_Servant;

package body Test_Common is

   use PolyORB.Types;

   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.Setup;

   use Test_Servant;

   --------------------
   -- Test_Simple_OA --
   --------------------

   procedure Test_Simple_OA
     (Obj_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access)
   is

      S1  : My_Servant_Access;
   begin
      --  Create object adapter.

      PolyORB.Obj_Adapters.Create (Obj_Adapter);

      --  Link object adapter with ORB.
      Set_Object_Adapter (The_ORB, Obj_Adapter);

      PolyORB.Report.Output ("Created Object Adapter", True);

      --  Create Servant.
      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");
      PolyORB.Report.Output ("Servant Created", True);

      --  Servant manipulation tests.

      declare
         My_Id : constant Object_Id_Access
           := new Object_Id'(PolyORB.Obj_Adapters.Export
                             (Obj_Adapter,
                              PolyORB.Servants.Servant_Access (S1)));
         --  Register it with the SOA.

         My_Ref : PolyORB.References.Ref;
      begin
         Create_Reference (The_ORB, My_Id, "POLYORB:TEST_SERVANT:1.0", My_Ref);
         --  Obtain object reference.

         PolyORB.Report.Output ("Registered object", True);

         declare
            IOR : constant String :=
              PolyORB.Types.To_Standard_String
              (PolyORB.References.IOR.Object_To_String (My_Ref));
            pragma Warnings (Off);
            pragma Unreferenced (IOR);
            pragma Warnings (On);
         begin
            PolyORB.Report.Output ("IOR created", True);
         end;

         PolyORB.Obj_Adapters.Unexport (Obj_Adapter, My_Id);
         PolyORB.Report.Output ("Unregistered object", True);
      end;

      --  Destroy object adapter
      PolyORB.Obj_Adapters.Destroy (Obj_Adapter);
      PolyORB.Report.Output ("Destroyed Object Adapter", True);
   end Test_Simple_OA;


end Test_Common;
