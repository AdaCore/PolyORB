with PolyORB.Exceptions;
with PolyORB.Objects;
with PolyORB.References.IOR;
with PolyORB.Servants;
with PolyORB.Setup;
with PolyORB.Types;
with PolyORB.ORB;
with PolyORB.Utils.Report;

with Test_Servant;

package body Test_Common is

   use PolyORB.Exceptions;
   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.Setup;
   use PolyORB.Types;
   use PolyORB.Utils.Report;

   use Test_Servant;

   --------------------
   -- Test_Simple_OA --
   --------------------

   procedure Test_Simple_OA
     (Obj_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access)
   is

      S1  : My_Servant_Access;
      My_Id : Object_Id_Access;

      My_Ref : PolyORB.References.Ref;

      Error : Error_Container;

   begin
      --  Create object adapter.

      PolyORB.Obj_Adapters.Create (Obj_Adapter);

      --  Link object adapter with ORB.
      Set_Object_Adapter (The_ORB, Obj_Adapter);

      Output ("Created Object Adapter", True);

      --  Create Servant.
      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");
      Output ("Servant Created", True);

      PolyORB.Obj_Adapters.Export
        (Obj_Adapter,
         PolyORB.Servants.Servant_Access (S1),
         null,
         My_Id,
         Error);
      --  Register it with the SOA.

      if Found (Error) then
         raise Program_Error;
      end if;

      Create_Reference (The_ORB, My_Id, "POLYORB:TEST_SERVANT:1.0", My_Ref);
      --  Obtain object reference.

      Output ("Registered object", True);

      declare
         IOR : constant String :=
           PolyORB.Types.To_Standard_String
           (PolyORB.References.IOR.Object_To_String (My_Ref));
         pragma Warnings (Off);
         pragma Unreferenced (IOR);
         pragma Warnings (On);
      begin
         Output ("IOR created", True);
      end;

      PolyORB.Obj_Adapters.Unexport
        (Obj_Adapter,
         My_Id,
         Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      Output ("Unregistered object", True);

      --  Destroy object adapter
      PolyORB.Obj_Adapters.Destroy (Obj_Adapter);
      Output ("Destroyed Object Adapter", True);
end Test_Simple_OA;

end Test_Common;
