with Idl_Fe.Types;          use Idl_Fe.Types;
with Ada_Be.Source_Streams; use Ada_Be.Source_Streams;

private package Ada_Be.Idl2Ada.Value_Skel is

   Suffix : constant String
     := ".Value_Skel";

   procedure Gen_Node_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);

end Ada_Be.Idl2Ada.Value_Skel;
