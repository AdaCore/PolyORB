with GNAT.Command_Line;

with Idl_Fe.Types;
with Idl_Fe.Parser;

with Ada_Be.Expansion;
with Ada_Be.Idl2Ada;

procedure testgen is
   Rep : Idl_Fe.Types.Node_Id;
begin
   Idl_Fe.Parser.Initialize
     (GNAT.Command_Line.Get_Argument,
      Preprocess => True,
      Keep_Temporary_Files => False);

   Rep := Idl_Fe.Parser.Parse_Specification;

   Ada_Be.Expansion.Expand_Repository (Rep);

   Ada_Be.Idl2Ada.Generate (Rep, Implement => True);

end testgen;
