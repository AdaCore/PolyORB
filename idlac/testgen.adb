with GNAT.Command_Line;

with Idl_Fe.Types;
with Idl_Fe.Tree;
with Idl_Fe.Parser;

with Ada_Be.Idl2Ada;

procedure testgen is
   Rep : Idl_Fe.Tree.N_Repository_Acc;
begin
   Idl_Fe.Parser.Initialize (GNAT.Command_Line.Get_Argument,
                             True,
                             True);
   Rep := Idl_Fe.Parser.Parse_Specification;
   Ada_Be.Idl2Ada.Generate (Idl_Fe.Types.N_Root_Acc (Rep));
end testgen;
