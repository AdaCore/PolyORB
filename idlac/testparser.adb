with GNAT.Command_Line;
with Idl_Fe.Tree;
with Idl_Fe.Parser;
with Idl_Fe.Display_Tree;

procedure testparser is
   Rep : Idl_Fe.Tree.N_Repository_Acc;
begin
   Idl_Fe.Parser.Initialize (GNAT.Command_Line.Get_Argument,
                             True,
                             True);
   Rep := Idl_Fe.Parser.Parse_Specification;
   Idl_Fe.Display_Tree.Disp_Tree (Rep.all);
end testparser;
