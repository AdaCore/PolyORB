with GNAT.Command_Line;
with Tree;
with Parse;
with Disp;

procedure testparser is
   Rep : Tree.N_Repository_Acc;
begin
   Parse.Initialize (GNAT.Command_Line.Get_Argument,
                     True,
                     True);
   Rep := Parse.Parse_Specification;
   Disp.Disp_Tree (Rep.all);
end testparser;
