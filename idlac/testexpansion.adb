with GNAT.Command_Line;
with Idl_Fe.Tree;
with Idl_Fe.Errors;
with Idl_Fe.Parser;
with Idl_Fe.Display_Tree;
with Ada.Text_IO;
with Ada_Be.Expansion;

procedure Testexpansion is
   Rep : Idl_Fe.Tree.N_Repository_Acc;
begin
   Idl_Fe.Parser.Initialize (GNAT.Command_Line.Get_Argument,
                             True,
                             True);
   Rep := Idl_Fe.Parser.Parse_Specification;
   Ada_Be.Expansion.Expand_Repository (Rep);
   Idl_Fe.Display_Tree.Disp_Tree (Rep.all);
   if Idl_Fe.Errors.Is_Error then
      Ada.Text_IO.Put ("there was " &
                       Natural'Image (Idl_Fe.Errors.Error_Number) &
                       " error(s)");
      if Idl_Fe.Errors.Is_Warning then
         Ada.Text_IO.Put_Line (" and " &
                               Natural'Image (Idl_Fe.Errors.Warning_Number) &
                               " warning(s) during parsing.");
      end if;
   else
      if Idl_Fe.Errors.Is_Warning then
         Ada.Text_IO.Put_Line ("there was " &
                               Natural'Image (Idl_Fe.Errors.Warning_Number) &
                               " warning(s) during parsing.");
      else
         Ada.Text_IO.Put_Line ("successfully parsed");
      end if;
   end if;
end Testexpansion;
