with GNAT.Command_Line;
with Idl_Fe.Types;
with Idl_Fe.Errors;
with Idl_Fe.Parser;
with Idl_Fe.Display_Tree;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Be.Expansion;

procedure Testexpansion is
   Rep : Idl_Fe.Types.Node_Id;
begin
   Put_Line ("Testexpansion : initializing parser");
   Idl_Fe.Parser.Initialize (GNAT.Command_Line.Get_Argument,
                             True,
                             True);
   Put_Line ("Testexpansion : Parsing");
   Rep := Idl_Fe.Parser.Parse_Specification;
   Put_Line ("Testexpansion : Expanding ");
   Ada_Be.Expansion.Expand_Repository (Rep);
   Idl_Fe.Display_Tree.Disp_Tree (Rep);
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
