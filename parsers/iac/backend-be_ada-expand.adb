with Types;  use Types;

with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;

package body Backend.BE_Ada.Expand is

   ---------------------
   -- Make_Designator --
   ---------------------

   function Expand_Designator
     (Identifier : Node_Id;
      Unit_Name : Node_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Designator);
      Set_Defining_Identifier (N, Identifier);
      Set_Parent_Unit_Name (N, Unit_Name);

      if Present (Unit_Name) then
         Add_With_Package (Unit_Name);
      end if;

      return N;
   end Expand_Designator;

end Backend.BE_Ada.Expand;
