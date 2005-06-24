with Types;  use Types;

with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;

package body Backend.BE_Ada.Expand is

   package BEN renames Backend.BE_Ada.Nodes;

   -----------------------
   -- Expand_Designator --
   -----------------------

   function Expand_Designator
     (N : Node_Id)
     return Node_Id
   is
      P  : Node_Id;
      U  : Node_Id;
      D  : Node_Id := No_Node;
      X  : Node_Id := N;
      FE : Node_Id;

   begin
      case Kind (N) is
         when K_Full_Type_Declaration |
           K_Subprogram_Specification =>
            P  := Parent (X);
            FE := FE_Node (X);

         when K_Object_Declaration =>
            P  := Parent (X);
            FE := FE_Node (X);

         when K_Package_Specification =>
            X  := Package_Declaration (N);
            P  := Parent (X);
            FE := FE_Node (IDL_Unit (X));

         when K_Package_Declaration =>
            P  := Parent (N);
            FE := FE_Node (IDL_Unit (X));

         when others =>
            raise Program_Error;
      end case;

      if No (P) then
         return No_Node;
      end if;

      D := New_Node (K_Designator);
      Set_Defining_Identifier
        (D, Make_Defining_Identifier
         (Name (Defining_Identifier (X))));
      Set_Corresponding_Node
        (Defining_Identifier (D),
         Corresponding_Node
         (Defining_Identifier (X)));


      if Present (FE) then
         Set_FE_Node (D, FE);
      end if;

      --  This handles the particular case of the forward declaration of
      --  interfaces.
      if Kind (N) = K_Full_Type_Declaration
        and then Present (Parent_Unit_Name (Defining_Identifier (N)))
        and then BEN.Kind
        (Corresponding_Node
         (Parent_Unit_Name
          (Defining_Identifier
           (N)))) = K_Package_Instanciation
      then
         U := New_Node (K_Designator);
         Set_Defining_Identifier
           (U, Parent_Unit_Name (Defining_Identifier (N)));
         Set_Correct_Parent_Unit_Name
           (U,
            Parent_Unit_Name
            (Parent_Unit_Name
             (Defining_Identifier (N))));
         Set_Correct_Parent_Unit_Name
           (D, U);
         P := Expand_Designator (P);
      else
         Set_Correct_Parent_Unit_Name
           (D, Expand_Designator (P));
         P := BEN.Parent_Unit_Name (D);
      end if;

      --  Adding the with clause

      if Present (P) then
         Add_With_Package (P);
      end if;

      return D;
   end Expand_Designator;

end Backend.BE_Ada.Expand;
