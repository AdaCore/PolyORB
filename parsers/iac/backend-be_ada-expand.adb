with Types;  use Types;

with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;

package body Backend.BE_Ada.Expand is

   -----------------------
   -- Expand_Designator --
   -----------------------

   function Expand_Designator
     (N        : Node_Id;
      Witheded : Boolean := True)
     return Node_Id
   is
      P  : Node_Id;
      D  : Node_Id := No_Node;
      X  : Node_Id := N;
      FE : Node_Id;
   begin

      case Kind (N) is
         when K_Full_Type_Declaration |
           K_Subprogram_Specification =>
            P := Parent (X);
            FE := FE_Node (X);

         when K_Package_Specification =>
            X := Package_Declaration (N);
            P := Parent (X);
            FE := FE_Node (IDL_Unit (X));

         when K_Package_Declaration =>
            P := Parent (N);
            FE := FE_Node (IDL_Unit (X));
            if No (P) then
               return No_Node;
            end if;

         when others =>
            raise Program_Error;
      end case;
      if No (FE) then
         raise Program_Error;
      end if;
      D := New_Node (K_Designator);
      Set_Defining_Identifier
        (D, Make_Defining_Identifier
         (Name (Defining_Identifier (X))));
      Set_FE_Node (D, FE);
      Set_Parent_Unit_Name
        (D, Expand_Designator (P, False));

      P := Parent_Unit_Name (D);
      if Witheded then
         if Present (P) then
            Add_With_Package (P);
         end if;
      end if;
      return D;
   end Expand_Designator;

end Backend.BE_Ada.Expand;
