with Types; use Types;

package Backend.BE_Ada.Generator is

   Var_Name_Len : Natural := 0;
   Generate_Specs : Boolean := True;
   Generate_Bodies : Boolean := True;

   procedure Generate (N : Node_Id);

end Backend.BE_Ada.Generator;
