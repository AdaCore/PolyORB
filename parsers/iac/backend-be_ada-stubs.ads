with Types; use Types;

package Backend.BE_Ada.Stubs is

   function Visible_Is_A_Spec return Node_Id;

   package Package_Spec is

      procedure Visit (E : Node_Id);

   end Package_Spec;

   package Package_Body is

      procedure Visit (E : Node_Id);

   end Package_Body;

end Backend.BE_Ada.Stubs;
