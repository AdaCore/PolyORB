with Types; use Types;

package Backend.BE_Ada.Impls is

   package Package_Spec is
      procedure Visit (E : Node_Id);
   end Package_Spec;

   package Package_Body is
      procedure Visit (E : Node_Id);
   end Package_Body;

end Backend.BE_Ada.Impls;
