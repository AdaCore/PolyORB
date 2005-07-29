with Types; use Types;

package Backend.BE_Ada.Stubs is

   --  The function below is used by the Impls package in the case of local
   --  interfaces the difference between the two functions are very tiny and
   --  dont justify the creation of a new "Is_A_Body" in the Impls package
   function Local_Is_A_Body
     (E        : Node_Id;
      Spec     : Node_Id := No_Node)
     return Node_Id;

   package Package_Spec is

      procedure Visit (E : Node_Id);

   end Package_Spec;

   package Package_Body is

      procedure Visit (E : Node_Id);

   end Package_Body;

end Backend.BE_Ada.Stubs;
