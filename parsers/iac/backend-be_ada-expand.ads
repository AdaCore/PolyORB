with Types; use Types;

package Backend.BE_Ada.Expand is

   function Expand_Designator
     (N        : Node_Id;
      Witheded : Boolean := True)
     return Node_Id;

end Backend.BE_Ada.Expand;
