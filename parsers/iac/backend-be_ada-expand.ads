with Types; use Types;

package Backend.BE_Ada.Expand is

   function Expand_Designator
     (Identifier : Node_Id;
      Unit_Name : Node_Id)
     return Node_Id;

end Backend.BE_Ada.Expand;
