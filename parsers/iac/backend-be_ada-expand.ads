with Types; use Types;

package Backend.BE_Ada.Expand is

   --  This function creates a new designator from the from the node N which
   --  may be :
   --  * a type declaration
   --  * a subprogram specification
   --  * an object declaration
   --  * a package specification
   --  * a package declaration

   --  The new created node is a designator having the same defining identifier
   --  as N. The parent unit name of the result is set basing on :
   --  * the Parent_Unit_Name of node N defining identifier, if we are handling
   --    an forward interface declaration.
   --  * the "Parent" field of N in the other cases.
   function Expand_Designator
     (N                : Node_Id)
     return Node_Id;

end Backend.BE_Ada.Expand;
