with GNAT.Table;
with Types; use Types;

package Backend.BE_Ada is

   type Inheritance_Stack_Entry is record
      Node : Node_Id;
   end record;

   No_Inheritance_Depth : constant Int := -1;
   package Inheritance_Stack is
      new GNAT.Table
     (Inheritance_Stack_Entry, Int, No_Inheritance_Depth + 1, 10, 10);
   procedure Push_Package (E : Node_Id);
   procedure Pop_Package;
   function  Current_Package return Node_Id;
   --  Return current package



   procedure Generate (E : Node_Id);

end Backend.BE_Ada;
