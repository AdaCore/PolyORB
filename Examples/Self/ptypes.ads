with Types;    use Types;

package Ptypes is

   pragma Remote_Call_Interface;

   type Node_Access is access all Node_Type'Class;

   procedure Register (A : Node_Access);
   function  Get_Node return Node_Access;

end Ptypes;
