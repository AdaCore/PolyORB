with Idl_Fe.Types; use Idl_Fe.Types;

--  This is the high-level interface to the tree structure.

package Idl_Fe.Tree.Traversal is

   function Head
     (NL : Node_List)
     return N_Root_Acc;
   --  Return the first node in a node list.

   function Contents
     (Node : N_Root_Acc)
     return Node_List;
   --  Return the Contents list of an N_Repository,
   --  N_Module, N_Interface or N_Value node.

   function Parents
     (Node : N_Root_Acc)
     return Node_List;
   --  Return the list of Parents of an N_Interface.

   function Members
     (Node : N_Root_Acc)
     return Node_List;
   --  Return the list of Members of an N_Struct or N_Exception.

   function Decl
     (Node : N_Root_Acc)
     return Node_List;
   --  Return the list of Decl of an N_Member.

   function Ada_Name
     (Node : N_Root_Acc)
     return String;
   --  Return the Ada name (unqualified) of N_Named node.

   function Ada_Full_Name
     (Node : N_Root_Acc)
     return String;
   --  Return the Ada full name of N_Named Node.

   function M_Type
     (Node : N_Root_Acc)
     return N_Root_Acc;
   --  Return the M_Type of a N_Member.

end Idl_Fe.Tree.Traversal;
