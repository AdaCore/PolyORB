with Idl_Fe.Types; use Idl_Fe.Types;

--  Synthetised attributes of the IDL tree nodes.

package Idl_Fe.Tree.Synthetic is

   ------------------------------
   -- Properties of node lists --
   ------------------------------

   function Head
     (NL : Node_List)
     return N_Root_Acc;
   --  Return the first node in NL.

   function Is_Empty
     (NL : Node_List)
     return Boolean;
   --  True iff NL is empty.

   ---------------------------------------
   -- Synthetic attributes of IDL nodes --
   ---------------------------------------

   function Ada_Name
     (Node : N_Root_Acc)
     return String;
   --  Return the Ada name (unqualified) of N_Named node.

   function Ada_Full_Name
     (Node : N_Root_Acc)
     return String;
   --  Return the Ada full name of N_Named Node.

   function Is_Interface_Type
     (Node : N_Root_Acc)
     return Boolean;
   --  True iff Node is a <type_spec> that denotes an
   --  object reference type.

   function Is_Scope
     (Node : N_Root_Acc)
     return Boolean;
   --  True iff Node is a Scope (ie N_Repository,
   --  N_Module, N_Interface or N_ValueType).

end Idl_Fe.Tree.Synthetic;
