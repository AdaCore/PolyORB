with Idl_Fe.Types; use Idl_Fe.Types;

--  Synthetised attributes of the IDL tree nodes.

package Idl_Fe.Tree.Synthetic is

   ------------------------------
   -- Properties of node lists --
   ------------------------------

   function Head
     (NL : Node_List)
     return Node_Id;
   --  Return the first node in NL.

   function Is_Empty
     (NL : Node_List)
     return Boolean;
   --  True iff NL is empty.

   function Length
     (NL : Node_List)
     return Natural;
   --  The length of a list.

   ---------------------------------------
   -- Synthetic attributes of IDL nodes --
   ---------------------------------------

   function Is_Interface_Type
     (Node : Node_Id)
     return Boolean;
   --  True iff Node is a <type_spec> that denotes an
   --  object reference type.

   function Is_Gen_Scope
     (Node : Node_Id)
     return Boolean;
   --  True iff Node is a generable Scope (ie K_Repository,
   --  K_Module, K_Interface or K_ValueType).

end Idl_Fe.Tree.Synthetic;
