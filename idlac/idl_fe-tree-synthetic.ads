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

   function Name
     (Node : in Node_Id)
     return String;
   --  The name of a K_Named node.

   function Parent_Scope
     (Node : in Node_Id)
     return Node_Id;
   --  The scope wherein a K_Named node was declared.

   function Idl_Repository_Id
     (Node : in Node_Id)
     return String;
   --  Return a poor ersatz of a Repository ID in OMG IDL
   --  format for K_Named Node (as defined in "10.6 RepositoryIds").
   --  In particular, #pragma prefix, #pragma version & the like
   --  are *NOT* taken into account. FIXME: provide a proper implementation
   --  of this attribute.

   function All_Ancestors
     (Node : Node_Id;
      Exclude : Node_List := Nil_List)
     return Node_List;
   --  Return the list of all ancestors (direct or
   --  indirect) of K_Interface Node.
   --  If Exclude is not Nil_List, all nodes in Exclude
   --  are ignored during the exploration.
   --  It is up to the caller to Free the returned Node_List
   --  after use.

end Idl_Fe.Tree.Synthetic;
