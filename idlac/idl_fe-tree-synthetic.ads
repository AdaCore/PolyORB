with Idl_Fe.Types; use Idl_Fe.Types;

--  Synthetised attributes of the IDL tree nodes.

package Idl_Fe.Tree.Synthetic is

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
   --  K_Ben_Idl_File, K_Module, K_Interface or K_ValueType).

   function Name
     (Node : in Node_Id)
     return String;
   --  The name of a K_Named node.

   function Original_Parent_Scope
     (Node : in Node_Id)
     return Node_Id;
   --  The scope wherein a K_Named node was initially
   --  declared. This property never changes once it
   --  is set by the parser.

   function Parent_Scope
     (Node : in Node_Id)
     return Node_Id;
   --  The scope wherein a K_Named node was declared. This
   --  property may be set explicitly by the expander
   --  using Set_Parent_Scope.

   procedure Set_Parent_Scope
     (Node : in Node_Id;
      To   : in Node_Id);
   --  Explicitly change the parent scope of Node to To.
   --  Intended for use only by the expander.

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

   function Integer_Value
     (Node : Node_Id)
     return Integer;
   --  Return the value of a numeric constant expression
   --  node as an integer.
   --  FIXME: This should be done in the parser with full
   --    arithmetic evalutaion. This will work only for
   --    expressions that are numeric literals.
   --    This function is here just for lack of a better
   --    way of handling numeric literals.

end Idl_Fe.Tree.Synthetic;
