----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  IDL syntactic information.
--  This package embodies the grammar rules for OMG IDL.
--  The usage of the fields of record Node is defined in
--  the specification of this package (and its children).
--  $Id: //depot/ciao/main/ciao-idl_syntax.ads#24 $

with Asis;

with CIAO.Types;    use CIAO.Types;
with CIAO.IDL_Tree; use CIAO.IDL_Tree;

package CIAO.IDL_Syntax is

   -------------------------
   -- Allocator functions --
   -------------------------

   function New_Specification    return Node_Id;
   --  Allocate a new <specification>.

   function New_Include_Directive return Node_Id;
   --  Allocate a new "#include" directive pseudo-node.

   function New_Name
     (Source_Val : Asis.Program_Text)
     return Name_Id;
   --  Create a name from representation Source_Val,
   --  and return its Name_Id.

   function New_Constructed_Type_Identifier
     (Type_Declarator_Identifier : Name_Id;
      Suffix                     : ASIS.Program_Text)
     return Name_Id;
   --  Create a name for a <constructed_type> from the name
   --  of an enclosing <type_declarator> and a suffix.

   function New_Simple_Type_Spec return Node_Id;
   --  Allocate a <type_spec> node with a
   --  <simple_type_spec> subnode.

   function New_Opaque_Type      return Node_Id;
   --  Allocate a <type_spec> node that resolves to
   --  denote the CIAO Opaque type.

   function New_Standard_Name
     (Source_Val : Asis.Program_Text)
     return Node_Id;
   --  Create a <scoped_name> that denotes the IDL type
   --  representing the Standard predefined type denoted
   --  by Source_Val.

   function New_Base_Type (Base_Type_Kind : N_Base_Type_Spec)
     return Node_Id;
   --  Create a <base_type_spec> with the given kind.

   function New_Scoped_Name
     (Source_Val : Asis.Program_Text)
     return Node_Id;
   --  Create a <scoped_name> from its string representation
   --  [::]ident1::ident2::...::identN.

   function New_Constructed_Type return Node_Id;
   --  Allocate a <type_spec> with a <constr_type_spec> subnode.

   function New_Interface        return Node_Id;
   --  Allocate an <interface> with an <interface_dcl>
   --  and its <interface_header>.

   function New_Forward_Interface return Node_Id;
   --  Allocate an <interface> with a <forward_dcl>.

   function New_Operation        return Node_Id;
   --  Allocate an <op_dcl> with its <op_type_spec>.

   function New_Parameter        return Node_Id;
   --  Allocate a <param_dcl> with its <param_type_spec>
   --  and <simple_declarator>.

   function New_In_Attribute     return Node_Id;
   function New_Out_Attribute    return Node_Id;
   function New_Inout_Attribute  return Node_Id;
   --  Allocate a <param_attribute> with the given keyword value.

   function New_Void             return Node_Id;
   --  Allocate a "void" keyword node.

   ---------------------------------------------------------
   -- Allocators that insert the created node in the tree --
   ---------------------------------------------------------

   function Insert_New_Member (Parent : Node_Id)
     return Node_Id;
   --  Allocate a <member> and insert it into its <struct_type>.

   function Insert_New_Enumerator (Parent : Node_Id)
     return Node_Id;
   --  Allocate an <enumerator> and insert it into its <enum_type>.

   function Insert_New_Simple_Type_Spec (Parent : Node_Id)
     return Node_Id;
   --  Allocate a <type_spec> node with a <simple_type_spec> subnode,
   --  then set the <type_spec> of Parent to that <type_spec>.

   function Insert_New_Constructed_Type (Parent : Node_Id)
     return Node_Id;
   --  Allocate a <type_spec> with a <constr_type_spec> subnode,
   --  then set the <type_spec> of Parent to that <type_spec>.

   function Insert_New_Opaque_Type (Parent : Node_Id)
     return Node_Id;
   --  Allocate a <type_spec> node that resolves to denote the
   --  CIAO Opaque type, then set the <type_spec> of Parent to
   --  that <type_spec>.

   -------------------------------------
   -- Syntactic information accessors --
   -------------------------------------

   function Directives      (N : Node_Id)
     return List_Id;                 --  List1
   --  The preprocessor directives of a <specification>.

   function Name            (N : Node_Id)
     return Name_Id;                 --  Name1
   --  The Name of a node.
   --  Legality: the Node shall have an <identifier> subnode.

   function Get_Name (N : Node_Id)
     return Wide_String;
   --  The textual representation for the Name of node N.

   function Declarators     (N : Node_Id)
     return List_Id;                 --  List1
   --  The <declarators> of a <type_declarator> or <member>
   --  as a list of <declarator> nodes.

   function Type_Declarator (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The <type_declarator> of a <type_dcl>.

   function Type_Spec       (N : Node_Id)
     return Node_Id;                 --  Node2
   --  The <type_spec> of a <type_declarator>, <member>,
   --  or <element_spec>.

   function Specific_Type_Spec (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The <simple_type_spec> or <constr_type_spec>
   --  of a <type_spec>.
   --  The <simple_type_spec> of a <sequence_type>.

   function Structure       (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The subnode that describes the structure
   --  of a <constr_type_spec>.

   function Scoped_Name     (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The <scoped_name> in a <simple_type_spec>, if any.

   function Template_Type_Spec (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The <template_type_spec> (actually a <sequence_type>) in
   --  a <simple_type_spec>, if any.

   function Base_Type_Spec  (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The <base_type_spec> in a <simple_type_spec> or
   --  <param_type_spec>, if any.

   function Specific_Declarator (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The <simple_declarator> or <complex_declarator>
   --  of a <declarator>.

   function Definitions     (N : Node_Id)
     return List_Id;                 --  List2
   --  The <definition>s of a <specification> or <module>.
   --  The <export>s of an <interface_dcl>.

   function Members         (N : Node_Id)
     return List_Id;                 --  List2
   --  The <member>s of (the <member_list>) of a <struct_type>.
   -- XXX or of a union ! XXX

   function Enumerators     (N : Node_Id)
     return List_Id;                 --  List2
   --  The <enumerators>s of an <enum_type>.

   function Prefix          (N : Node_Id)
     return Node_Id;                 --  Node2
   --  In a <scoped_name>, the leftmost "::" or <scoped_name> at the
   --  left of the <identifier>.

   function Specific_Interface (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The <interface_dcl> or <forward_dcl> of
   --  an <interface>.

   function Interface_Header (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The <interface_header> of an <interface_dcl>

   function Inheritance_Spec (N : Node_Id)
     return List_Id;                 --  List2
   --  The <inheritance_spec> of an <interface_header>
   --  as a list of <scoped_name>s.

   function Interface_Body  (N : Node_Id)
     return List_Id;                 --  List2
   --  The <interface_body> of an <interface_dcl> as
   --  a list of <export>s.

   function Operation_Value_Type (N : Node_Id)
     return Node_Id;                 --  Node1
   --  The keyword "void" or the <param_type_spec>
   --  of an <op_type_spec>.

   function Op_Type_Spec    (N : Node_Id)
     return Node_Id;                 --  Node2
   --  The <op_type_spec> of an <op_dcl>.

   function Param_Dcls      (N : Node_Id)
     return List_Id;                 --  List3
   --  The list of <param_dcl>s of an <op_dcl>.

   function Param_Type_Spec (N : Node_Id)
     return Node_Id;                 --  Node2
   --  The <param_type_spec> of a <param_dcl>.

   function Parameter_Attribute (N : Node_Id)
     return Node_Id;                 --  Node3
   --  The <parameter_attribute> of a <param_dcl>.

   function Fixed_Array_Sizes (N : Node_Id)
     return List_Id;                 --  List2
   --  The <fixed_array_size>s of an <array_declarator>.

   function Size_Value (N : Node_Id)
     return Unbiased_Uint;           --  Uint1-unbiased
   --  The <positive_int_const> of a <fixed_array_size>.

   ------------------------------------
   -- Semantic information accessors --
   ------------------------------------

   function Constants_Interface (N : Node_Id)
     return Node_Id;                 --  Node3-Sem
   --  The <interface_dcl> node of the "Constants"
   --  interface in a <module>.

   function Interfaces      (N : Node_Id)
     return List_Id;                 --  List4-Sem
   --  The <interface_dcl>s of a <module> or <interface>.
   --  For a <module>, these are to be output
   --  after all other definitions.

   function Unit_Used (N : Node_Id)
     return Boolean;                  --  Flag1-Sem
   --  For an N_Preprocessor_Include node, True iff an entity
   --  from the corresponding library unit is used in the
   --  visible part of the current library unit.
   --  If false, the include directive can safely be omitted.

   function Is_Remote_Subprograms (N : Node_Id)
     return Boolean;                  --  Flag1-Sem
   --  For an N_Interface_Dcl node, True iff this interface
   --  is the special "remote subprograms" interface that
   --  contains the translation of a Remote Call Interface.

   function Translated_Unit (N : Node_Id)
     return Node_Id;                 --  Node2-Sem
   --  The translation of a with'ed unit (attribute
   --  of the corresponding N_Preprocessor_Include node).

   -------------------------------------------------------
   -- Corresponding Set_* procedures                    --
   -- These procedures enforce the grammatical legality --
   -- of the manipulated tree.                          --
   -------------------------------------------------------

   procedure Set_Name
     (N   : Node_Id;
      Val : Name_Id);             --  Name1

   procedure Set_Type_Declarator
     (N   : Node_Id;
      Val : Node_Id);             --  Node1

   procedure Set_Type_Spec
     (N   : Node_Id;
      Val : Node_Id);             --  Node2

   procedure Set_Specific_Type_Spec
     (N   : Node_Id;
      Val : Node_Id);             --  Node1

   procedure Set_Structure
     (N   : Node_Id;
      Val : Node_Id);             --  Node1

   procedure Set_Scoped_Name
     (N   : Node_Id;
      Val : Node_Id);             --  Node1

   procedure Set_Template_Type_Spec
     (N   : Node_Id;
      Val : Node_Id);             --  Node1

   procedure Set_Base_Type_Spec
     (N   : Node_Id;
      Val : Node_Id);             --  Node1

   procedure Set_Specific_Declarator
     (N   : Node_Id;
      Val : Node_Id);             --  Node1

   procedure Set_Prefix
     (N   : Node_Id;
      Val : Node_Id);             --  Node2

   procedure Set_Specific_Interface
     (N   : Node_Id;
      Val : Node_Id);                --  Node1

   procedure Set_Interface_Header
     (N   : Node_Id;
      Val : Node_Id);                --  Node1

   procedure Set_Operation_Value_Type
     (N   : Node_Id;
      Val : Node_Id);                --  Node1

   procedure Set_Op_Type_Spec
     (N   : Node_Id;
      Val : Node_Id);                --  Node2

   procedure Set_Param_Type_Spec
     (N   : Node_Id;
      Val : Node_Id);                --  Node2

   procedure Set_Parameter_Attribute
     (N   : Node_Id;
      Val : Node_Id);                --  Node3

   procedure Set_Size_Value
     (N   : Node_Id;
      Val : Unbiased_Uint);          --  Uint1-unbiased

   procedure Set_Constants_Interface
     (N   : Node_Id;
      Val : Node_Id);                --  Node3-Sem

   procedure Set_Unit_Used
     (N   : Node_Id;
      Val : Boolean);                --  Flag1-Sem

   procedure Set_Is_Remote_Subprograms
     (N   : Node_Id;
      Val : Boolean);                --  Flag1-Sem

   procedure Set_Translated_Unit
     (N   : Node_Id;
      Val : Node_Id);                --  Node2-Sem

   --------------------------------
   -- Add_* procedures for lists --
   --------------------------------

   procedure Add_Directive           --  List1
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Definition          --  List2
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Declarator          --  List1
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Member              --  List2
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Enumerator          --  List2
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Inherited_Interface --  List2
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Export              --  List2
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Param_Dcl           --  List3
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Fixed_Array_Size    --  List2
     (N   : Node_Id;
      Val : Node_Id);

   procedure Add_Interface           --  List4-Sem
     (N   : Node_Id;
      Val : Node_Id);

end CIAO.IDL_Syntax;
