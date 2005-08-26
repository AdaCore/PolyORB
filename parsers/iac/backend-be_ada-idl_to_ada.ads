with Types; use Types;

with Frontend.Nodes;

with Backend.BE_Ada.Runtime;  use Backend.BE_Ada.Runtime;

package Backend.BE_Ada.IDL_To_Ada is

   package FEN renames Frontend.Nodes;

   function Base_Type_TC
     (K : FEN.Node_Kind)
     return Node_Id;

   procedure Bind_FE_To_Impl
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Helper
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Skel
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Stub
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_TC
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_From_Any
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_To_Any
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_To_Ref
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_U_To_Ref
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Type_Def
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Forward
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Instanciations
     (F : Node_Id;
      Stub_Package_Node : Node_Id := No_Node;
      Stub_Type_Node : Node_Id := No_Node;
      Helper_Package_Node : Node_Id := No_Node;
      TC_Node : Node_Id := No_Node;
      From_Any_Node : Node_Id := No_Node;
      To_Any_Node : Node_Id := No_Node);

   procedure Bind_FE_To_Args_Record
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_From_CDR
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_To_CDR
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Update_Request
     (F : Node_Id;
      B : Node_Id);

   function Is_Base_Type
     (N : Node_Id)
     return Boolean;

   --  Returns True if the E is an interface type, when E denotes the
   --  CORBA::Object type or when its a redefinition for one of the two
   --  first cases
   function Is_Object_Type
     (E : Node_Id)
     return Boolean;

   function Is_N_Parent_Of_M
     (N : Node_Id;
      M : Node_Id)
     return Boolean;

   procedure Link_BE_To_FE
     (BE : Node_Id;
      FE : Node_Id);

   function Map_Accessor_Declaration
     (Accessor  : Character;
      Attribute : Node_Id)
     return Node_Id;

   function Map_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
     return Node_Id;

   function Map_Defining_Identifier
     (Entity : Node_Id)
     return Node_Id;

   function Map_Designator
     (Entity : Node_Id)
     return Node_Id;

   function Map_Fully_Qualified_Identifier
     (Entity : Node_Id)
     return Node_Id;

   function Map_Get_Members_Spec
     (Member_Type : Node_Id)
     return Node_Id;

   function Map_IDL_Unit
     (Entity : Node_Id)
     return Node_Id;

   function Map_Impl_Type
     (Entity : Node_Id)
     return Node_Id;

   function Map_Impl_Type_Ancestor
     (Entity : Node_Id)
     return Node_Id;

   function Map_Members_Definition
     (Members : List_Id)
     return List_Id;

   function Map_Narrowing_Designator
     (E         : Node_Id;
      Unchecked : Boolean)
     return Node_Id;

   function Map_Range_Constraints
     (Array_Sizes : List_Id)
     return List_Id;

   function Map_Ref_Type
     (Entity : Node_Id)
     return Node_Id;

   function Map_Ref_Type_Ancestor
     (Entity : Node_Id)
     return Node_Id;

   function Map_Repository_Declaration
     (Entity : Node_Id)
     return Node_Id;

   function Map_Variant_List
     (Alternatives   : List_Id;
      Literal_Parent : Node_Id := No_Node)
     return List_Id;

   --  The subprograms below handle the CORBA modules and the CORBA::XXXX
   --  scoped names

   function Get_CORBA_Predefined_Entity (E : Node_Id) return RE_Id;
   function Map_Predefined_CORBA_Entitiy (E : Node_Id) return Node_Id;

   function Map_Predefined_CORBA_TC (E : Node_Id) return Node_Id;
   function Map_Predefined_CORBA_From_Any (E : Node_Id) return Node_Id;
   function Map_Predefined_CORBA_To_Any (E : Node_Id) return Node_Id;

   --  This section concerns interface inheritance.
   --  According to the Ada mapping specification, some entities from the
   --  parent interfaces must be generated "manually" (details are given below)
   --  The "manual" code generation occurs in the stubs, the helpers, the
   --  skeletons and the implementations. The subprograms that do this
   --  generation are not exactli the same, but are very similar. So the fact
   --  of generate as many subprograms as packages is a kind of code
   --  replication.

   --  The two types below designate the Visit_XXX and the Visit fuctions
   --  which are different depending on which package are we generating (stub,
   --  skel, helper or impl)

   type Visit_Procedure_One_Param_Ptr is access procedure
     (E       : Node_Id);

   type Visit_Procedure_Two_Params_Ptr is access procedure
     (E       : Node_Id;
      Binding : Boolean := True);

   --  The two procedures below generate mapping for several entities declared
   --  in the parent interfaces. The generation is done recursivly.
   --  During the first recursion level, the operations and attributes
   --  are generated  only for the second until the last parent.
   --  During the other recursion level, we generate the operations and
   --  attributes for all parents.

   procedure Map_Inherited_Entities_Specs
     (Current_Interface     : Node_Id;
      First_Recusrion_Level : Boolean := True;
      Visit_Operation_Subp  : Visit_Procedure_Two_Params_Ptr;
      Visit_Attribute_Subp  : Visit_Procedure_Two_Params_Ptr;
      Stub                  : Boolean := False;
      Helper                : Boolean := False;
      Skel                  : Boolean := False;
      Impl                  : Boolean := False);

   procedure Map_Inherited_Entities_Bodies
     (Current_Interface     : Node_Id;
      First_Recusrion_Level : Boolean := True;
      Visit_Operation_Subp  : Visit_Procedure_One_Param_Ptr;
      Visit_Attribute_Subp  : Visit_Procedure_One_Param_Ptr;
      Stub                  : Boolean := False;
      Helper                : Boolean := False;
      Skel                  : Boolean := False;
      Impl                  : Boolean := False);

   --  Extract from the Ada mapping specifications :
   --  "The definitions of types, constants, and exceptions in the
   --   parent package are renamed or subtyped so that they are also
   --   'inherited' in accordance with the IDL semantic."

   procedure Map_Additional_Entities_Specs
     (Parent_Interface : Node_Id;
      Child_Interface  : Node_Id;
      Stub             : Boolean := False;
      Helper           : Boolean := False);

   procedure Map_Additional_Entities_Bodies
     (Parent_Interface : Node_Id;
      Child_Interface  : Node_Id;
      Stub             : Boolean := False;
      Helper           : Boolean := False);

   --  The subprograms below are related to the use of the SII when handling a
   --  request. To avoid conflicts the names of the entities generated have
   --  the operation name as prefix

   function Map_Args_Type_Identifier (E : Node_Id) return Node_Id;
   function Map_Args_Identifier (E : Node_Id) return Node_Id;
   function Map_From_CDR_Identifier (E : Node_Id) return Node_Id;
   function Map_To_CDR_Identifier (E : Node_Id) return Node_Id;
   function Map_Update_Request_Identifier (E : Node_Id) return Node_Id;

end Backend.BE_Ada.IDL_To_Ada;
