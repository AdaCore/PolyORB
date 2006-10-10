------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--      B A C K E N D . B E _ C O R B A _ A D A . I D L _ T O _ A D A       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Frontend.Nodes;

with Backend.BE_CORBA_Ada.Runtime;  use Backend.BE_CORBA_Ada.Runtime;

package Backend.BE_CORBA_Ada.IDL_To_Ada is

   package FEN renames Frontend.Nodes;

   function Base_Type_TC (K : FEN.Node_Kind) return Node_Id;
   --  Return the CORBA TypeCode corresponding to the IDL base type K

   type Binding is
     (B_Impl,
      B_Stub,
      B_TC,
      B_From_Any_Container,
      B_From_Any,
      B_To_Any,
      B_Raise_Excp,
      B_Initialize,
      B_To_Ref,
      B_U_To_Ref,
      B_Type_Def,
      B_Forward,
      B_Unmarshaller,
      B_Marshaller,
      B_Buffer_Size,
      B_Instantiation,
      B_Pointer_Type,
      B_Aggr_Container,
      B_Clone,
      B_Finalize_Value,
      B_Get_Aggregate_Count,
      B_Set_Aggregate_Count,
      B_Get_Aggregate_Element,
      B_Set_Aggregate_Element,
      B_Wrap,
      B_Element_Wrap,
      B_Args_Out,
      B_Args_In,
      B_Access_Args_Out);

   procedure Bind_FE_To_BE (F : Node_Id; B : Node_Id; W : Binding);
   --  To make easier the creation of the Ada tree, to minimize the
   --  number of nodes in this tree and finally to retrieve some Ada
   --  tree nodes created previously, we need to link the IDL tree and
   --  the Ada tree. The links are performed using the Bind_FE_To_BE
   --  subprogram. Bind_FE_To_BE creates a link between the IDL node F
   --  and the Ada node B according to the binding B_XXXX. The result
   --  of the execution : Bind_FE_To_BE (F, B, B_XXXX) is : B =
   --  XXXX_Node (BE_Node (F)) and F = FE_Node (B)

   function Is_Base_Type (N : Node_Id) return Boolean;
   --  Return True iff N is an IDL base type

   function Is_Object_Type (E : Node_Id) return Boolean;
   --  Returns True when the E is an interface type, when E denotes
   --  the CORBA::Object type or when it is a redefinition for one of
   --  the two first cases.

   function Is_N_Parent_Of_M (N : Node_Id; M : Node_Id) return Boolean;
   --  Return True iff the IDL node M is declared within (directly or
   --  not) the Scope N.

   function Map_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
     return Node_Id;
   --  Map an Ada designator from Type_Decl according to the Ada
   --  mapping rules. If Declarator is a complex declarator, creates
   --  an array type definition as specified by the mapping
   --  specifications

   function Map_Defining_Identifier (Entity : Node_Id) return Node_Id;
   --  Map an Ada defining identifier from the IDL node 'Entity'

   function Map_Designator (Entity : Node_Id) return Node_Id;
   --  Map an Ada designator from the IDL node 'Entity'. Handle the
   --  case of IDL base types for which we must return a CORBA type
   --  designator

   function Map_Fully_Qualified_Identifier (Entity : Node_Id) return Node_Id;
   --  Map a fully qualified Identifier (with the proper parent unit
   --  name) from 'Entity'.

   function Map_Get_Members_Spec (Member_Type : Node_Id) return Node_Id;
   --  Create the spec of the Get_Member procedure of an exception

   function Map_IDL_Unit (Entity : Node_Id) return Node_Id;
   --  Create an IDL unit from the Interface, Module or Specification
   --  'Entity' and initialize the packages of the Unit according to
   --  its kind.

   function Map_Impl_Type (Entity : Node_Id) return Node_Id;
   --  Map an Implementation type according to the properties of the
   --  interface 'Entity'.

   function Map_Impl_Type_Ancestor (Entity : Node_Id) return Node_Id;
   --  Map an Implementation parent type according to the properties
   --  of the interface 'Entity'.

   function Map_Members_Definition (Members : List_Id) return List_Id;
   --  Map a Component_List from an exception member list

   function Map_Narrowing_Designator
     (E         : Node_Id;
      Unchecked : Boolean)
     return Node_Id;
   --  Map a designator for the narrowing function corresponding to
   --  the interface type 'E'.

   function Map_Range_Constraints (Array_Sizes : List_Id) return List_Id;
   --  Create an Ada range constraint list from the IDL list Array_Sizes

   function Map_Ref_Type (Entity : Node_Id) return Node_Id;
   --  Map an reference type according to the properties of the
   --  interface 'Entity'.

   function Map_Ref_Type_Ancestor
     (Entity : Node_Id;
      Withed : Boolean := True)
     return Node_Id;
   --  Map an reference parent type according to the properties of the
   --  interface 'Entity'. If Withed is True, add the proper 'with'
   --  clause to the current package

   function Map_Raise_From_Any_Name (Entity : Node_Id) return Name_Id;
   --  Map the name of the Raise_<Exception>_From_Any

   function Map_Repository_Declaration (Entity : Node_Id) return Node_Id;
   --  Map the Repository Id constant String declaration for the IDL
   --  entity 'Entity'

   function Map_Fixed_Type_Name (F : Node_Id) return Name_Id;
   --  Map a fixed type name from the IDL node F according to the
   --  mapping specifications and handle name clashing by adding a
   --  unique suffix at the end of the name. If the
   --  Map_Fixed_Type_Helper_Name function is called twice on the same
   --  node F, it returns the same Name_Id.

   function Map_Fixed_Type_Helper_Name (F : Node_Id) return Name_Id;
   --  Map the 'Helper' instantiated package name name from the IDL
   --  node F.

   function Map_Sequence_Pkg_Name (S : Node_Id) return Name_Id;
   --  Map a Sequence package name from the Sequence Type S. Has the
   --  same name clashing handling properties as Map_Fixed_Type_Name.

   function Map_Sequence_Pkg_Helper_Name (S : Node_Id) return Name_Id;
   --  Maps Sequence package Helper name from the mapped name if the
   --  Sequence Type S.

   function Map_String_Pkg_Name (S : Node_Id) return Name_Id;
   --  Maps a Bounded String package name from the String Type S. Has
   --  the same name clashing handling properties as
   --  Map_Fixed_Type_Name.

   function Map_Variant_List
     (Alternatives   : List_Id;
      Literal_Parent : Node_Id := No_Node)
     return List_Id;
   --  Map a variant record part from an IDL union alternative list

   -------------------
   -- Stub Routines --
   -------------------

   function Map_Argument_Identifier_Name
     (P : Name_Id;
      O : Name_Id)
     return Name_Id;
   --  Maps the `PolyORB.Types.Identifier' variable name from the
   --  parameter name `P' and the operation name `O'.

   function Map_Argument_Name (P : Name_Id) return Name_Id;
   --  Maps an internal use variable name from the parameter name `P'.

   function Map_Argument_Content_Name (P : Name_Id) return Name_Id;
   --  Maps the `PolyORB.Any.Content'Class' variable name from the
   --  parameter name `P'.

   function Map_Argument_Any_Name (P : Name_Id) return Name_Id;
   --  Maps the `CORBA.Any' variable name from the parameter name `P'

   function Map_Result_Subprogram_Name (O : Name_Id) return Name_Id;
   --  Maps the name of the subprogram that set the Result_NV_� name
   --  value.

   function Map_Result_Identifier_Name (O : Name_Id) return Name_Id;
   --  Maps the `PolyORB.Types.Identifier' variable name from the
   --  operation name `O'.

   function Map_Operation_Name_Literal (O : Node_Id) return Name_Id;
   --  Maps the string literal that represents the operation name. If
   --  the operation is an attribute accessor, a '_' is appended at
   --  the beginning of the string literal.

   -------------------------
   -- Shadow Any routines --
   -------------------------

   function Map_Container_Name (E : Node_Id) return Name_Id;
   --  Maps a name for the aggregate container corresponding to the
   --  IDL type 'E'

   function Map_Indices_Name (D : Node_Id) return Name_Id;
   --  Maps and identifier for an array type declaration (used in the
   --  Shadow Any's) corresponding to the complex declarator D

   function Map_Lengths_Name (D : Node_Id) return Name_Id;
   --  Maps a name for the constant that conatains the length of an
   --  array

   function Map_Pointer_Type_Name (E : Node_Id) return Name_Id;
   --  Maps a Pointer type name corresponding to the IDL type E

   procedure Cast_When_Necessary
     (Ada_Node           : in out Node_Id;
      IDL_Immediate_Type :        Node_Id;
      IDL_Original_Type  :        Node_Id);
   --  Cast the Ada node to:
   --  a) The Ada type corresponding to IDL_Original_Type if
   --  IDL_Immediate_Type is a scoped name and its reference is a
   --  simple declarator and if IDL_Original_Type is not an Object
   --  type.
   --  b) CORBA.Object.Ref if IDL_Original_Type is an Object type and
   --  then if IDL_Immediate_Type is a scoped name.

   ----------------------------------------
   -- CORBA Predefined Entities Routines --
   ----------------------------------------

   --  The subprograms below handle the CORBA modules and the CORBA::XXXX
   --  scoped names. If the 'Implem' parameter is 'True', the returned value is
   --  the implementation type instead of the reference type

   function Get_Predefined_CORBA_Entity
     (E      : Node_Id;
      Implem : Boolean := False)
     return RE_Id;
   --  Return the runtime entity corresponding to the CORBA Predefined
   --  Entity 'E'. If E is an interface declaration node, the Implem
   --  indicate whether the user wants the reference type or the
   --  implementation type. RE_Null is returned if 'E' is not a CORBA
   --  Predefined Entity.

   function Map_Predefined_CORBA_Entity
     (E      : Node_Id;
      Implem : Boolean := False)
     return Node_Id;
   --  Use Get_Predefined_CORBA_Entity to return a designator for the
   --  runtime entity. No_Node is returned if 'E' is not a CORBA
   --  Predefined Entity.

   function Map_Predefined_CORBA_Initialize (E : Node_Id) return Node_Id;
   --  Return a designator to the Initialize function corresponding to
   --  the CORBA Predefined Entity 'E' and No_Node if 'E' is not a
   --  CORBA Predefined Entity.

   function Map_Predefined_CORBA_TC (E : Node_Id) return Node_Id;
   --  Return a designator to the TypeCode variable corresponding to
   --  the CORBA Predefined Entity 'E' and No_Node if 'E' is not a
   --  CORBA Predefined Entity.

   function Map_Predefined_CORBA_From_Any (E : Node_Id) return Node_Id;
   --  Return a designator to the From_Any function corresponding to
   --  the CORBA Predefined Entity 'E' and No_Node if 'E' is not a
   --  CORBA Predefined Entity.

   function Map_Predefined_CORBA_To_Any (E : Node_Id) return Node_Id;
   --  Return a designator to the To_Any function corresponding to the
   --  CORBA Predefined Entity 'E' and No_Node if 'E' is not a CORBA
   --  Predefined Entity.

   function Map_Predefined_CORBA_Wrap (E : Node_Id) return Node_Id;
   --  Return a designator to the Wrap function corresponding to the
   --  CORBA Predefined Entity 'E' and No_Node if 'E' is not a CORBA
   --  Predefined Entity.

   function Map_Wrap_Element_Identifier (E : Node_Id) return Node_Id;
   --  Maps a defining identifier fothe wrap element function
   --  corresponding to the sequence type E.

   --------------------------
   -- Inheritance Routines --
   --------------------------

   --  This section concerns interface inheritance.  According to the
   --  Ada mapping specification, some entities from the parent
   --  interfaces must be generated "manually" (details are given
   --  below) The "manual" code generation occurs in the stubs, the
   --  helpers, the skeletons and the implementations. The subprograms
   --  that do this generation are not exactly the same, but are very
   --  similar. So the fact of generate as many subprograms as
   --  packages is a kind of code replication.

   --  The two types below designate the Visit_XXX and the Visit
   --  functions which are different depending on which package are we
   --  generating (stub, skel, helper or impl).

   type Visit_Procedure_One_Param_Ptr is access procedure
     (E       : Node_Id);

   type Visit_Procedure_Two_Params_Ptr is access procedure
     (E       : Node_Id;
      Binding : Boolean := True);

   --  The two procedures below generate mapping for several entities
   --  declared in the parent interfaces. The generation is done
   --  recursively. During the first recursion level, the operations
   --  and attributes are generated only for the second until the last
   --  parent. During the other recursion levels, we generate the
   --  operations and attributes for all parents.

   procedure Map_Inherited_Entities_Specs
     (Current_Interface     : Node_Id;
      First_Recusrion_Level : Boolean := True;
      Visit_Operation_Subp  : Visit_Procedure_Two_Params_Ptr;
      Stub                  : Boolean := False;
      Helper                : Boolean := False;
      Skel                  : Boolean := False;
      Impl                  : Boolean := False);

   procedure Map_Inherited_Entities_Bodies
     (Current_Interface     : Node_Id;
      First_Recusrion_Level : Boolean := True;
      Visit_Operation_Subp  : Visit_Procedure_One_Param_Ptr;
      Stub                  : Boolean := False;
      Helper                : Boolean := False;
      Skel                  : Boolean := False;
      Impl                  : Boolean := False);

   --  Extract from the Ada mapping specifications :
   --  "The definitions of types, constants, and exceptions in the
   --  parent package are renamed or sub-typed so that they are also
   --  'inherited' in accordance with the IDL semantic."

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

   -----------------------------
   -- Static Request Handling --
   -----------------------------

   --  The subprograms below are related to the use of the SII when
   --  handling a request. To avoid conflicts the names of the
   --  entities generated have the operation name as prefix

   function Map_Args_Type_Identifier (E : Node_Id) return Node_Id;
   --  Create an Identifier for the Args record type of an operation

   function Map_Args_Identifier (E : Node_Id) return Node_Id;
   --  Create an Identifier for the Args record identifier of an
   --  operation

   function Map_Unmarshaller_Identifier (E : Node_Id) return Node_Id;
   --  Create an Identifier for the Unmarshaller procedure of an
   --  operation

   function Map_Marshaller_Identifier (E : Node_Id) return Node_Id;
   --  Create an Identifier for the Marshaller procedure of an
   --  operation

   function Map_Set_Args_Identifier (E : Node_Id) return Node_Id;
   --  Create an Identifier for the Set_Arg procedure of an operation

   function Map_Buffer_Size_Identifier (E : Node_Id) return Node_Id;
   --  Create an Identifier for the Buffer Size Identifier of an
   --  operation

   -------------------------------------------------
   -- Routines to resolve links between the trees --
   -------------------------------------------------

   function Get_TC_Node
     (T               : Node_Id;
      Resolve_Forward : Boolean := True)
     return Node_Id;
   --  Return the TypeCode Variable corresponding to the IDL node
   --  T. It handles base types and user defined types. If the
   --  Resolve_Forward is set and T is a forward declaration node then
   --  return the TypeCode of the forwarded entity.

   function Get_From_Any_Container_Node (T : Node_Id) return Node_Id;
   --  Return the additional From_Any function designator
   --  corresponding to the enumeration type T. It handles base types
   --  and user defined types.

   function Get_From_Any_Node (T : Node_Id) return Node_Id;
   --  Return the From_Any function designator corresponding to the
   --  IDL node T. It handles base types and user defined types.

   function Get_To_Any_Node (T : Node_Id) return Node_Id;
   --  Return the To_Any function designator corresponding to the IDL
   --  node T. It handles base types and user defined types.

   function Get_Initialize_Node
     (T               : Node_Id;
      Resolve_Forward : Boolean := True)
     return Node_Id;
   --  Return the Initialize function designator corresponding to the
   --  IDL node T. It handles base types and user defined types. If
   --  the Resolve_Forward is set and T is a forward declaration node
   --  then return the TypeCode of the forwarded entity.

   function Get_Wrap_Node (T : Node_Id) return Node_Id;
   --  Return the To_Any function designator corresponding to the IDL
   --  node T. It handles base types and user defined types.

   function Get_Type_Definition_Node (T : Node_Id) return Node_Id;
   --  Return the Ada type mapped from the IDL entity T

end Backend.BE_CORBA_Ada.IDL_To_Ada;
