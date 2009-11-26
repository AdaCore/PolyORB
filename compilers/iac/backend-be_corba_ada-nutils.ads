------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          B A C K E N D . B E _ C O R B A _ A D A . N U T I L S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2009, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Frontend.Nodes;
with Backend.BE_CORBA_Ada.Runtime;  use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Nodes;    use Backend.BE_CORBA_Ada.Nodes;

package Backend.BE_CORBA_Ada.Nutils is

   --  Frequently used values

   Int0_Val             : Value_Id; -- 0
   Int1_Val             : Value_Id; -- 1
   Int2_Val             : Value_Id; -- 2
   CORBA_Name           : Name_Id;  -- "CORBA"
   Repository_Root_Name : Name_Id;  -- "Repository_Root"
   IDL_Sequences_Name   : Name_Id;  -- "IDL_Sequences"

   Var_Suffix           : constant String := "_Ü";
   Initialized          : Boolean  := False;

   --  Ada tokens

   type Token_Type is
     (
      --   Token name      Token type
      --   Keywords
      Tok_Mod,             -- MOD   **** First Keyword
      Tok_Rem,             -- REM
      Tok_New,             -- NEW
      Tok_Abs,             -- ABS
      Tok_Others,          -- OTHERS
      Tok_Null,            -- NULL
      Tok_Delta,           -- DELTA
      Tok_Digits,          -- DIGITS
      Tok_Range,           -- RANGE
      Tok_And,             -- AND
      Tok_Or,              -- OR
      Tok_Xor,             -- XOR
      Tok_In,              -- IN
      Tok_Not,             -- NOT
      Tok_Abstract,        -- ABSTRACT
      Tok_Access,          -- ACCESS
      Tok_Aliased,         -- ALIASED
      Tok_All,             -- ALL
      Tok_Array,           -- ARRAY
      Tok_At,              -- AT
      Tok_Body,            -- BODY
      Tok_Constant,        -- CONSTANT
      Tok_Do,              -- DO
      Tok_Is,              -- IS
      Tok_Limited,         -- LIMITED
      Tok_Of,              -- OF
      Tok_Out,             -- OUT
      Tok_Record,          -- RECORD
      Tok_Renames,         -- RENAMES
      Tok_Reverse,         -- REVERSE
      Tok_Tagged,          -- TAGGED
      Tok_Then,            -- THEN
      Tok_Abort,           -- ABORT
      Tok_Accept,          -- ACCEPT
      Tok_Case,            -- CASE
      Tok_Delay,           -- DELAY
      Tok_Else,            -- ELSE
      Tok_Elsif,           -- ELSIF
      Tok_End,             -- END
      Tok_Exception,       -- EXCEPTION
      Tok_Exit,            -- EXIT
      Tok_Goto,            -- GOTO
      Tok_If,              -- IF
      Tok_Pragma,          -- PRAGMA
      Tok_Raise,           -- RAISE
      Tok_Requeue,         -- REQUEUE
      Tok_Return,          -- RETURN
      Tok_Select,          -- SELECT
      Tok_Terminate,       -- TERMINATE
      Tok_Until,           -- UNTIL
      Tok_When,            -- WHEN

      Tok_Begin,           -- BEGIN
      Tok_Declare,         -- DECLARE
      Tok_For,             -- FOR
      Tok_Loop,            -- LOOP
      Tok_While,           -- WHILE

      Tok_Entry,           -- ENTRY
      Tok_Protected,       -- PROTECTED
      Tok_Task,            -- TASK
      Tok_Type,            -- TYPE
      Tok_Subtype,         -- SUBTYPE
      Tok_Interface,       -- INTERFACE
      Tok_Overriding,      -- OVERRIDING
      Tok_Synchronized,    -- SYNCHRONIZED
      Tok_Use,             -- USE

      Tok_Function,        -- FUNCTION
      Tok_Generic,         -- GENERIC
      Tok_Package,         -- PACKAGE
      Tok_Procedure,       -- PROCEDURE

      Tok_Private,         -- PRIVATE
      Tok_With,            -- WITH
      Tok_Separate,        -- SEPARATE **** Last Keyword

      --  Graphic Characters

      Tok_Double_Asterisk, -- **
      Tok_Ampersand,       -- &
      Tok_Minus,           -- -
      Tok_Plus,            -- +
      Tok_Asterisk,        -- *
      Tok_Slash,           -- /
      Tok_Dot,             -- .
      Tok_Apostrophe,      -- '
      Tok_Left_Paren,      -- (
      Tok_Right_Paren,     -- )
      Tok_Comma,           -- ,
      Tok_Less,            -- <
      Tok_Equal,           -- =
      Tok_Greater,         -- >
      Tok_Not_Equal,       -- /=
      Tok_Greater_Equal,   -- >=
      Tok_Less_Equal,      -- <=
      Tok_Box,             -- <>
      Tok_Colon_Equal,     -- :=
      Tok_Colon,           -- :
      Tok_Greater_Greater, -- >>
      Tok_Less_Less,       -- <<
      Tok_Semicolon,       -- ;
      Tok_Arrow,           -- =>
      Tok_Vertical_Bar,    -- |
      Tok_Dot_Dot,         -- ..
      Tok_Minus_Minus);    -- --

   Token_Image : array (Token_Type) of Name_Id;
   --  A table of the images of the Ada tokens

   subtype Keyword_Type is Token_Type range Tok_Mod .. Tok_Separate;

   type Operator_Type  is
     (Op_Not,             -- not
      Op_And,             -- and
      Op_In,              -- in
      Op_Not_In,          -- not in
      Op_And_Then,        -- and then
      Op_Or,              -- or
      Op_Or_Else,         -- or else
      Op_And_Symbol,      -- &
      Op_Double_Asterisk, -- **
      Op_Minus,           -- -
      Op_Plus,            -- +
      Op_Asterisk,        -- *
      Op_Slash,           -- /
      Op_Less,            -- <
      Op_Equal,           -- =
      Op_Greater,         -- >
      Op_Not_Equal,       -- /=
      Op_Greater_Equal,   -- >=
      Op_Less_Equal,      -- <=
      Op_Box,             -- <>
      Op_Colon_Equal,     -- :=
      Op_Colon,           -- :
      Op_Greater_Greater, -- >>
      Op_Less_Less,       -- <<
      Op_Semicolon,       -- ;
      Op_Arrow,           -- =>
      Op_Vertical_Bar,    -- |
      Op_None);           -- No operation

   Operator_Image : array (Operator_Type'Pos (Op_And)
                        .. Operator_Type'Pos (Op_Vertical_Bar)) of Name_Id;

   subtype Keyword_Operator is Operator_Type
     range Operator_Type'First .. Op_Or_Else;

   --  The types XXXX_Id are used to make easier the building of the
   --  Ada identifiers. The user does not have to manipulate the name
   --  buffer. He just uses the Name_Id from the proper array type.

   type Parameter_Id is
     (P_A,
      P_ACC,
      P_Arg_List,
      P_Arg_List_In,
      P_Arg_List_Out,
      P_Arg_Modes,
      P_Args,
      P_Argument,
      P_Aux,
      P_Base_Ifs,
      P_Bound,
      P_Buffer,
      P_C,
      P_Conflicts,
      P_Container_Ref,
      P_Contexts,
      P_Count,
      P_Data_Alignment,
      P_Depends,
      P_Discriminator_Type,
      P_Dummy,
      P_E,
      P_El_C,
      P_El_CC,
      P_El_M,
      P_Element_From_Any,
      P_Element_To_Any,
      P_Element_Type,
      P_Element_Wrap,
      P_Error,
      P_Exc_List,
      P_Exception_Info,
      P_Exceptions,
      P_First_Arg_Alignment,
      P_From,
      P_From_C,
      P_Id,
      P_IDL_Digits,
      P_IDL_Type,
      P_Implicit,
      P_In_Context,
      P_Index,
      P_Init,
      P_Into,
      P_Invoke_Access,
      P_Invoke_Db,
      P_Invoke_Name_Access,
      P_Invoke_Record,
      P_Item,
      P_Label,
      P_Length,
      P_Logical_Type_Id,
      P_Mech,
      P_Members,
      P_Mode,
      P_N_Operations,
      P_Name,
      P_Name_Access,
      P_Names_Db,
      P_New_Switch,
      P_New_Union,
      P_Notepad,
      P_Null_Sequence,
      P_Obj,
      P_Operation,
      P_Operation_Name,
      P_Original_Type,
      P_Params,
      P_Parent,
      P_Payload,
      P_Provides,
      P_R_ACC,
      P_Repository_Id,
      P_Representation,
      P_Req,
      P_Req_Flags,
      P_Request,
      P_Result,
      P_Returns,
      P_Role,
      P_Scale,
      P_Self,
      P_Shutdown,
      P_Target,
      P_TC,
      P_The_Ref,
      P_To,
      P_Version,
      P_Message,
      P_Dependent_Binding_Object,
      P_X,
      P_Content,
      P_QoS);

   PN : array (Parameter_Id) of Name_Id;
   --  Array of parameter identifiers

   type Variable_Id is
     (V_Argument,
      V_Argument_List,
      V_Argument_Name,
      V_Args_In,
      V_Args_Out,
      V_Argument_Type_Id,
      V_Arg_Name_Type_Id,
      V_Context,
      V_Exception_List,
      V_Fixed_Point,
      V_Handler,
      V_Id,
      V_Impl_Object_Ptr,
      V_Index,
      V_Label,
      V_Label_Any,
      V_Members,
      V_Name,
      V_Operation_Name,
      V_Operation,
      V_Position,
      V_Req_Payload,
      V_Request,
      V_Result,
      V_Result_Name,
      V_Result_NV,
      V_Returns,
      V_Send_Request_Result,
      V_Seq,
      V_Seq_Element,
      V_Seq_Len,
      V_Type_Id,
      V_Value_Operation,
      V_Buffer_Size,
      V_Buffer,
      V_Buffer_In,
      V_Buffer_Out,
      V_CDR_Position,
      V_FXS,
      V_Error,
      V_Representation,
      V_Minor,
      V_Binding_Profile,
      V_Binding_Object,
      V_Component,
      V_Operation_Argument_List,
      V_Session,
      V_Pointer);

   VN : array (Variable_Id) of Name_Id;
   --  Array of variable identifiers

   type Subprogram_Id is
     (S_Adjust,
      S_Append,
      S_Clone,
      S_Deferred_Initialization,
      S_Entity_Of,
      S_Get_Members,
      S_Finalize,
      S_Finalize_Value,
      S_Free,
      S_From_Any,
      S_Get_Aggregate_Count,
      S_Get_Aggregate_Element,
      S_Hash,
      S_Initialize,
      S_Invoke,
      S_Is_A,
      S_Length,
      S_Marshall,
      S_Minus, -- "-"
      S_Register_Procedure,
      S_Servant_Is_A,
      S_Set,
      S_Set_Aggregate_Count,
      S_Set_Aggregate_Element,
      S_To_Abstract_Ref,
      S_To_Address,
      S_To_Any,
      S_To_Bounded_String,
      S_To_Bounded_Wide_String,
      S_To_CORBA_String,
      S_To_CORBA_Wide_String,
      S_To_Local_Ref,
      S_To_Ref,
      S_To_String,
      S_To_Wide_String,
      S_Unchecked_Get_V,
      S_Unchecked_To_Abstract_Ref,
      S_Unchecked_To_Local_Ref,
      S_Unchecked_To_Ref,
      S_Unmarshall,
      S_Wrap,
      S_Type_Size);

   SN : array (Subprogram_Id) of Name_Id;
   --  Array of subprogram identifiers

   type Component_Id is
     (C_Argument,
      C_Completed,
      C_Deferred_Arguments_Session,
      C_Dimen,
      C_Indices,
      C_Minor,
      C_Mode,
      C_Name,
      C_IDL_Type,
      C_Repr_Cache,
      C_Switch,
      C_Switch_Cache,
      C_Type_Def,
      C_V);

   CN : array (Component_Id) of Name_Id;
   --  Array of component identifiers

   type Attribute_Id is
     (A_Access,
      A_Class,
      A_First,
      A_Last,
      A_Pos,
      A_Val,
      A_Identity,
      A_Address,
      A_Repr,
      A_Size,
      A_Length,
      A_Unchecked_Access,
      A_Unrestricted_Access);

   AN : array (Attribute_Id) of Name_Id;
   --  Array of attribute identifiers

   type Type_Id is
     (T_Abstract_Ref,
      T_Bounded_String,
      T_Bounded_Wide_String,
      T_Invoke_Record_Type,
      T_Local_Ref,
      T_Object,
      T_Object_Ptr,
      T_Procedure_Access,
      T_Ref,
      T_Sequence,
      T_String_Ptr);

   TN : array (Type_Id) of Name_Id;
   --  Array of type identifiers

   type Pragma_Id is
     (Pragma_Assert,
      Pragma_Elaborate_Body,
      Pragma_Inline,
      Pragma_No_Return,
      Pragma_Style_Checks,
      Pragma_Suppress,
      Pragma_Unreferenced,
      Pragma_Warnings);

   GN : array (Pragma_Id) of Name_Id;
   --  Array of pragma identifiers

   type Error_Id is
     (E_Program_Error,
      E_Constraint_Error);

   EN : array (Error_Id) of Name_Id;
   --  Array of exception identifiers

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id)
     return Name_Id;
   --  Add the 'Prefix' string to the beginning of 'Name' and returns
   --  the corresponding name id. Note that the content of the
   --  Name_Buffer could be modified after the end of this function.

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id)
     return Name_Id;
   --  Append the 'Suffix' string to the end of 'Name' and returns the
   --  corresponding name id. Note that the content of the Name_Buffer
   --  could be modified after the end of this function.

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id)
     return Name_Id;
   --  This function returns a new name id without the Suffix. If the
   --  suffix does not exist, the returned name id is equal to the
   --  given name id.

   procedure Add_With_Package
     (E : Node_Id; Unreferenced : Boolean := False);
   --  Append a 'with' clause to the Withed_Package list of the
   --  current package. E is a Designator of the Withed package.
   --  If Unreferenced is True, generate a pragma Unreferenced.

   procedure Append_To (L : List_Id; E : Node_Id);
   --  Append node E to the end of list L

   function Convert (K : Frontend.Nodes.Node_Kind) return RE_Id;
   --  If K is an IDL base type, returns the corresponding CORBA type
   --  (according to the mapping specifications. Otherwise, raises
   --  Program_Error

   procedure Push_Entity (E : Node_Id);
   --  Push the IDL_Entity E at the Top of the IDL_Entity stack

   procedure Pop_Entity;
   --  Remove the current top of the IDL_Entity stack

   function  Current_Entity return Node_Id;
   --  Return the top of the IDL_Entity stack

   function  Current_Package return Node_Id;
   --  Return the top of the Ada Package stack

   function Copy_Node (N : Node_Id) return Node_Id;
   --  Return a recursive copy of node N and its children.
   --  Implemented for:
   --    K_Identifier
   --    K_Defining_Identifier
   --    K_Attribute_Reference.
   --    K_Selected_Component
   --    K_Literal
   --  Program_Error is raised for all other kinds.

   function Get_Declaration_Node (N : Node_Id) return Node_Id;
   --  If N is of kind K_Defininy_Identifier, return the value of its
   --  Declaration_Node field. If N is of kind K_Selected_Component,
   --  return the value of its Selector_Name's Declaration_Node
   --  field. Otherwise, raise Program_Error.

   function Get_Base_Identifier (N : Node_Id) return Node_Id;
   --  If N is a K_Identifier or K_Defining_Identifier, return it
   --  unchanged. If N is a selected subcomponent, return its
   --  Selector_Name. Otherwise, returns the Defining_Identifier of N.

   function Get_Name (N : Node_Id) return Name_Id;
   --  If N is of kind K_Defininy_Identifier or K_identifier, return
   --  the value of its Name field. If N is of kind
   --  K_Selected_Component, return the value of its Selector_Name's
   --  Name field. Otherwise, raise Program_Error.

   function Get_Value (N : Node_Id) return Value_Id;
   --  If N is a K_Literal, return its Value. If N is a
   --  K_Selected_Component, return the Value of its
   --  Selector_Name. Otherwise, raise Program_Error.

   function Get_Parent_Unit_Name (N : Node_Id) return Node_Id;
   --  If N is a selected component, return the value of its Prefix
   --  field. Otherwise, return No_Node.

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return Node_Id;
   --  Create a new Ada Node_Id of Kind 'Kind'. If the 'From' node is given,
   --  set the FE_Node of the newly created node to 'From'. 'From' is
   --  consequently assumed to designate an IDL Node_Id

   function Image (T : Token_Type) return String;
   --  Return the lower case image of token T (used to build the
   --  Token_Image table

   function Image (O : Operator_Type) return String;
   --  Return the lower case image of token T. All '_' are replaced by
   --  spaces (used to build the Operator_Image table)

   function Is_Class_Wide (E : Node_Id) return Boolean;
   --  Return True if the type specifier of IDL entity E (an operation
   --  or a parameter declaration) must be mapped into an Ada
   --  class-wide type. Extract from the Ada mapping specification
   --  V. 1.2 concerning the mapping of IDL operations : "The argument
   --  or return type shall be mapped from the IDL type except in the
   --  case of an argument or return type that is of the enclosing IDL
   --  unit type. Arguments or result types of the enclosing unit
   --  types shall be mapped to the class of the mapped reference type
   --  (for example, to Ref'Class for an constrained references)."
   --  This subprogram returns the corresponding Ada type from the
   --  given IDL parameter according to the requirements above.

   procedure Initialize;
   --  Initialize the Nutils package by initializing different tables

   procedure New_Token (T : Token_Type; I : String := "");
   --  Create a new Token and set its image to I (if given)

   function Length (L : List_Id) return Natural;
   --  Return the number of nodes in the list L

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is No_List or when Length (L) is 0

   function Copy_Expanded_Name
     (N      : Node_Id;
      Withed : Boolean := True)
     return Node_Id;
   --  copy the expanded name N add the proper 'with' clause (of the
   --  parent) if the 'Withed' flag is set.

   function Expand_Designator
     (N               : Node_Id;
      Add_With_Clause : Boolean := True)
     return Node_Id;
   --  This function creates a new designator from the node N which
   --  may be:

   --  * a type declaration
   --  * a subprogram specification
   --  * an object declaration
   --  * a package specification
   --  * a package declaration

   --  The new created node is a designator having the same defining
   --  identifier as N. The parent unit name of the result is set
   --  basing on:

   --  * the Parent_Unit_Name of node N defining identifier, if we are
   --  handling a forward interface declaration.

   --  * the "Parent" field of N in the other cases.

   ---------------------------------
   -- Ada Tree Building Functions --
   ---------------------------------

   --  Each Make_<Node_Kind> function create a Node_Id of Kind
   --  <Node_Kind>. The parameters of the function correspond usually
   --  to the fields of the Node (see the file
   --  backend-be_corba_ada-nodes.idl for more detail on the Ada tree
   --  structure).

   --  ??? The "usually" above is frightening, these factory fuctions should
   --  be generated automatically, and their signatures should correspond
   --  EXACTLY to the tree structure!

   function Make_Access_Type_Definition
     (Subtype_Indication : Node_Id;
      Is_All             : Boolean := False;
      Is_Constant        : Boolean := False;
      Is_Not_Null        : Boolean := False)
     return Node_Id;
   --  Usually used with Make_Full_Type_Declaration

   function Make_Ada_Comment
     (N                 : Name_Id;
      Has_Header_Spaces : Boolean := True)
     return Node_Id;
   --  This function does only the following thing : it creates a node
   --  whose name is the full text of the comment. It does not split
   --  the comment into many lines. This is done in the code
   --  generation phase

   function Make_Array_Aggregate (Elements : List_Id) return Node_Id;

   function Make_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id;
      Index_Definition     : Node_Id := No_Node)
     return Node_Id;
   --  Usually used with Make_Full_Type_Declaration

   function Make_String_Type_Definition
     (Defining_Identifier : Node_Id;
      Range_Constraint    : Node_Id)
     return Node_Id;
   --  Usually used with Make_Full_Type_Declaration

   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id)
     return Node_Id;

   function Make_Attribute_Reference
     (Prefix    : Node_Id;
      Attribute : Attribute_Id)
     return Node_Id;

   function Make_Block_Statement
     (Statement_Identifier : Node_Id := No_Node;
      Declarative_Part     : List_Id;
      Statements           : List_Id;
      Exception_Handler    : List_Id := No_List)
     return Node_Id;

   function Make_Case_Statement
     (Expression                  : Node_Id;
      Case_Statement_Alternatives : List_Id)
     return Node_Id;

   function Make_Case_Statement_Alternative
     (Discret_Choice_List : List_Id;
      Statements          : List_Id)
     return Node_Id;

   function Make_Component_Association
     (Selector_Name : Node_Id;
      Expression    : Node_Id)
     return Node_Id;
   --  If 'Selector_Name' is No_Node, then 'others => <Expression>'
   --  will be generated

   function Make_Component_Declaration
     (Defining_Identifier : Node_Id;
      Subtype_Indication  : Node_Id;
      Expression          : Node_Id := No_Node;
      Aliased_Present     : Boolean := False)
     return Node_Id;

   function Make_Decimal_Type_Definition
     (Definition : Node_Id)
     return Node_Id;
   --  Creates an Ada Fixed point type definition from the IDL fixed
   --  point type definition node. Usually used with
   --  Make_Full_Type_Declaration

   function Make_Identifier (Name : Name_Id) return  Node_Id;

   function Make_Defining_Identifier (Name : Name_Id) return  Node_Id;

   function Make_Derived_Type_Definition
     (Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id := No_Node;
      Is_Abstract_Type      : Boolean := False;
      Is_Private_Extension  : Boolean := False;
      Is_Subtype            : Boolean := False)
     return Node_Id;
   --  Usually used with Make_Full_Type_Declaration

   function Make_Elsif_Statement
     (Condition       : Node_Id;
      Then_Statements : List_Id)
     return Node_Id;

   function Make_Element_Association
     (Index      : Node_Id;
      Expression : Node_Id)
     return Node_Id;
   --  If 'Index' is No_Node, then 'others => <Expression>' will be
   --  generated

   function Make_Enumeration_Type_Definition
     (Enumeration_Literals : List_Id)
     return Node_Id;
   --  Usually used with Make_Full_Type_Declaration

   function Make_Exception_Declaration
     (Defining_Identifier : Node_Id;
      Renamed_Exception   : Node_Id := No_Node;
      Parent              : Node_Id := Current_Package)
     return Node_Id;

   function Make_Explicit_Dereference
     (Prefix : Node_Id)
     return Node_Id;

   function Make_Expression
     (Left_Expr  : Node_Id;
      Operator   : Operator_Type := Op_None;
      Right_Expr : Node_Id := No_Node)
     return Node_Id;

   function Make_For_Statement
     (Defining_Identifier : Node_Id;
      Range_Constraint    : Node_Id;
      Statements          : List_Id)
     return Node_Id;

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id;
      Discriminant_Spec   : List_Id := No_List;
      Parent              : Node_Id := Current_Package;
      Is_Subtype          : Boolean := False)
     return Node_Id;
   --  Parent is the package in which the Type declaration will be put
   --  (useful for further with clauses and for designator expanding)

   function Make_If_Statement
     (Condition        : Node_Id;
      Then_Statements  : List_Id;
      Elsif_Statements : List_Id := No_List;
      Else_Statements  : List_Id := No_List)
     return Node_Id;

   function Make_Indexed_Component
     (Prefix      : Node_Id;
      Expressions : List_Id)
     return Node_Id;

   function Make_Instantiated_Subprogram
     (Defining_Identifier : Node_Id;
      Parameter_List      : List_Id)
     return Node_Id;

   function New_List
     (N1 : Node_Id := No_Node;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node;
      N4 : Node_Id := No_Node;
      N5 : Node_Id := No_Node) return List_Id;
   --  Create a list which contains all the given nodes

   function Make_Literal (Value  : Value_Id) return Node_Id;

   function Make_Literal_With_Parent
     (Value  : Value_Id;
      Parent : Node_Id := No_Node)
     return Node_Id;
   --  Same as Make_Literal, except that if parent is present and Value is not
   --  No_Value, creates a selected component whose prefix is the parent and
   --  whose selector name is the literal.

   function Make_Null_Statement return Node_Id;

   function Make_Object_Declaration
     (Defining_Identifier : Node_Id;
      Constant_Present    : Boolean := False;
      Object_Definition   : Node_Id;
      Expression          : Node_Id := No_Node;
      Parent              : Node_Id := Current_Package;
      Renamed_Object      : Node_Id := No_Node;
      Aliased_Present     : Boolean := False)
     return Node_Id;
   --  Parent is the package in which the Type declaration will be put
   --  (useful for further with clauses and for designator expanding)

   function Make_Object_Instantiation
     (Qualified_Expression : Node_Id)
     return Node_Id;

   function Make_Package_Declaration (Identifier : Node_Id) return Node_Id;

   function Make_Package_Instantiation
     (Defining_Identifier : Node_Id;
      Generic_Package     : Node_Id;
      Parameter_List      : List_Id := No_List;
      Parent              : Node_Id := Current_Package)
     return Node_Id;
   --  Parent is the package in which the Type declaration will be put
   --  (useful for further with clauses and for designator expanding)

   function Make_Parameter_Association
     (Selector_Name    : Node_Id;
      Actual_Parameter : Node_Id)
     return Node_Id;

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := Mode_In;
      Expression          : Node_Id := No_Node)
      return                Node_Id;

   function Make_Pragma
     (The_Pragma    : Pragma_Id;
      Argument_List : List_Id := No_List)
     return Node_Id;

   function Make_Qualified_Expression
     (Subtype_Mark : Node_Id;
      Operand      : Node_Id)
     return Node_Id;

   function Make_Raise_Statement
     (Raised_Error : Node_Id := No_Node)
     return Node_Id;

   function Make_Range_Constraint
     (First : Node_Id; Last : Node_Id)
     return Node_Id;

   function Make_Record_Aggregate
     (L             : List_Id;
      Ancestor_Part : Node_Id := No_Node)
     return Node_Id;

   function Make_Record_Definition
     (Component_List : List_Id)
     return Node_Id;

   function Make_Record_Type_Definition
     (Record_Definition : Node_Id;
      Is_Abstract_Type  : Boolean := False;
      Is_Tagged_Type    : Boolean := False;
      Is_Limited_Type   : Boolean := False)
      return              Node_Id;

   function Make_Return_Statement
     (Expression : Node_Id)
     return Node_Id;

   function Make_Subprogram_Call
     (Defining_Identifier   : Node_Id;
      Actual_Parameter_Part : List_Id)
     return Node_Id;

   function Make_Subprogram_Body
     (Specification : Node_Id;
      Declarations  : List_Id;
      Statements    : List_Id)
     return Node_Id;

   function Make_Selected_Component
     (Prefix        : Node_Id;
      Selector_Name : Node_Id)
     return Node_Id;
   function Make_Selected_Component
     (Prefix        : Name_Id;
      Selector_Name : Name_Id)
     return Node_Id;
   --  If the prefix is No_Node (or No_Name), these functions return a
   --  simple Identifier.

   function Make_Subprogram_Specification
     (Defining_Identifier     : Node_Id;
      Parameter_Profile       : List_Id;
      Return_Type             : Node_Id := No_Node;
      Parent                  : Node_Id := Current_Package;
      Renamed_Subprogram      : Node_Id := No_Node;
      Instantiated_Subprogram : Node_Id := No_Node)
     return Node_Id;
   --  Parent is the package in which the Type declaration will be put
   --  (useful for further with clauses and for designator expanding)

   function Make_Type_Conversion
     (Subtype_Mark : Node_Id;
      Expression   : Node_Id)
     return Node_Id;

   function Make_Slice
     (Prefix         : Node_Id;
      Discrete_Range : Node_Id) return Node_Id;

   function Make_Range
     (Low_Bound  : Node_Id;
      High_Bound : Node_Id) return Node_Id;

   function Make_Used_Package (The_Used_Package : Node_Id) return Node_Id;

   function Make_Used_Type (The_Used_Type : Node_Id) return Node_Id;

   function Make_Variant_Part
     (Discriminant : Node_Id;
      Variant_List : List_Id)
     return Node_Id;

   procedure Make_Comment_Header
     (Package_Header     : List_Id;
      Package_Identifier : Node_Id);
   --  This procedure generates a comment header for the generated
   --  packages. The comment text depends on the nature of the package
   --  (editable by the user or not)

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id;
   --  This function executes Next_Node 'Num' times

   procedure Set_Forwarded (E : Node_Id);
   --  Mark the IDL node E as "Forwarded"

   function  Is_Forwarded  (E : Node_Id) return Boolean;
   --  Return True iff the node E has been marked as "Forwarded"

   --  The Set_XXXX_(Spec|Body) subprograms modifies the current_package to
   --  the Package_(Spec|Body) (XXXX_Package (N))

   procedure Set_CDR_Body (N : Node_Id := Current_Entity);
   procedure Set_CDR_Spec (N : Node_Id := Current_Entity);

   procedure Set_Aligned_Spec (N : Node_Id := Current_Entity);

   procedure Set_Buffers_Body (N : Node_Id := Current_Entity);
   procedure Set_Buffers_Spec (N : Node_Id := Current_Entity);

   procedure Set_Helper_Body (N : Node_Id := Current_Entity);
   procedure Set_Helper_Spec (N : Node_Id := Current_Entity);

   procedure Set_Internals_Body (N : Node_Id := Current_Entity);
   procedure Set_Internals_Spec (N : Node_Id := Current_Entity);

   procedure Set_Impl_Body (N : Node_Id := Current_Entity);
   procedure Set_Impl_Spec (N : Node_Id := Current_Entity);

   procedure Set_IR_Info_Body (N : Node_Id := Current_Entity);
   procedure Set_IR_Info_Spec (N : Node_Id := Current_Entity);

   procedure Set_Main_Body (N : Node_Id := Current_Entity);
   procedure Set_Main_Spec (N : Node_Id := Current_Entity);

   procedure Set_Skeleton_Body (N : Node_Id := Current_Entity);
   procedure Set_Skeleton_Spec (N : Node_Id := Current_Entity);

   function To_Ada_Name
     (N                 : Name_Id;
      Is_Operation_Name : Boolean := False) return Name_Id;
   --  Converts IDL name to Ada names. The IDL name is converted
   --  according to the Ada mapping specifications. The following
   --  modifications may be applied to the IDL name to produce the Ada
   --  name:

   --  * Any leading underscore are removed

   --  * When there are two consecutive '_', replace the second
   --  underscore with the character 'U'.

   --   * Where '_' is at the end of an identifier, add the character
   --  'U' after the underscore.

   --   * When an IDL identifier clashes with an Ada reserved word or,
   --     if Is_Operation_Name is True, with the name of a primitive operation
   --     of Ada.Finalization.Controlled, insert the string "IDL_" before the
   --     identifier.

   function To_Spec_Name (N : Name_Id) return Name_Id;
   --  Builds an internal name id used when handling runtime entities

   function Fully_Qualified_Name (N : Node_Id) return Name_Id;
   --  Returns the full name of an Ada designator or defining
   --  identifier. The separator is '.'

   --  The routines below allow the access to some global statement
   --  lists

   type GLists is
     (GL_Deferred_Initialization,
      GL_Initialization_Block,
      GL_Dependencies,
      GL_Register_IR_Info);

   procedure Initialize_GList (P : Node_Id; L : GLists);
   --  Creates a new global list for the package declaration P and
   --  makes a binding between the list and P. If the list has been
   --  already initialized, this procedure does not do anything.

   function Get_GList (P : Node_Id; L : GLists) return List_Id;
   --  Return the List_Id corresponding to the list L of the package
   --  declaration P. If the list has not been initialized, initialize
   --  it and return it.

end Backend.BE_CORBA_Ada.Nutils;
