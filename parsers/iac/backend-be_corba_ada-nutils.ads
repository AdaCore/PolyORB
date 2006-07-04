------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--          B A C K E N D . B E _ C O R B A _ A D A . N U T I L S           --
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
with Backend.BE_CORBA_Ada.Nodes;    use Backend.BE_CORBA_Ada.Nodes;

package Backend.BE_CORBA_Ada.Nutils is

   Int0_Val             : Value_Id;
   Int1_Val             : Value_Id;
   CORBA_Name           : Name_Id;
   Repository_Root_Name : Name_Id;
   IDL_Sequences_Name   : Name_Id;
   Var_Suffix           : constant String := "_�";
   Initialized          : Boolean  := False;

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

   subtype Keyword_Type is Token_Type
     range Tok_Mod .. Tok_Separate;

   type Operator_Type  is
     (Op_Not,             -- not
      Op_And,             -- and
      Op_In,              -- in
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

   Operator_Image : array
     (Operator_Type'Pos (Op_And) ..  Operator_Type'Pos (Op_Vertical_Bar))
     of Name_Id;

   subtype Keyword_Operator is Operator_Type
     range Operator_Type'First .. Op_Or_Else;

   type Parameter_Id is
     (P_A,
      P_Arg_List,
      P_Arg_Modes,
      P_Args,
      P_Argument,
      P_Aux,
      P_Buffer,
      P_Conflicts,
      P_Data_Alignment,
      P_Depends,
      P_E,
      P_Element_From_Any,
      P_Element_To_Any,
      P_Error,
      P_Exc_List,
      P_Exception_Info,
      P_First_Arg_Alignment,
      P_From,
      P_Implicit,
      P_In_Context,
      P_Init,
      P_Invoke_Access,
      P_Invoke_Db,
      P_Invoke_Name_Access,
      P_Invoke_Record,
      P_Item,
      P_Logical_Type_Id,
      P_Members,
      P_N_Operations,
      P_Name,
      P_Name_Access,
      P_Names_Db,
      P_Notepad,
      P_Null_Sequence,
      P_Obj,
      P_Operation,
      P_Operation_Name,
      P_Payload,
      P_Provides,
      P_Repository_Id,
      P_Representation,
      P_Req,
      P_Req_Flags,
      P_Request,
      P_Result,
      P_Returns,
      P_Role,
      P_Self,
      P_Target,
      P_The_Ref,
      P_To,
      P_Message,
      P_Dependent_Binding_Object);

   PN : array (Parameter_Id) of Name_Id;

   type Variable_Id is
     (V_Argument,
      V_Argument_List,
      V_Argument_Name,
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
      V_Returns,
      V_Self_Ref,
      V_Send_Request_Result,
      V_Seq,
      V_Seq_Element,
      V_Seq_Len,
      V_Type_Id,
      V_Value_Operation,
      V_Buffer_Size,
      V_Buffer,
      V_CDR_Position,
      V_FXS,
      V_Error,
      V_Representation,
      V_Minor,
      V_Binding_Profile,
      V_Binding_Object,
      V_Component,
      V_Operation_Argument_List,
      V_Session);

   VN : array (Variable_Id) of Name_Id;

   type Subprogram_Id is
     (S_Append,
      S_Deferred_Initialization,
      S_Element_Of,
      S_Entity_Of,
      S_Get_Members,
      S_From_Any,
      S_Hash,
      S_Initialize,
      S_Invoke,
      S_Is_A,
      S_Length,
      S_Marshall,
      S_Register_Procedure,
      S_Servant_Is_A,
      S_Set,
      S_To_Abstract_Ref,
      S_To_Any,
      S_To_Bounded_String,
      S_To_Bounded_Wide_String,
      S_To_Local_Ref,
      S_To_Ref,
      S_To_String,
      S_To_Wide_String,
      S_Unchecked_To_Abstract_Ref,
      S_Unchecked_To_Local_Ref,
      S_Unchecked_To_Ref,
      S_Unmarshall,
      S_Type_Size);

   SN : array (Subprogram_Id) of Name_Id;

   type Component_Id is
     (C_Switch);

   CN : array (Component_Id) of Name_Id;

   type Attribute_Id is
     (A_Access,
      A_Class,
      A_First,
      A_Pos,
      A_Val,
      A_Identity,
      A_Adress,
      A_Repr);

   AN : array (Attribute_Id) of Name_Id;

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

   type Pragma_Id is
     (Pragma_Elaborate_Body,
      Pragma_Inline,
      Pragma_No_Return,
      Pragma_Style_Checks,
      Pragma_Unreferenced,
      Pragma_Warnings);

   GN : array (Pragma_Id) of Name_Id;

   type Error_Id is
     (E_Program_Error,
      E_Constraint_Error);

   EN : array (Error_Id) of Name_Id;

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id)
      return Name_Id;

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id)
     return Name_Id;

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id)
     return Name_Id;
   --  This function returns a new name without the suffix. If the
   --  suffix does not exist, the returned name is equal to the given
   --  name.

   procedure Add_With_Package (E : Node_Id);

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);

   function Convert (K : Frontend.Nodes.Node_Kind) return RE_Id;

   procedure Push_Entity (E : Node_Id);
   procedure Pop_Entity;
   function  Current_Entity return Node_Id;
   function  Current_Package return Node_Id;

   function Copy_Node
     (N : Node_Id)
     return Node_Id;

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return Node_Id;
   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return List_Id;

   function Image (T : Token_Type) return String;
   function Image (O : Operator_Type) return String;
   procedure Initialize;
   procedure New_Token (T : Token_Type; I : String := "");

   function Length
     (L : List_Id)
     return Natural;

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function Copy_Designator
     (Designator : Node_Id;
      Witheded   : Boolean := True)
     return Node_Id;

   function Defining_Identifier_To_Designator
     (N                       : Node_Id;
      Copy                    : Boolean := False;
      Keep_Parent             : Boolean := True;
      Keep_Corresponding_Node : Boolean := True)
     return Node_Id;

   function Get_TC_Node
     (T               : Node_Id;
      Resolve_Forward : Boolean := True)
     return Node_Id;
   --  This function return the TypeCode Variable. It handles base
   --  types and user defined types. If the Resolve_Forward is set and
   --  T is a forward declaration node then return the TypeCode of the
   --  forwarded entity

   function Get_From_Any_Node (T : Node_Id) return Node_Id;
   --  This function return the From_Any function. It handles base
   --  types and user defined types

   function Get_To_Any_Node (T : Node_Id) return Node_Id;
   --  This function return the To_Any function. It handles base types
   --  and user defined types

   function Get_Initialize_Node
     (T               : Node_Id;
      Resolve_Forward : Boolean := True)
     return Node_Id;
   --  This function return the Initialize function. It handles only
   --  user defined types. If the Resolve_Forward is set and
   --  T is a forward declaration node then return the TypeCode of the
   --  forwarded entity

   function Make_Access_Type_Definition
     (Subtype_Indication : Node_Id;
      Is_All             : Boolean := False;
      Is_Constant        : Boolean := False)
     return Node_Id;

   function Make_Ada_Comment
     (N                 : Name_Id;
      Has_Header_Spaces : Boolean := True)
     return Node_Id;
   --  This function does only the fllowing thing : it creates a node
   --  whose name is the full text of the comment. It does not split
   --  the comment into many lines. This is done in the code
   --  generation phase

   function Make_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id)
     return Node_Id;

   function Make_String_Type_Definition
     (Defining_Identifier : Node_Id;
      Range_Constraint    : Node_Id)
     return Node_Id;

   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id)
     return Node_Id;

   function Make_Attribute_Designator
     (Prefix    : Node_Id;
      Attribute : Attribute_Id)
     return Node_Id;

   function Make_Block_Statement
     (Statement_Identifier : Node_Id := No_Node;
      Declarative_Part     : List_Id;
      Statements           : List_Id;
      Exception_Handler    : List_Id := No_List)
     return Node_Id;

   function Make_Case_Label (Value : Value_Id) return Node_Id;

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

   function Make_Component_Declaration
     (Defining_Identifier : Node_Id;
      Subtype_Indication  : Node_Id;
      Expression          : Node_Id := No_Node)
     return Node_Id;

   function Make_Decimal_Type_Definition
     (Definition : Node_Id)
     return Node_Id;

   function Make_Defining_Identifier
     (Name  : Name_Id)
     return  Node_Id;

   function Make_Derived_Type_Definition
     (Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id := No_Node;
      Is_Abstract_Type      : Boolean := False;
      Is_Private_Extention  : Boolean := False;
      Is_Subtype            : Boolean := False)
     return Node_Id;

   function Make_Designator
     (Designator : Name_Id;
      Parent     : Name_Id := No_Name;
      Is_All     : Boolean := False)
     return Node_Id;

   function Make_Elsif_Statement
     (Condition       : Node_Id;
      Then_Statements : List_Id)
     return Node_Id;

   function Make_Enumeration_Type_Definition
     (Enumeration_Literals : List_Id)
     return Node_Id;

   function Make_Exception_Declaration
     (Defining_Identifier : Node_Id;
      Renamed_Exception   : Node_Id := No_Node)
     return Node_Id;

   function Make_Expression
     (Left_Expr : Node_Id;
      Operator  : Operator_Type := Op_None;
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
      Parent              : Node_Id := No_Node;
      Is_Subtype          : Boolean := False)
     return Node_Id;

   function Make_If_Statement
     (Condition        : Node_Id;
      Then_Statements  : List_Id;
      Elsif_Statements : List_Id := No_List;
      Else_Statements  : List_Id := No_List)
     return Node_Id;

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node;
      N4 : Node_Id := No_Node)
     return List_Id;

   function Make_Literal
     (Value             : Value_Id;
      Parent_Designator : Node_Id := No_Node)
     return Node_Id;

   function Make_Null_Statement
     return Node_Id;

   function Make_Object_Declaration
     (Defining_Identifier : Node_Id;
      Constant_Present    : Boolean := False;
      Object_Definition   : Node_Id;
      Expression          : Node_Id := No_Node;
      Parent              : Node_Id := No_Node;
      Renamed_Object      : Node_Id := No_Node;
      Aliased_Present     : Boolean := False)
     return                Node_Id;

   function Make_Object_Instanciation
     (Qualified_Expression : Node_Id)
     return Node_Id;

   function Make_Package_Declaration
     (Identifier : Node_Id)
     return Node_Id;

   function Make_Package_Instantiation
     (Defining_Identifier : Node_Id;
      Generic_Package     : Node_Id;
      Parameter_List      : List_Id := No_List;
      Parent              : Node_Id := Current_Package)
     return Node_Id;

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := Mode_In;
      Expression          : Node_Id := No_Node)
      return                Node_Id;

   function Make_Pragma_Statement
     (The_Pragma    : Pragma_Id;
      Argument_List : List_Id := No_List)
     return Node_Id;

   function Make_Qualified_Expression
     (Subtype_Mark  : Node_Id;
      Expression    : Node_Id := No_Node;
      Aggregate     : Node_Id)
     return Node_Id;

   function Make_Raise_Statement
     (Raised_Error  : Node_Id := No_Node)
     return Node_Id;

   function Make_Record_Aggregate
     (L : List_Id)
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
     (Defining_Identifier : Node_Id;
      Actual_Parameter_Part : List_Id)
     return Node_Id;

   function Make_Subprogram_Implementation
     (Specification : Node_Id;
      Declarations  : List_Id;
      Statements    : List_Id)
      return          Node_Id;

   function Make_Subprogram_Specification
     (Defining_Identifier : Node_Id;
      Parameter_Profile   : List_Id;
      Return_Type         : Node_Id := No_Node;
      Parent              : Node_Id := Current_Package;
      Renamed_Subprogram  : Node_Id := No_Node)
      return                Node_Id;

   function Make_Type_Attribute
     (Designator : Node_Id;
      Attribute  : Attribute_Id)
     return Node_Id;

   function Make_Used_Package
     (The_Used_Package : Node_Id)
     return Node_Id;

   function Make_Used_Type
     (The_Used_Type : Node_Id)
     return Node_Id;

   function Make_Variant_Part
     (Discriminant        : Node_Id;
      Variant_List        : List_Id)
     return                Node_Id;

   procedure Make_Comment_Header
     (Package_Header     : List_Id;
      Package_Identifier : Node_Id);
   --  This procedure generates a comment header for the generated
   --  packages. The comment text depends on the nature of the package

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id;
   --  This function executes Next_Node Num times

   function Qualified_Designator
     (P : Node_Id)
     return Node_Id;

   procedure Set_Homogeneous_Parent_Unit_Name
     (Child  : Node_Id;
      Parent : Node_Id);
   --  This procedure sets correctly the parent unit name of a node
   --  depending on its kind :
   --  * K_Defining_Identifier : the parent unit name is also a
   --    K_Defining_Identifier
   --  * K_Designator : The parent unit name is a K_Designator and the
   --    parent unit name of its defining identifier is also set up.

   procedure Set_Forwarded (E : Node_Id);
   function  Is_Forwarded  (E : Node_Id) return Boolean;
   --  The two subprograms above are used to permit the generation of
   --  additional code necessary for forwarded entities.

   --  The Set_XXXX_(Spec|Body) subrograms modifies the current Ada
   --  package

   procedure Set_CDR_Body (N : Node_Id := No_Node);
   procedure Set_CDR_Spec (N : Node_Id := No_Node);

   procedure Set_Aligned_Spec (N : Node_Id := No_Node);

   procedure Set_Buffers_Body (N : Node_Id := No_Node);
   procedure Set_Buffers_Spec (N : Node_Id := No_Node);

   procedure Set_Helper_Body (N : Node_Id := No_Node);
   procedure Set_Helper_Spec (N : Node_Id := No_Node);

   procedure Set_Init_Body (N : Node_Id := No_Node);
   procedure Set_Init_Spec (N : Node_Id := No_Node);

   procedure Set_Impl_Body (N : Node_Id := No_Node);
   procedure Set_Impl_Spec (N : Node_Id := No_Node);

   procedure Set_Main_Body (N : Node_Id := No_Node);
   procedure Set_Main_Spec (N : Node_Id := No_Node);

   procedure Set_Skeleton_Body (N : Node_Id := No_Node);
   procedure Set_Skeleton_Spec (N : Node_Id := No_Node);

   function To_Ada_Name (N : Name_Id) return Name_Id;
   --  Converts IDL name to Ada names. The IDL nama is converted
   --  according to the Ada mapping specification (if it conflicts
   --  with an Ada keyword, if it contains to consecutive dashes
   --  '_'...)

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
      GL_Dependencies);

   procedure Initialize_GList (P : Node_Id; L : GLists);
   --  Creates a new global list for the package declaration P and
   --  makes a binding between the list and P. If the list has been
   --  already initialized, this procedure does not do anything

   function Get_GList (P : Node_Id; L : GLists) return List_Id;
   --  Return the List_Id corresponding to the list L of the package
   --  declaration P. If the list has not been initialized, initilize
   --  it and return it

end Backend.BE_CORBA_Ada.Nutils;
