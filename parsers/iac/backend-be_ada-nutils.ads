with Types;     use Types;

with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;

package Backend.BE_Ada.Nutils is

   Int0_Val : Value_Id;

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
      Tok_Dot_Dot);        -- ..

   Token_Image : array (Token_Type) of Name_Id;
   subtype Keyword_Type is Token_Type
     range Tok_Mod .. Tok_Separate;

   type Parameter_Id is
     (P_Arg_Modes,
      P_Argument,
      P_From,
      P_Name,
      P_Result,
      P_Returns,
      P_Self,
      P_To);

   PN : array (Parameter_Id) of Name_Id;

   type Variable_Id is
     (V_Argument,
      V_Argument_List,
      V_Argument_Name,
      V_Context,
      V_Def_Sys_Member,
      V_Exception_List,
      V_Handler,
      V_Impl_Object_Ptr,
      V_Members,
      V_Name,
      V_Operation_Name,
      V_Request,
      V_Result,
      V_Result_Name,
      V_Returns,
      V_Self_Ref,
      V_Send_Request_Result,
      V_Value_Operation);

   VN : array (Variable_Id) of Name_Id;

   type Subprogram_Id is
     (S_Get_Members);

   SN : array (Subprogram_Id) of Name_Id;

   type Component_Id is
     (C_Switch);

   CN : array (Component_Id) of Name_Id;

   type Attribute_Id is
     (A_First);

   AN : array (Attribute_Id) of Name_Id;

   type Type_Id is
     (T_Ref);

   TN : array (Type_Id) of Name_Id;

   procedure Add_With_Package
     (P : Node_Id);

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);

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
     (Designator : Node_Id)
     return Node_Id;

   function Make_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id)
     return Node_Id;

   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id)
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

   function Make_Defining_Identifier
     (Name : Name_Id)
     return  Node_Id;

   function Make_Derived_Type_Definition
     (Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id;
      Is_Abstract_Type      : Boolean := False)
     return Node_Id;

   function Make_Enumeration_Type_Definition
     (Enumeration_Literals : List_Id)
     return Node_Id;

   function Make_Exception_Declaration
     (Defining_Identifier : Node_Id)
     return Node_Id;

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id;
      Discriminant_Spec   : Node_Id := No_Node)
     return Node_Id;

   function Make_If_Statement
     (Condition : Node_Id;
      Then_Statements : List_Id;
      Else_Statements : List_Id)
     return Node_Id;

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node)
     return List_Id;

   function Make_Literal
     (Value : Value_Id)
     return Node_Id;

   function Make_Object_Declaration
     (Defining_Identifier : Node_Id;
      Constant_Present    : Boolean;
      Object_Definition   : Node_Id;
      Expression          : Node_Id)
      return                Node_Id;

   function Make_Package_Declaration
     (Identifier : Node_Id)
     return Node_Id;

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := Mode_In)
      return                Node_Id;

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
      Return_Type         : Node_Id := No_Node)
      return                Node_Id;

   function Make_Type_Attribute
     (Designator : Node_Id;
      Attribute  : Attribute_Id)
     return Node_Id;

   function Make_Variant_Part
     (Discriminant        : Node_Id;
      Variant_List        : List_Id)
      return                Node_Id;

   function Qualified_Designator
     (P : Node_Id)
     return Node_Id;

   procedure Set_Helper_Body (N : Node_Id := No_Node);
   procedure Set_Helper_Spec (N : Node_Id := No_Node);

   procedure Set_Impl_Body (N : Node_Id := No_Node);
   procedure Set_Impl_Spec (N : Node_Id := No_Node);

   procedure Set_Main_Body (N : Node_Id := No_Node);
   procedure Set_Main_Spec (N : Node_Id := No_Node);

   function To_Ada_Name (N : Name_Id) return Name_Id;

end Backend.BE_Ada.Nutils;
