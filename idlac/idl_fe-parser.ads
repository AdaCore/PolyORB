------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        I D L _ F E . P A R S E R                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Idl_Fe.Lexer; use Idl_Fe.Lexer;
with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Errors;
with Ada.Unchecked_Deallocation;

package Idl_Fe.Parser is

   ---------------------
   --  Initialization --
   ---------------------

   procedure Initialize
     (Filename : in String;
      Preprocess : in Boolean;
      Keep_Temporary_Files : in Boolean);

   --------------------------
   --  Parsing of the idl  --
   --------------------------

   --  CORVA V2.3, 3.4
   --
   --  Rule 1 :
   --  <specification> ::= <definition>+
   function Parse_Specification return Node_Id;

private

   --------------------------------------
   --  Management of the token stream  --
   --------------------------------------

   --  This function returns the current token
   function Get_Token return Idl_Token;

   --  This procedure gets the next token from the lexer and put it
   --  into the token_buffer. It also gets its location and
   --  eventually its string representation and put them in the
   --  corresponding buffers
   procedure Get_Token_From_Lexer;

   --  This procedure consumes a token. If the token was already
   --  in the buffer, it just increases the index. Else, it gets
   --  the next token from the lexer.
   procedure Next_Token;

   --  Returns the previous token in the token stream.
   function View_Previous_Token return Idl_Token;

   --  Returns the previous token in the token stream.
   function View_Previous_Previous_Token return Idl_Token;

   --  Returns the next token in the token stream without consuming
   --  it. If necessary get it from the lexer and put it in the buffer
   function View_Next_Token return Idl_Token;

   --  Returns the next token in the token stream without consuming
   --  it. If necessary get it from the lexer and put it in the buffer
   function View_Next_Next_Token return Idl_Token;

   --  Returns the location of the current_token
   function Get_Token_Location return Idl_Fe.Errors.Location;

   --  Returns the location of the previous token
   function Get_Previous_Token_Location return Idl_Fe.Errors.Location;

   --  Returns the location of the previous token
   function Get_Previous_Previous_Token_Location return Idl_Fe.Errors.Location;

   --  Returns the location of the current_token
   function Get_Next_Token_Location return Idl_Fe.Errors.Location;


   --  The next three methods unreference a pointer without any
   --  verification. that's because the verification is useless
   --  in this case if this package is correctly written.
   --  Since these methods are not exported...

   --  Returns the location of the current_token
   function Get_Token_String return String;

   --  Returns the string of the previous token
   function Get_Previous_Token_String return String;

   --  Returns the string of the current_token
   function Get_Next_Token_String return String;

   --  Divides T_Greater_Greater in two T_Greater
   --  usefull for the parsing of sequences

   procedure Divide_T_Greater_Greater;

   ---------------------------------
   --  Management of expressions  --
   ---------------------------------

   --  a generic interval of values
   type Interval_Type is record
      Min, Max : Idl_Value;
   end record;

   --  a generic set of values, implemented as a list of intervals
   type Set;
   type Set_Ptr is access Set;
   type Set is record
      Interval : Interval_Type;
      Next : Set_Ptr;
   end record;

   --  to deallocate a set_ptr
   procedure Free is new Ada.Unchecked_Deallocation
     (Set, Set_Ptr);

--    --  try to add a value to the set of already used values.
--    --  if this value was already there, it return false, else true
--    function Add_Used_Value
--      (C : Node_Id)
--      return Boolean;

--    --  Frees all the set of already used values
--    procedure Release_All_Used_Values;


   --------------------------
   --  Parsing of the idl  --
   --------------------------

   --
   --  CORVA V2.3, 3.4
   --

   --  Rule 2
   --  <definition> ::= <type_dcl> ";"
   --               |   <const_dcl> ";"
   --               |   <except_dcl> ";"
   --               |   <interface> ";"
   --               |   <module> ";"
   --               |   <value> ";"
   procedure Parse_Definition (Result : out Node_Id;
                               Success : out Boolean);

   --  Rule 3
   --  <module> ::= "module" <identifier> "{" <definition>+ "}"
   procedure Parse_Module (Result : out Node_Id;
                           Success : out Boolean);

   --  Rule 4
   --  <interface> ::= <interface_dcl> | <forward_dcl>
   --
   --  Rule 5
   --  <interface_decl> ::= <interface_header> "{" <interface_body> "}"
   --
   --  Rule 6
   --  <forward_dcl> ::= ["abstract"] "interface" <identifier>
   --
   --  Rule 7
   --  <interface_header> ::= ["abstract"] "interface" <identifier>
   --                         [ <interface_inheritance_spec> ]
   --
   --  These rules are equivalent to
   --
   --  Rule Inter1
   --  <interface> ::= ["abstract"] "interface" <identifier>
   --                  <interface_end>
   --
   --  Rule Inter2
   --  <interface_end> ::= <forward_dcl_end>
   --                  |   <interface_dcl_end>
   --
   --  Rule Inter3
   --  <forward_dcl_end> ::=
   --
   --  Rule Inter4
   --  <interface_dcl_end> ::= [<interface_inheritance_spec>] "{"
   --                          <interface_body> "}"
   --  this last will be used in Parse_Interface_Dcl_End
   procedure Parse_Interface (Result : out Node_Id;
                              Success : out Boolean);

   --  Rule 8
   --  <interface_body> ::= <export>*
   procedure Parse_Interface_Body (List : in out Node_List;
                                   Success : out Boolean);

   --  Rule 9
   --  <export> ::= <type_dcl> ";"
   --           |   <const_dcl> ";"
   --           |   <except_dcl> ";"
   --           |   <attr_dcl> ";"
   --           |   <op_dcl> ";"
   procedure Parse_Export (Result : out Node_Id;
                           Success : out Boolean);

   --  <interface_dcl_end> ::= [<interface_inheritance_spec>] "{"
   --                          <interface_body> "}"
   --
   --  Rule 10
   --  <inheritance_spec> ::= ":" <interface_name> { "," <interface_name> }*
   procedure Parse_Interface_Dcl_End (Result : in out Node_Id;
                                      Success : out Boolean);

   --  Rule 11
   --  <interface_name> ::= <scoped_name>
   procedure Parse_Interface_Name (Result : out Node_Id;
                                   Success : out Boolean);

   --  Rule 12
   --  <scoped_name> ::= <identifier>
   --                | "::" <identifier>
   --                | <scoped_name> "::" <identifier>
   procedure Parse_Scoped_Name (Result : out Node_Id;
                                Success : out Boolean);

   --  Rule 13
   --  <value> ::= ( <value_dcl>
   --              | <value_abs_dcl>
   --              | <value_box_dcl>
   --              | <value_forward_dcl>) ";"

   --  Rule 14
   --  <value_forward_dcl> ::= [ "abstract" ] "valuetype" <identifier>

   --  Rule 15
   --  <value_box_dcl> ::= "valuetype"  <identifier> <type_spec>

   --  Rule 16
   --  <value_abs_dcl> ::= "abstract" "valuetype" <identifier>
   --                      [ <value_inheritance_spec> ] "{" <export>* "}"

   --  Rule 17
   --  <value_dcl> ::= <value_header> "{"  < value_element>* "}"

   --  Rule 18
   --  <value_header> ::= ["custom" ] "valuetype" <identifier>
   --                     [ <value_inheritance_spec> ]

   --  These Rules (13 to 18) are equivalent to the following :

   --  Rule Value1
   --  <value> ::= ( "custom" <custom_value>
   --              | "abstract" <abstract_value>
   --              | <direct_value> ) ";"

   --  Rule Value2
   --  <custom_value> ::= "valuetype" <end_value_dcl>

   --  Rule Value3
   --  <abstract_value> ::= "valuetype" ( <end_value_abs_dcl>
   --                                              | <end_value_forward_dcl> )

   --  Rule Value4
   --  <direct_value> ::= "valuetype" ( <end_value_box_dcl>
   --                                 | <end_value_forward_dcl>
   --                                 | <end_value_dcl> )

   --  Rule Value5
   --  <end_value_dcl> ::= <identifier> [ <value_inheritance_spec> ]
   --                      "{"  < value_element>* "}"

   --  Rule Value6
   --  <end_value_abs_dcl> ::= <identifier> [ <value_inheritance_spec> ]
   --                          "{" <export>* "}"

   --  Rule Value7
   --  <end_value_forward_dcl> ::= <identifier>

   --  Rule Value8
   --  <end_value_box_dcl> ::= <identifier> <type_spec>

   --  Rule Value1
   --  <value> ::= ( "custom" <custom_value>
   --              | "abstract" <abstract_value>
   --              | <direct_value> ) ";"
   procedure Parse_Value (Result : out Node_Id;
                          Success : out Boolean);

   --  Rule Value2
   --  <custom_value> ::= "valuetype" <end_value_dcl>
   procedure Parse_Custom_Value (Result : out Node_Id;
                                 Success : out Boolean);

   --  Rule Value3
   --  <abstract_value> ::= "valuetype" ( <end_value_abs_dcl>
   --                                   | <end_value_forward_dcl> )
   procedure Parse_Abstract_Value (Result : out Node_Id;
                                   Success : out Boolean);

   --  Rule Value4
   --  <direct_value> ::= "valuetype" ( <end_value_box_dcl>
   --                                 | <end_value_forward_dcl>
   --                                 | <end_value_dcl> )
   procedure Parse_Direct_Value (Result : out Node_Id;
                                 Success : out Boolean);

   --  Since rule 5 and 6 are very close, there is only one method
   --  Rule Value5
   --  <end_value_dcl> ::= <identifier> [ <value_inheritance_spec> ]
   --                      "{"  < value_element>* "}"

   --  Rule Value6
   --  <end_value_abs_dcl> ::= <identifier> [ <value_inheritance_spec> ]
   --                          "{" <export>* "}"
   procedure Parse_End_Value_Dcl (Result : out Node_Id;
                                  Success : out Boolean;
                                  Custom : in Boolean;
                                  Abst : in Boolean);

   --  Rule Value7
   --  <end_value_forward_dcl> ::= <identifier>
   procedure Parse_End_Value_Forward_Dcl (Result : out Node_Id;
                                          Success : out Boolean;
                                          Abst : in Boolean);

   --  Rule Value8
   --  <end_value_box_dcl> ::= <identifier> <type_spec>
   procedure Parse_End_Value_Box_Dcl (Result : out Node_Id;
                                      Success : out Boolean);

   --  Rule 19
   --  <value_inheritance_spec> ::= [ ":" [ "truncatable" ]
   --                               <value_name> { "," <value_name> }* ]
   --                               [ "supports" <interface_name>
   --                               { "," <interface_name> }* ]
   procedure Parse_Value_Inheritance_Spec (Result : in out Node_Id;
                                           Success : out Boolean);

   --  Rule 20
   --  <value_name> ::= <scoped_name>
   procedure Parse_Value_Name (Result : out Node_Id;
                               Success : out Boolean);

   --  Rule 21
   --  <value_element> ::= <export> | < state_member> | <init_dcl>
   procedure Parse_Value_Element  (Result : out Node_Id;
                                   Success : out Boolean);

   --  Rule 22
   --  <state_member> ::= ( "public" | "private" )
   --                     <type_spec> <declarators> ";"
   procedure Parse_State_Member (Result : out Node_Id;
                                 Success : out Boolean);

   --  Rule 23
   --  <init_dcl> ::= "factory" <identifier> "("
   --                 [ <init_param_decls> ] ")" ";"
   procedure Parse_Init_Dcl (Result : out Node_Id;
                             Success : out Boolean);

   --  Rule 24
   --  <init_param_decls> ::= <init_param_decl> { "," <init_param_decl> }
   procedure Parse_Init_Param_Decls (Result : out Node_List;
                                     Success : out Boolean);

   --  Rule 25
   --  <init_param_decl> ::= <init_param_attribute> <param_type_spec>
   --                        <simple_declarator>
   --  Rule 26
   --  <init_param_attribute> ::= "in"
   procedure Parse_Init_Param_Decl (Result : out Node_Id;
                                    Success : out Boolean);

   --  Rule 27
   --  <const_dcl> ::= "const" <const_type> <identifier> "=" <const_exp>
   procedure Parse_Const_Dcl (Result : out Node_Id;
                              Success : out Boolean);

   --  Rule 28
   --  <const_type> ::= <integer_type>
   --               |   <char_type>
   --               |   <wide_char_type>
   --               |   <boolean_type>
   --               |   <floating_pt_type>
   --               |   <string_type>
   --               |   <wide_string_type>
   --               |   <fixed_pt_const_type>
   --               |   <scoped_name>
   --               |   <octet_type>
   procedure Parse_Const_Type (Result : out Node_Id;
                               Success : out Boolean);

   --  Rule 29
   --  <const_exp> ::= <or_expr>
   procedure Parse_Const_Exp (Result : out Node_Id;
                              Constant_Type : in Node_Id;
                              Success : out Boolean);

   --  Rule 30
   --  <or_expr> ::= <xor_expr>
   --            |   <or_expr> "^" <xor_expr>
   --  actually, the implemented gramar is slightly different :
   --  <or_expr> ::= <xor_expr>
   --            |   <xor_expr> "^" <or_expr>
   procedure Parse_Or_Expr (Result : out Node_Id;
                            Success : out Boolean;
                            Expr_Type : in Const_Type_Ptr);

   --  Rule 31
   --  <xor_expr> ::= <and_expr>
   --             |   <xor_expr> "^" <and_expr>
   --  actually, the implemented gramar is slightly different :
   --  <xor_expr> ::= <and_expr>
   --             |   <and_expr> "^" <xor_expr>
   procedure Parse_Xor_Expr (Result : out Node_Id;
                             Success : out Boolean;
                             Expr_Type : in Const_Type_Ptr);

   --  Rule 32
   --  <and_expr> ::= <shift_expr>
   --             |   <and_expr> "&" <shift_expr>
   --  actually, the implemented gramar is slightly different :
   --  <and_expr> ::= <shift_expr>
   --             |   <shift_expr> "&" <and_expr>
   procedure Parse_And_Expr (Result : out Node_Id;
                             Success : out Boolean;
                             Expr_Type : in Const_Type_Ptr);

   --  Rule 33
   --  <shift_expr> ::= <add_expr>
   --               |   <shift_expr> ">>" <add_expr>
   --               |   <shift_expr> "<<" <add_expr>
   --  actually, the implemented gramar is slightly different :
   --  <shift_expr> ::= <add_expr>
   --               |   <add_expr> ">>" <shift_expr>
   --               |   <add_expr> "<<" <shift_expr>
   procedure Parse_Shift_Expr (Result : out Node_Id;
                               Success : out Boolean;
                               Expr_Type : in Const_Type_Ptr);

   --  Rule 34
   --  <add_expr> ::= <mult_expr>
   --             |   <add_expr> "+" <mult_expr>
   --             |   <add_expr> "-" <mult_expr>
   --  actually, the implemented gramar is slightly different :
   --  <Add_expr> ::= <mult_expr>
   --             |   <mult_expr> ">>" <add_expr>
   --             |   <mult_expr> "<<" <add_expr>
   procedure Parse_Add_Expr (Result : out Node_Id;
                             Success : out Boolean;
                             Expr_Type : in Const_Type_Ptr);

   --  Rule 35
   --  <mult_expr> ::= <unary_expr>
   --              |   <mult_expr> "*" <unary_expr>
   --              |   <mult_expr> "/" <unary_expr>
   --              |   <mult_expr> "%" <unary_expr>
   --  actually, the implemented gramar is slightly different :
   --  <mult_expr> ::= <unary_expr>
   --              |   <unary_expr> "*" <mult_expr>
   --              |   <unary_expr> "/" <mult_expr>
   --              |   <unary_expr> "%" <mult_expr>
   procedure Parse_Mult_Expr (Result : out Node_Id;
                              Success : out Boolean;
                              Expr_Type : in Const_Type_Ptr);

   --  Rule 36
   --  <unary_expr> ::= <unary_operator> <primary_expr>
   --               |   <primary_expr>
   --  Rule 37
   --  <unary_operator> ::= "+" | "-" | "~"
   procedure Parse_Unary_Expr (Result : out Node_Id;
                               Success : out Boolean;
                               Expr_Type : in Const_Type_Ptr);

   --  Rule 38
   --  <primary_expr> ::= <scoped_name>
   --                 |   <literal>
   --                 |   "(" <const_expr> ")"
   procedure Parse_Primary_Expr (Result : out Node_Id;
                                 Success : out Boolean;
                                 Expr_Type : in Const_Type_Ptr);

   --  Rule 39
   --  <literal> ::= <integer_literal>
   --            | <string_literal>
   --            | <character_literal>
   --            | <floating_pt_literal>
   --            | <boolean_literal>
   --  Actually, the specification is sort of inconsistant here
   --  since we can not use wide strings, wide chars or fixed point
   --  numbers. So the implemented rule is in fact :
   --  <literal> ::= <integer_literal>
   --            | <string_literal>
   --            | <wide_string_literal>
   --            | <character_literal>
   --            | <wide_character_literal>
   --            | <floating_pt_literal>
   --            | <fixed_pt_literal>
   --            | <boolean_literal>
   procedure Parse_Literal (Result : out Node_Id;
                            Success : out Boolean);

   --  Rule 40
   --  <boolean_literal> ::= "TRUE"
   --                    | "FALSE"
   procedure Parse_Boolean_Literal (Result : out Node_Id;
                                    Success : out Boolean);

   --  Rule 41
   --  <positive_int_const> ::= <const_exp>
   procedure Parse_Positive_Int_Const (Result : out Node_Id;
                                       Success : out Boolean);

   --  Rule 42
   --  <type_dcl> ::= "typedef" <type_declarator>
   --             |   <struct_type>
   --             |   <union_type>
   --             |   <enum_type>
   --             |   "native" <simple_declarator>
   procedure Parse_Type_Dcl (Result : out Node_Id;
                             Success : out Boolean);

   --  Rule 43
   --  <type_declarator> ::= <type_spec> <declarators>
   procedure Parse_Type_Declarator (Result : out Node_Id;
                                    Success : out Boolean);

   --  Rule 44
   --  <type_spec> ::= <simple_type_spec>
   --              |   <constr_type_spec>
   procedure Parse_Type_Spec (Result : out Node_Id;
                              Success : out Boolean);

   --  Rule 45
   --  <simple_type_spec> ::= <base_type_spec>
   --                     |   <template_type_spec>
   --                     |   <scoped_name>
   procedure Parse_Simple_Type_Spec (Result : out Node_Id;
                                     Success : out Boolean);

   --  Rule 46
   --  <base_type_spec> ::= <floating_pt_type>
   --                   |   <integer_type>
   --                   |   <char_type>
   --                   |   <wide_char_type>
   --                   |   <boolean_type>
   --                   |   <octet_type>
   --                   |   <any_type>
   --                   |   <object_type>
   procedure Parse_Base_Type_Spec (Result : out Node_Id;
                                   Success : out Boolean);

   --  Rule 47
   --  <template_type_spec> ::= <sequence_type>
   --                       |   <string_type>
   --                       |   <wide_string_type>
   --                       |   <fixed_pt_type>
   procedure Parse_Template_Type_Spec (Result : out Node_Id;
                                       Success : out Boolean);

   --  Rule 48
   --  <constr_type_spec> ::= <struct_type>
   --                     |   <union_type>
   --                     |   <enum_type>
   procedure Parse_Constr_Type_Spec (Result : out Node_Id;
                                     Success : out Boolean);

   --  Rule 49
   --  <declarators> ::= <declarator> { "," <declarator> }*
   procedure Parse_Declarators (Result : out Node_List;
                                Parent : in Node_Id;
                                Success : out Boolean);

   --  Rule 50
   --  <declarator> ::= <simple_declarator>
   --               |   <complex_declarator>
   procedure Parse_Declarator (Result : out Node_Id;
                               Parent : in Node_Id;
                               Success : out Boolean);

   --  Rule 51
   --  <simple_declarator> ::= <identifier>
   procedure Parse_Simple_Declarator (Result : out Node_Id;
                                      Parent : in Node_Id;
                                      Success : out Boolean);

   --  Rule 52
   --  <complex_declarator> ::= <array_declarator>
   procedure Parse_Complex_Declarator (Result : out Node_Id;
                                       Parent : in Node_Id;
                                       Success : out Boolean);

   --  Rule 53
   --  <floating_pt_type> ::= "float"
   --                     |   "double"
   --                     |   "long" "double"
   procedure Parse_Floating_Pt_Type (Result : in out Node_Id;
                                     Success : out Boolean);

   --  Rule 54
   --  <integer_type> ::= <signed_int>
   --                 |   <unsigned_int>
   procedure Parse_Integer_Type (Result : in out Node_Id;
                                 Success : out Boolean);

   --  Rule 55
   --  <signed_int> ::= <signed_short_int>
   --               |   <signed_long_int>
   --               |   <signed_longlong_int>
   procedure Parse_Signed_Int (Result : in out Node_Id;
                               Success : out Boolean);

   --  Rule 56
   --  <signed_short_int> ::= "short"
   procedure Parse_Signed_Short_Int (Result : in out Node_Id;
                                     Success : out Boolean);

   --  Rule 57
   --  <signed_long_int> := "long"
   procedure Parse_Signed_Long_Int (Result : in out Node_Id;
                                    Success : out Boolean);

   --  Rule 58
   --  <signed_longlong_int> ::= "long" "long"
   procedure Parse_Signed_Longlong_Int (Result : in out Node_Id;
                                        Success : out Boolean);

   --  Rule 59
   --  <unsigned_int> ::= <unsigned_short_int>
   --                 |   <unsigned_long_int>
   --                 |   <unsigned_longlong_int>
   procedure Parse_Unsigned_Int (Result : in out Node_Id;
                                 Success : out Boolean);

   --  Rule 60
   --  <unsigned_short_int> ::= "unsigned" "short"
   procedure Parse_Unsigned_Short_Int (Result : in out Node_Id;
                                       Success : out Boolean);

   --  Rule 61
   --  <unsigned_long_int> ::= "unsigned" "long"
   procedure Parse_Unsigned_Long_Int (Result : in out Node_Id;
                                      Success : out Boolean);

   --  Rule 62
   --  <unsigned_longlong_int> ::= "unsigned" "long" "long"
   procedure Parse_Unsigned_Longlong_Int (Result : in out Node_Id;
                                          Success : out Boolean);

   --  Rule 63
   --  <char_type> ::= "char"
   procedure Parse_Char_Type (Result : in out Node_Id;
                              Success : out Boolean);

   --  Rule 64
   --  <wide_char_type> ::= "wchar"
   procedure Parse_Wide_Char_Type (Result : in out Node_Id;
                                   Success : out Boolean);

   --  Rule 65
   --  <boolean_type> ::= "boolean"
   procedure Parse_Boolean_Type (Result : in out Node_Id;
                                 Success : out Boolean);

   --  Rule 66
   --  <octet_type> ::= "octet"
   procedure Parse_Octet_Type (Result : in out Node_Id;
                               Success : out Boolean);

   --  Rule 67
   --  <any_type> ::= "any"
   procedure Parse_Any_Type (Result : in out Node_Id;
                             Success : out Boolean);

   --  Rule 68
   --  <object_type> ::= "object"
   procedure Parse_Object_Type (Result : in out Node_Id;
                                Success : out Boolean);

   --  Rule 69
   --  <struct_type> ::= "struct" <identifier> "{" <member_list> "}"
   procedure Parse_Struct_Type (Result : out Node_Id;
                                Success : out Boolean);

   --  Rule 70
   --  <member_list> ::= <member>+
   procedure Parse_Member_List (Result : out Node_List;
                                Success : out Boolean);

   --  Rule 71
   --  <member> ::= <type_spec> <declarators> ";"
   procedure Parse_Member (Result : out Node_Id;
                           Success : out Boolean);

   --  Rule 72
   --  <union_type> ::= "union" <identifier>
   --                   "switch" "(" <switch_type_spec> ")"
   --                   "{" <switch_body> "}"
   procedure Parse_Union_Type (Result : out Node_Id;
                               Success : out Boolean);

   --  Rule 73
   --  <switch_type_spec> ::= <integer_type>
   --                     |   <char_type>
   --                     |   <boolean_type>
   --                     |   <enum_type>
   --                     |   <scoped_name>
   procedure Parse_Switch_Type_Spec (Result : out Node_Id;
                                     Success : out Boolean);

   --  Rule 74
   --  <switch_body> ::= <case>+
   procedure Parse_Switch_Body (Result : out Node_List;
                                Switch_Type : in Node_Id;
                                Success : out Boolean);

   --  Rule 75
   --  <case> ::= <case_label>+ <element_spec> ";"
   procedure Parse_Case (Result : out Node_Id;
                         Switch_Type : in Node_Id;
                         Success : out Boolean);

   --  Rule 76
   --  <case_label> ::= "case" <const_exp> ":"
   --                 | "default ":"
   procedure Parse_Case_Label (Result : out Node_Id;
                               Switch_Type : in Node_Id;
                               Success : out Boolean);

   --  Rule 77
   --  <element_spec> ::= <type_spec> <declarator>
   procedure Parse_Element_Spec (Element_Type : out Node_Id;
                                 Element_Decl : out Node_Id;
                                 Parent : in Node_Id;
                                 Success : out Boolean);

   --  Rule 78
   --  <enum_type> ::= "enum" <identifier> "{" <enumerator>
   --                  { "," <enumerator> }* "}"
   procedure Parse_Enum_Type (Result : out Node_Id;
                              Success : out Boolean);

   --  Rule 79
   --  <enumerator> ::= <identifier>
   procedure Parse_Enumerator (Result : out Node_Id;
                               Success : out Boolean);

   --  Rule 80
   --  <sequence_type> ::= "sequence" "<" <simple_type_spec>
   --                      "," <positive_int_const> ">"
   --                  |   "sequence" "<" <simple_type_spec> ">"
   procedure Parse_Sequence_Type (Result : out Node_Id;
                                  Success : out Boolean);
   --  Rule 81
   --  <string_type> ::= "string" "<" <positive_int_const> ">"
   --                |   "string"
   procedure Parse_String_Type (Result : out Node_Id;
                                Success : out Boolean);

   --  Rule 82
   --  <wide_string_type> ::= "wstring" "<" <positive_int_const> ">"
   --                     |   "wstring"
   procedure Parse_Wide_String_Type (Result : out Node_Id;
                                     Success : out Boolean);

   --  Rule 83
   --  <array_declarator> ::= <identifier> <fixed_array_size>+
   procedure Parse_Array_Declarator (Result : out Node_Id;
                                     Parent : in Node_Id;
                                     Success : out Boolean);

   --  Rule 84
   --  <fixed_array_size> ::= "[" <positive_int_const> "]"
   procedure Parse_Fixed_Array_Size (Result : out Node_Id;
                                     Success : out Boolean);


   --  Rule 85:
   --  <attr_dcl> ::= [ "readonly" ] "attribute" <param_type_spec>
   --                 <simple_declarator> { "," <simple_declarator> }*
   procedure Parse_Attr_Dcl (Result : out Node_Id;
                             Success : out Boolean);

   --  Rule 86
   --  <except_dcl> ::= "exception" <identifier> "{" <member>* "}"
   procedure Parse_Except_Dcl (Result : out Node_Id;
                               Success : out Boolean);

   --  Rule 87
   --  <op_dcl> ::= [ <op_attribute> ] <op_type_spec> <identifier>
   --               <parameters_dcls> [ <raises_expr> ]
   --               [ <context_expr> ]
   procedure Parse_Op_Dcl (Result : out Node_Id;
                           Success : out Boolean);

   --  Rule 88
   --  <op_attribute> ::= "oneway"
   --  no parsing mathod needed here

   --  Rule 89
   --  <op_type_spec> ::= <param_type_spec>
   --                 |   "void"
   procedure Parse_Op_Type_Spec (Result : out Node_Id;
                                 Success : out Boolean);

   --  Rule 90
   --  <parameter_dcls> ::= "(" <param_dcl> { "," <param_dcl> }* ")"
   --                   |   "(" ")"
   procedure Parse_Parameter_Dcls (Result : out Node_List;
                                   Success : out Boolean);

   --  Rule 91
   --  <param_dcl> ::= <param_attribute> <param_type_spec> <simple_declarator>
   procedure Parse_Param_Dcl (Result : out Node_Id;
                              Success : out boolean);

   --  Rule 92
   --  <param_attribute> ::= "in"
   --                    |   "out"
   --                    |   "inout"
   procedure Parse_Param_Attribute (Result : out Param_Mode;
                                    Success : out Boolean);

   --  Rule 93
   --  <raises_expr> ::= "raises" "(" <scoped_name> { ","
   --                                 <scoped_name" }* ")"
   procedure Parse_Raises_Expr (Result : out Node_List;
                                Success : out Boolean);

   --  Rule 94
   --  <context_expr> ::= "context" "(" <string_literal> { ","
   --                                   <string_literal> }* ")"
   procedure Parse_Context_Expr (Result : out Node_List;
                                 Success : out Boolean);

   --  Rule 95
   --  <param_type_spec> ::= <base_type_spec>
   --                    |   <string_type>
   --                    |   <wide_string_type>
   --                    |   <scoped_name>
   procedure Parse_Param_Type_Spec (Result : out Node_Id;
                                    Success : out Boolean);

   --  Rule 96
   --  <fixed_pt_type>  ::= "fixed" "<" <positive_int_const> ","
   --                       <positive_int_const> ">"
   procedure Parse_Fixed_Pt_Type (Result : out Node_Id;
                                  Success : out Boolean);

   --  Rule 98
   --  <value_base_type> ::= "ValueBase"
   procedure Parse_Value_Base_Type (Result : in out Node_Id;
                                    Success : out Boolean);

   ------------------------------
   --  Inheritance management  --
   ------------------------------

   --  verifying that an interface can be imported :
   --     Int in a scoped name denoting the interface to be imported
   --     Scope is an interface where the other will be imported
   --  This method verifies that there is no operation or
   --  attributes in the new imported interface that clashes
   --  with the already imported ones.
   function Interface_Is_Importable (Int : in Node_Id;
                                     Scope : in Node_Id)
                                     return Boolean;

   --------------------------
   --  Parsing of pragmas  --
   --------------------------

   --  parsing pragmas
   procedure Parse_Pragma (Result : out Node_Id;
                           Success : out Boolean);

   ---------------------------
   --  Parsing of literals  --
   ---------------------------

   --  parsing of an integer
   procedure Parse_Integer_Literal (Result : out Node_Id;
                                    Success : out Boolean);

   --  parsing of a string
   procedure Parse_String_Literal (Result : out Node_Id;
                                   Success : out Boolean);

   --  parsing of a wide string
   procedure Parse_Wide_String_Literal (Result : out Node_Id;
                                        Success : out Boolean);

   --  parsing of a char
   procedure Parse_Char_Literal (Result : out Node_Id;
                                 Success : out Boolean);

   --  parsing of a wide char
   procedure Parse_Wide_Char_Literal (Result : out Node_Id;
                                      Success : out Boolean);

   --  parsing of a float
   procedure Parse_Floating_Pt_Literal (Result : out Node_Id;
                                        Success : out Boolean);

   --  parsing of a fixed point number
   procedure Parse_Fixed_Pt_Literal (Result : out Node_Id;
                                     Success : out Boolean);

   --  CORBA V2.3 - 3.12.4
   --
   --  "Each String_literal is an arbitrary long sequence of
   --  alphabetic, digit, period ("."), underscore ("_") and
   --  asterisk ("*") characters. The first character of the string
   --  must be an alphabetic character. An asterisk may only be
   --  used at the last character of the string. "
   --
   --  This procedure raises an error if S does not respect these
   --  constraints.
   procedure Check_Context_String (S : in String);


   ---------------------------------
   --  evaluation of expressions  --
   ---------------------------------

--    --  CORBA V2.3 - 3.9.2
--    --
--    --  "An infix operator can combine two integer, floats
--    --  or fixed but not mixtures of these."
--    --  "Infix operator are applicable only to integer, float
--    --  and fixed types."
--    --
--    --  all the functions in this section raise an error if the types
--    --  of each element are not compatible. It then computes the type
--    --  of the result. In case of incompatibility, the type is
--    --  C_No_Type
--    --
--    --  Beside that, when the type is ok, these functions compute
--    --  the value of the expression.

--    --  Or expression evaluation
--    procedure Eval_Or_Expr (Left : in Value_Ptr;
--                            Right : in Value_Ptr;
--                            Result : out Value_Ptr;
--                            Loc : in Idl_Fe.Errors.Location);

--    --  Xor expression evaluation
--    procedure Eval_Xor_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  And expression evaluation
--    procedure Eval_And_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Right shift expression evaluation
--    procedure Eval_Shr_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Left shift expression evaluation
--    procedure Eval_Shl_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Add expression evaluation
--    procedure Eval_Add_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Sub expression evaluation
--    procedure Eval_Sub_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Mul expression evaluation
--    procedure Eval_Mul_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Div expression evaluation
--    procedure Eval_Div_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Mod expression evaluation
--    procedure Eval_Mod_Expr (Left : in Value_Ptr;
--                             Right : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Neg expression evaluation
--    procedure Eval_Neg_Expr (Operand : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  Not expression evaluation
--    procedure Eval_Not_Expr (Operand : in Value_Ptr;
--                             Result : out Value_Ptr;
--                             Loc : in Idl_Fe.Errors.Location);

--    --  actual or functions for idl types
--    function "or" (X, Y : Idl_Short) return Idl_Short;
--    function "or" (X, Y : Idl_Long) return Idl_Long;
--    function "or" (X, Y : Idl_LongLong) return Idl_LongLong;
--    function "or" (X, Y : Idl_UShort) return Idl_UShort;
--    function "or" (X, Y : Idl_ULong) return Idl_ULong;
--    function "or" (X, Y : Idl_ULongLong) return Idl_ULongLong;

   function "or" (X, Y : Idl_Value) return Idl_Value;

   ------------------------------
   --  To resume after errors  --
   ------------------------------

   --  This methods are called when the parser encounters an error
   --  in order to try to continue the parsing

   --  Goes to the beginning of the next definition.
   procedure Go_To_Next_Definition;

   --  Goes to the next Cbracket opening.
   procedure Go_To_Next_Left_Cbracket;

   --  Goes to the next Cbracket opening.
   procedure Go_To_Next_Right_Cbracket;

   --  Goes to the next export (see rule 9)
   procedure Go_To_Next_Export;

   --  Goes to the next value_element (see rule 21)
   procedure Go_To_Next_Value_Element;

   --  Goes to the end of a state member declaration (see rule 22)
   procedure Go_To_End_Of_State_Member;

   --  Goes to the next right parenthesis.
   procedure Go_To_Next_Right_Paren;

   --  Goes to the next member (see rule 71)
   procedure Go_To_Next_Member;

   --  Goes to the end of a case clause in an union (see rule 74)
   procedure Go_To_End_Of_Case;

   --  Goes to the end of a case label in an union (see rule 75)
   procedure Go_To_End_Of_Case_Label;

   --  Goes to the end of a scoped name (see rule 12)
   procedure Go_To_End_Of_Scoped_Name;

   --  Goes to the next T_End_Pragma token and consumes it
   procedure Go_To_End_Of_Pragma;

end Idl_Fe.Parser;
