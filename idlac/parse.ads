--  idlac: IDL to Ada compiler.
--  Copyright (C) 1999 Tristan Gingold.
--
--  emails: gingold@enst.fr
--          adabroker@adabroker.eu.org
--
--  IDLAC is free software;  you can  redistribute it and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Software
--  Foundation;  either version 2,  or (at your option) any later version.
--  IDLAC is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for  more details.  You should have  received  a copy of the GNU General
--  Public License  distributed with IDLAC;  see file COPYING.  If not, write
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston,
--  MA 02111-1307, USA.
--

with Tree; use Tree;
with Tokens; use Tokens;
with Types; use Types;
with Errors;

package Parse is

   ---------------------
   --  Initialization --
   ---------------------

   procedure Initialize (Filename : in String;
                         Preprocess : in Boolean;
                         Keep_Temporary_Files : in Boolean);


   --------------------------
   --  Parsing of the idl  --
   --------------------------

   --  CORVA V2.3, 3.4
   --
   --  Rule 1 :
   --  <specification> ::= <definition>+
   function Parse_Specification return N_Repository_Acc;


private

   --------------------------------------
   --  management of the token stream  --
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

   --  Returns the next token in the token stream without consuming
   --  it. If necessary get it from the lexer and put it in the buffer
   function View_Next_Token return Idl_Token;

   --  Returns the location of the current_token
   function Get_Token_Location return Errors.Location;

   --  Returns the location of the previous token
   function Get_Previous_Token_Location return Errors.Location;

   --  Returns the location of the previous token
   function Get_Previous_Previous_Token_Location return Errors.Location;

   --  Returns the location of the current_token
   function Get_Next_Token_Location return Errors.Location;


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


   --------------------------
   --  Parsing of the idl  --
   --------------------------

   --
   --  CORVA V2.3, 3.4
   --

   --  Rule 2:
   --  <definition> ::= <type_dcl> ";"
   --               |   <const_dcl> ";"
   --               |   <except_dcl> ";"
   --               |   <interface> ";"
   --               |   <module> ";"
   --               |   <value> ";"
   procedure Parse_Definition (Result : out N_Root_Acc;
                               Success : out Boolean);

   --  Rule 3:
   --  <module> ::= "module" <identifier> "{" <definition>+ "}"
   procedure Parse_Module (Result : out N_Module_Acc;
                           Success : out Boolean);

   --    --  Rule 9:
   --    --  <export> ::= <type_dcl> ";"
   --    --           |   <const_dcl> ";"
   --    --           |   <except_dcl> ";"
   --    --           |   <attr_dcl> ";"
   --    --           |   <op_dcl> ";"
   procedure Parse_Export (List : in out Node_List;
                           Success : out Boolean);

   --    --  Rule 11:
   --    --  <scoped_name> ::= <identifier>
   --    --                    | "::" <identifier>
   --    --                    | <scoped_name> "::" <identifier>
   procedure Parse_Scoped_Name (Result : out N_Scoped_Name_Acc;
                                Success : out Boolean);

   --  Rule 13:
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
   procedure Parse_Value (Result : out N_Named_Acc;
                          Success : out Boolean);

   --  Rule Value2
   --  <custom_value> ::= "valuetype" <end_value_dcl>
   procedure Parse_Custom_Value (Result : out N_ValueType_Acc;
                                 Success : out Boolean);

   --  Rule Value3
   --  <abstract_value> ::= "valuetype" ( <end_value_abs_dcl>
   --                                   | <end_value_forward_dcl> )
   procedure Parse_Abstract_Value (Result : out N_Named_Acc;
                                   Success : out Boolean);

   --  Rule Value4
   --  <direct_value> ::= "valuetype" ( <end_value_box_dcl>
   --                                 | <end_value_forward_dcl>
   --                                 | <end_value_dcl> )
   procedure Parse_Direct_Value (Result : out N_Named_Acc;
                                 Success : out Boolean);

   --  Since rule 5 and 6 are very close, there is only one method
   --  Rule Value5
   --  <end_value_dcl> ::= <identifier> [ <value_inheritance_spec> ]
   --                      "{"  < value_element>* "}"

   --  Rule Value6
   --  <end_value_abs_dcl> ::= <identifier> [ <value_inheritance_spec> ]
   --                          "{" <export>* "}"
   procedure Parse_End_Value_Dcl (Result : out N_ValueType_Acc;
                                  Success : out Boolean;
                                  Custom : in Boolean;
                                  Abst : in boolean);

   --  Rule Value7
   --  <end_value_forward_dcl> ::= <identifier>
   procedure Parse_End_Value_Forward_Dcl (Result : out N_Forward_ValueType_Acc;
                                          Success : out Boolean;
                                          Abst : in Boolean);

   --  Rule Value8
   --  <end_value_box_dcl> ::= <identifier> <type_spec>
   procedure Parse_End_Value_Box_Dcl (Result : out N_Boxed_ValueType_Acc;
                                      Success : out Boolean);

   --  Rule 19
   --  <value_inheritance_spec> ::= [ ":" [ "truncatable" ]
   --                               <value_name> { "," <value_name> }* ]
   --                               [ "supports" <interface_name>
   --                               { "," <interface_name> }* ]
   procedure Parse_Value_Inheritance_Spec (Result : in out N_ValueType_Acc;
                                           Success : out Boolean);

   --  Rule 20
   --  <value_name> ::= <scoped_name>
   procedure Parse_Value_Name (Result : out N_Scoped_Name_Acc;
                               Success : out Boolean)
     renames Parse_Scoped_Name;

   --  Rule 21
   --  <value_element> ::= <export> | < state_member> | <init_dcl>
   procedure Parse_Value_Element  (List : in out Node_List;
                                   Success : out Boolean);



   --  Rule 22
   --  <state_member> ::= ( "public" | "private" )
   --                     <type_spec> <declarators> ";"

   --  Rule 23
   --  <init_dcl> ::= "factory" <identifier> "("
   --                 [ <init_param_decls> ] ")" ";"

   --  Rule 24
   --  <init_param_decls> ::= <init_param_decl> { "," <init_param_decl> }

   --  Rule 25
   --  <init_param_decl> ::= <init_param_attribute> <param_type_spec>
   --                        <simple_declarator>

   --  Rule 26
   --  <init_param_attribute> ::= "in"



   ------------------------------
   --  To resume after errors  --
   ------------------------------

   --  This methods are called when the parser encounters an error
   --  in order to try to continue the parsing

   --  Goes to the beginning of the next definition.
   procedure Go_To_Next_Definition;

   --  Goes to the next Cbracket opening.
   procedure Go_To_Next_Left_Cbracket;

   --  Goes to the next export (see rule 9)
   procedure Go_To_Next_Export;

   --  Goes to the next value_element (see rule 21)
   procedure Go_To_Next_Value_Element;








--    --  FIXME: to add: rules 25, 26, 81, 82.

--    function Parse_Param_Type_Spec return N_Root_Acc;
--    function Parse_Const_Exp return N_Root_Acc;
--    function Parse_Sequence_Type return N_Sequence_Acc;
--    function Parse_Constr_Type_Spec return N_Root_Acc;
--    function Parse_Module return N_Module_Acc;


--    --  Rule 24:
--    --  <literal> ::= <integer_literal>
--    --            | <string_literal>
--    --            | <wide_string_literal>
--    --            | <character_literal>
--    --            | <wide_character_literal>
--    --            | <fixed_pt_literal>
--    --            | <floating_pt_literal>
--    --            | <boolean_literal>
--    function Parse_Literal return N_Root_Acc is

--    --  Rule 74:
--    --  <op_type_spec> ::= <param_type_spec>
--    --                 |   "void"
--    function Parse_Op_Type_Spec return N_Root_Acc is

--    --  Rule 76:
--  --  <param_dcl> ::= <param_attribute> <param_type_spec> <simple_declarator>
--    --
--    --  Rule 77:
--    --  <param_attribute> ::= "in"
--    --                    |   "out"
--    --                    |   "inout"
--    procedure Parse_Param_Dcl (List : in out Node_List) is

--    --  Rule 75:
--    --  <parameter_dcls> ::= "(" <param_dcl> { "," <param_dcl> }* ")"
--    --                   |   "(" ")"
--    function Parse_Parameter_Dcls return Node_List is

--    --  Rule 72:
--    --  <op_dcl> ::= [ <op_attribute> ] <op_type_spec> <identifier>
--    --               <parameters_dcls> [ <raises_expr> ] [ <context_expr> ]
--    --
--    --  Rule 78:
--  --  <raises_expr> ::= "raises" "(" <scoped_name> { "," <scoped_name" }* ")"
--    --
--    --  Rule 79:
--    --  <context_expr> ::= "context" "(" <string_literal> { ","
--    --                                   <string_literal> }* ")"
--    function Parse_Op_Dcl return N_Operation_Acc is

--    --  Rule 23:
--    --  <primary_expr> ::= <scoped_name>
--    --                 |   <literal>
--    --                 |   "(" <const_expr> ")"
--    function Parse_Primary_Expr return N_Root_Acc is

--    --  Rule 21:
--    --  <unary_expr> ::= <unary_operator> <primary_expr>
--    --               |   <primary_expr>
--    --
--    --  Rule 22:
--    --  <unary_operator> ::= "+" | "-" | "~"
--    function Parse_Unary_Expr return N_Root_Acc is

--    --  Rule 20:
--    --  <mult_expr> ::= <unary_expr>
--    --              |   <mult_expr> "*" <unary_expr>
--    --              |   <mult_expr> "/" <unary_expr>
--    --              |   <mult_expr> "%" <unary_expr>
--    function Parse_Mult_Expr return N_Root_Acc is

--    --  Rule 19:
--    --  <add_expr> ::= <mult_expr>
--    --             |   <add_expr> "+" <mult_expr>
--    --             |   <add_expr> "-" <mult_expr>
--    function Parse_Add_Expr return N_Root_Acc is

--    --  Rule 18:
--    --  <shift_expr> ::= <add_expr>
--    --               |   <shift_expr> ">>" <add_expr>
--    --               |   <shift_expr> "<<" <add_expr>
--    function Parse_Shift_Expr return N_Root_Acc is

--    --  Rule 17:
--    --  <and_expr> ::= <shift_expr>
--    --             |   <and_expr> "&" <shift_expr>
--    function Parse_And_Expr return N_Root_Acc is

--    --  Rule 16:
--    --  <xor_expr> ::= <and_expr>
--    --             |   <xor_expr> "^" <and_expr>
--    function Parse_Xor_Expr return N_Root_Acc is

--    --  Rule 15:
--    --  <or_expr> ::= <xor_expr>
--    --            |   <or_expr> "^" <xor_expr>
--    function Parse_Or_Expr return N_Root_Acc is

--    --  Rule 14:
--    --  <const_expr> ::= <or_expr>
--    function Parse_Const_Exp return N_Root_Acc is

--    --  Rule 66:
--    --  <string_type> ::= "string" "<" <positive_int_const> ">"
--    --                |   "string"
--    function Parse_String_Type return N_String_Acc is

--    --  Rule 67:
--    --  <wide_string_type> ::= "wstring" "<" <positive_int_const> ">"
--    --                     |   "wstring"
--    function Parse_Wide_String_Type return N_Root_Acc is

--    --  Rule 81:
--    --  <fixed_pt_type>  ::= "fixed" "<" <positive_int_const> ","
--    --                       <integer_literal> ">"
--    function Parse_Fixed_Pt_Type return N_Root_Acc is

--    --  Rule 31:
--    --  <base_type_spec> ::= <floating_pt_type>
--    --                   |   <integer_type>
--    --                   |   <char_type>
--    --                   |   <wide_char_type>
--    --                   |   <boolean_type>
--    --                   |   <octet_type>
--    --                   |   <any_type>
--    --                   |   <object_type>
--    --
--    --  Rule 38:
--    --  <floating_pt_type> ::= "float"
--    --                     |   "double"
--    --                     |   "long" "double"
--    --
--    --  Rule 39:
--    --  <integer_type> ::= <signed_int>
--    --                 |   <unsigned_int>
--    --
--    --  Rule 40:
--    --  <signed_int> ::= <signed_short_int>
--    --               |   <signed_long_int>
--    --               |   <signed_longlong_int>
--    --
--    --  Rule 41:
--    --  <signed_short_int> ::= "short"
--    --
--    --  Rule 42:
--    --  <signed_long_int> := "long"
--    --
--    --  Rule 43:
--    --  <signed_longlong_int> ::= "long" "long"
--    --
--    --  Rule 44:
--    --  <unsigned_int> ::= <unsigned_short_int>
--    --                 |   <unsigned_long_int>
--    --                 |   <unsigned_longlong_int>
--    --
--    --  Rule 45:
--    --  <unsigned_short_int> ::= "unsigned" "short"
--    --
--    --  Rule 46:
--    --  <unsigned_long_int> ::= "unsigned" "long"
--    --
--    --  Rule 47:
--    --  <unsigned_longlong_int> ::= "unsigned" "long" "long"
--    --
--    --  Rule 48:
--    --  <char_type> ::= "char"
--    --
--    --  Rule 49:
--    --  <wide_char_type> ::= "wchar"
--    --
--    --  Rule 50:
--    --  <boolean_type> ::= "boolean"
--    --
--    --  Rule 51:
--    --  <octet_type> ::= "octet"
--    --
--    --  Rule 52:
--    --  <any_type> ::= "any"
--    --
--    --  Rule 53:
--    --  <object_type> ::= "Object"
--    function Parse_Base_Type_Spec return N_Root_Acc is

--    --  Rule 80:
--    --  <param_type_spec> ::= <base_type_spec>
--    --                    |   <string_type>
--    --                    |   <wide_string_type>
--    --                    |   <fixed_pt_type>
--    --                    |   <scoped_name>
--    function Parse_Param_Type_Spec return N_Root_Acc is

--    --  Rule 70:
--    --  <attr_dcl> ::= [ "readonly" ] "attribute" <param_type_spec>
--    --                 <simple_declarator> { "," <simple_declarator> }*
--    procedure Parse_Attr_Dcl (List : in out Node_List) is

--    --  Rule 32:
--    --  <template_type_spec> ::= <sequence_type>
--    --                       |   <string_type>
--    --                       |   <wide_string_type>
--    --                       |   <fixed_pt_type>
--    function Parse_Template_Type_Spec return N_Root_Acc is

--    --  Rule 30:
--    --  <simple_type_spec> ::= <base_type_spec>
--    --                     |   <template_type_spec>
--    --                     |   <scoped_name>
--    function Parse_Simple_Type_Spec return N_Root_Acc is

--    --  Rule 65:
--    --  <sequence_type> ::= "sequence" "<" <simple_type_spec>
--    --                      "," <positive_int_const> ">"
--    --                  |   "sequence" "<" <simple_type_spec> ">"
--    function Parse_Sequence_Type return N_Sequence_Acc is

--    --  Rule 29:
--    --  <type_spec> ::= <simple_type_spec>
--    --              |   <constr_type_spec>
--    function Parse_Type_Spec return N_Root_Acc is

--    --  Rule 35:
--    --  <declarator> ::= <simple_declarator>
--    --               |   <complex_declarator>
--    --
--    --  Rule 36:
--    --  <simple_declarator> ::= <identifier>
--    --
--    --  Rule 37:
--    --  <complex_declarator> ::= <array_declarator>
--    --
--    --  Rule 68:
--    --  <array_declarator> ::= <identifier> <fixed_array_size>+
--    --
--    --  Rule 69:
--    --  <fixed_array_size> ::= "[" <positive_int_const> "]"
--    function Parse_Declarator return N_Declarator_Acc is

--    --  Rule 34:
--    --  <declarators> ::= <declarator> { "," <declarator> }*
--    procedure Parse_Declarators (List : in out Node_List) is

--    --  Rule 55:
--    --  <member_list> ::= <member>+
--    --
--    --  Rule 56:
--    --  <member> ::= <type_spec> <declarators> ";"
--    procedure Parse_Member_List (List : in out Node_List) is

--    --  Rule 71:
--    --  <except_dcl> ::= "exception" <identifier> "{" <member>* "}"
--    function Parse_Except_Dcl return N_Exception_Acc is

--    --  Rule 60:
--    --  <case> ::= <case_label>+ <element_spec> ";"
--    --
--    --  Rule 61:
--    --  <case_label> ::= "case" <const_exp> ":"
--    --               |   "default ":"
--    function Parse_Case return N_Case_Acc is

--    --  Rule 57:
--    --  <union_type> ::= "union" <identifier>
--    --                   "switch" "(" <switch_type_spec> ")"
--    --                   "{" <switch_body> "}"
--    --
--    --  Rule 58:
--    --  <switch_type_spec> ::= <integer_type>
--    --                     |   <char_type>
--    --                     |   <boolean_type>
--    --                     |   <enum_type>
--    --                     |   <scoped_name>
--    --
--    --  Rule 59:
--    --  <switch_body> ::= <case>+
--    function Parse_Union_Type return N_Union_Acc is

--    --  Rule 54:
--    --  <struct_type> ::= "struct" <identifier> "{" <member_list> "}"
--    function Parse_Struct_Type return N_Struct_Acc is

--    --  Rule 63:
--    --  <enum_type> ::= "enum" <identifier> "{" <enumerator>
--    --                  { "," <enumerator> }* "}"
--    --  Rule 64:
--    --  <enumerator> ::= <identifier>
--    function Parse_Enum_Type return N_Enum_Acc is

--    --  Rule 33:
--    --  <constr_type_spec> ::= <struct_type>
--    --                     |   <union_type>
--    --                     |   <enum_type>
--    function Parse_Constr_Type_Spec return N_Root_Acc is

--    --  Rule 28:
--    --  <type_declarator> ::= <type_spec> <declarators>
--    function Parse_Type_Declarator return N_Type_Declarator_Acc is

--    --  Rule 27:
--    --  <type_dcl> ::= "typedef" <type_declarator>
--    --             |   <struct_type>
--    --             |   <union_type>
--    --             |   <enum_type>
--    --             |   "native" <simple_declarator>
--    function Parse_Type_Dcl return N_Root_Acc is

   --  Rule 8:
   --  <interface_body> ::= <export>*
   procedure Parse_Interface_Body (List : in out Node_List);

   --  <interface_dcl_end> ::= [<interface_inheritance_spec>] "{"
   --                          <interface_body> "}"
   --
   --  Rule 10:
   --  <inheritance_spec> ::= ":" <scoped_name> { "," <scoped_name> }*
   procedure Parse_Interface_Dcl_End (Result : in out  N_Interface_Acc;
                                     Success : out Boolean);

   --  Rule 4:
   --  <interface> ::= <interface_dcl> | <forward_dcl>
   --
   --  Rule 5:
   --  <interface_decl> ::= <interface_header> "{" <interface_body> "}"
   --
   --  Rule 6:
   --  <forward_dcl> ::= ["abstract"] "interface" <identifier>
   --
   --  Rule 7:
   --  <interface_header> ::= ["abstract"] "interface" <identifier>
   --                         [ <interface_inheritance_spec> ]
   --
   --
   --  These rules are equivalent to
   --
   --  <interface> ::= ["abstract"] "interface" <identifier>
   --                  <interface_end>
   --
   --  <interface_end> ::= <forward_dcl_end>
   --                  |   <interface_dcl_end>
   --
   --  <forward_dcl_end> ::=
   --
   --  <interface_dcl_end> ::= [<interface_inheritance_spec>] "{"
   --                          <interface_body> "}"
   --  this last will be used in Parse_Interface_Dcl_End
   procedure Parse_Interface (Result : out N_Root_Acc;
                              Success : out Boolean);

--    --  Rule 13:
--    --  <const_type> ::= <integer_type>
--    --               |   <char_type>
--    --               |   <wide_char_type>
--    --               |   <boolean_type>
--    --               |   <floating_pt_type>
--    --               |   <string_type>
--    --               |   <wide_string_type>
--    --               |   <fixed_pt_const_type>
--    --               |   <scoped_name>
--    function Parse_Const_Type return N_Root_Acc is

--    --  Rule 12:
--    --  <const_dcl> ::= "const" <const_type> <identifier> "=" <const_exp>
--    function Parse_Const_Dcl return N_Const_Acc is





end Parse;
