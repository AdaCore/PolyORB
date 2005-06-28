with GNAT.Table;

with Errors;    use Errors;
with Lexer;     use Lexer;
with Locations; use Locations;
with Namet;     use Namet;
with Scopes;    use Scopes;
with Types;     use Types;
with Values;    use Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils; use Frontend.Nutils;

package body Parser is

   Specification : Node_Id;

   procedure Declare_Base_Type (L : Token_List_Type; K : Node_Kind);
   --  L denotes a token list used to name an IDL base type. Allocate
   --  a node for it and associate it to the concatenated names.

   function Resolve_Base_Type (L : Token_List_Type) return Node_Id;
   --  Take the sequence of tokens in the paremter list to return the
   --  node of the IDL predefined type.

   function Is_Param_Type_Spec (E : Node_Id) return Boolean;
   --  Return true when the type specifier N belongs to the restricted
   --  parameter type specifier set.

   Sequencing_Level : Natural := 0;

   function P_No_Such_Node return Node_Id;

   function P_Attribute_Declaration return Node_Id;
   function P_Constant_Declaration return Node_Id;
   function P_Constant_Expression return Node_Id;
   function P_Constant_Type return Node_Id;
   function P_Declarator return Node_Id;
   function P_Declarator_List return List_Id;
   function P_Definition return Node_Id;
   function P_Enumeration_Type return Node_Id;
   function P_Exception_Declaration return Node_Id;
   function P_Export return Node_Id;
   function P_Fixed_Point_Type return Node_Id;
   function P_Identifier return Node_Id;
   function P_Initializer_Declaration return Node_Id;
   function P_Interface return Node_Id;
   function P_Interface_Declaration return Node_Id;
   function P_Interface_Name return Node_Id;
   function P_Member return Node_Id;
   function P_Module return Node_Id;
   function P_Operation_Declaration return Node_Id;
   function P_Parameter_Declaration return Node_Id;
   function P_Pragma return Node_Id
     renames P_No_Such_Node;
   function P_Scoped_Name return Node_Id;
   function P_Sequence_Type return Node_Id;
   function P_Simple_Declarator return Node_Id;
   function P_Simple_Type_Spec return Node_Id;
   function P_Specification return Node_Id;
   function P_State_Member return Node_Id;
   function P_Structure_Type return Node_Id;
   function P_String_Type return Node_Id;
   function P_Type_Declaration return Node_Id;
   function P_Type_Spec return Node_Id;
   function P_Union_Type return Node_Id;
   function P_Value return Node_Id;
   function P_Value_Abstract_Declaration return Node_Id;
   function P_Value_Box_Declaration return Node_Id;
   function P_Value_Declaration return Node_Id;
   function P_Value_Forward_Declaration return Node_Id;
   function P_Value_Spec return Node_Id;

   package Expressions is new GNAT.Table (Node_Id, Natural, 1, 100, 10);

   Preferences : constant array (T_Tilde .. T_Less_Less) of Natural
     := (T_Tilde            => 0,
         T_Percent          => 1,
         T_Slash            => 2,
         T_Star             => 3,
         T_Minus            => 4,
         T_Plus             => 5,
         T_Less_Less        => 6,
         T_Greater_Greater  => 7,
         T_Ampersand        => 8,
         T_Circumflex       => 9,
         T_Bar              => 10);

   -----------------------
   -- Declare_Base_Type --
   -----------------------

   procedure Declare_Base_Type (L : Token_List_Type; K : Node_Kind) is
      E : Node_Id;
      N : Name_Id;
   begin

      --  Create a fake node located at the beginning of the
      --  specification (current token location).

      E := New_Node (K, No_Location);

      --  Accumulate token names and store node id as table info

      Set_Str_To_Name_Buffer (Image (L (L'First)));
      for I in L'First + 1 .. L'Last loop
         Add_Char_To_Name_Buffer (' ');
         Add_Str_To_Name_Buffer (Image (L (I)));
      end loop;
      N := Name_Find;
      Set_Name_Table_Info (N, Int (E));
      Set_Image (Base_Type (E), N);
   end Declare_Base_Type;

   ------------------------
   -- Is_Param_Type_Spec --
   ------------------------

   --  (95) <param_type_spec> ::= <base_type_spec>
   --                           | <string_type>
   --                           | <wide_string_type>
   --                           | <scoped_name>

   function Is_Param_Type_Spec (E : Node_Id) return Boolean is
   begin
      case Kind (E) is
         when K_Float
           | K_Double
           | K_Long_Double
           | K_Short
           | K_Long
           | K_Long_Long
           | K_Unsigned_Short
           | K_Unsigned_Long
           | K_Unsigned_Long_Long
           | K_Char
           | K_Wide_Char
           | K_Boolean
           | K_Any
           | K_Object
           | K_Octet
           | K_Value_Base
           | K_String
           | K_Wide_String
           | K_String_Type
           | K_Wide_String_Type
           | K_Scoped_Name =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Param_Type_Spec;

   -----------------------------
   -- P_Attribute_Declaration --
   -----------------------------

   --  (85) <attr_dcl> ::= [ "readonly" ] "attribute"
   --                      <param_type_spec> <simple_declarator>
   --                                  { "," <simple_declarator> }*

   function P_Attribute_Declaration return Node_Id is
      Attribute_Decl  : Node_Id;
      Attr_Type_Spec  : Node_Id;
      Is_Readonly     : Boolean := False;
      Declarators     : List_Id;
      Declarator      : Node_Id;
   begin
      Scan_Token; --  past "readonly" or "attribute"

      if Token = T_Readonly then
         Is_Readonly := True;
         Scan_Token (T_Attribute);
         if Token = T_Error then
            return No_Node;
         end if;
      end if;

      --  Read general type specifier

      Attr_Type_Spec := P_Type_Spec;
      if No (Attr_Type_Spec) then
         return No_Node;
      end if;

      --  Check that the type specifier follows the restriction of the
      --  parameter type specifier.

      if not Is_Param_Type_Spec (Attr_Type_Spec) then
         Error_Loc (1) := Loc (Attr_Type_Spec);
         DE ("incorrect attribute type spec");
         return No_Node;
      end if;

      Declarators := P_Declarator_List;
      if Is_Empty (Declarators) then
         return No_Node;
      end if;

      Attribute_Decl := New_Node (K_Attribute_Declaration,
                                    Loc (Attr_Type_Spec));
      Set_Is_Readonly (Attribute_Decl, Is_Readonly);
      Set_Type_Spec   (Attribute_Decl, Attr_Type_Spec);
      Set_Declarators (Attribute_Decl, Declarators);
      Bind_Declarators_To_Entity (Declarators, Attribute_Decl);

      Declarator := First_Entity (Declarators);
      while Present (Declarator) loop
         if Kind (Declarator) /= K_Simple_Declarator then
            Error_Loc (1) := Loc (Declarator);
            DE ("incorrect attribute declarator");
            return No_Node;
         end if;
         Declarator := Next_Entity (Declarator);
      end loop;

      return Attribute_Decl;
   end P_Attribute_Declaration;

   ----------------------------
   -- P_Constant_Declaration --
   ----------------------------

   --  (27) <const_dcl> ::= "const" <const_type> <identifier> "=" <const_exp>

   function P_Constant_Declaration return Node_Id
   is
      Constant_Decl   : Node_Id;
      Const_Type_Spec : Node_Id;
      Const_Expr      : Node_Id;
   begin
      Scan_Token; --  past "const"

      Const_Type_Spec := P_Constant_Type;
      if No (Const_Type_Spec) then
         return No_Node;
      end if;

      Constant_Decl := P_Simple_Declarator;
      if No (Constant_Decl) then
         return No_Node;
      end if;

      Set_Kind (Constant_Decl, K_Constant_Declaration);
      Set_Type_Spec (Constant_Decl, Const_Type_Spec);

      Scan_Token (T_Equal);
      if Token = T_Error then
         return No_Node;
      end if;

      Const_Expr := P_Constant_Expression;
      if No (Const_Expr) then
         return No_Node;
      end if;
      Set_Expression (Constant_Decl, Const_Expr);

      return Constant_Decl;
   end P_Constant_Declaration;

   ---------------------------
   -- P_Constant_Expression --
   ---------------------------

   --  (29) <const_exp> ::= <or_expr>
   --  (30) <or_expr> ::= <xor_expr>
   --                   | <or_expr> "|" <xor_expr>

   --  (31) <xor_expr> ::= <and_expr>
   --                    | <xor_expr> "^" <and_expr>

   --  (32) <and_expr> ::= <shift_expr>
   --                    | <and_expr> "&" <shift_expr>

   --  (33) <shift_expr> ::= <add_expr>
   --                      | <shift_expr> ">>" <add_expr>
   --                      | <shift_expr> "<<" <add_expr>

   --  (34) <add_expr> ::= <mult_expr>
   --                    | <add_expr> "+" <mult_expr>
   --                    | <add_expr> "-" <mult_expr>

   --  (35) <mult_expr> ::= <unary_expr>
   --                     | <mult_expr> "*" <unary_expr>
   --                     | <mult_expr> "/" <unary_expr>
   --                     | <mult_expr> "%" <unary_expr>

   --  (36) <unary_expr> ::= <unary_operator> <primary_expr>
   --                      | <primary_expr>

   --  (37) <unary_operator> ::= "-"
   --                          | "+"
   --                          | "~"

   --  (38) <primary_expr> ::= <scoped_name>
   --                        | <literal>
   --                        | "(" <const_exp> ")"

   function P_Constant_Expression return Node_Id is

      use Expressions;

      --  There are two kinds of expressions. A binary operator has
      --  two inner expressions (left and right). When the right
      --  expression is assigned and not the left one, the operator is
      --  an unary operator and this expression is considered as an
      --  expression value. When both inner expressions are assigned,
      --  this is also an expression value. An operator is a binary
      --  operator when at least the right expression is not
      --  assigned. An expression value can be an operator with at
      --  least a right expression assigned or a literal or a scoped
      --  name.

      function Is_Expression_Completed return Boolean;
      --  Return True when there are no more token to read to complete
      --  the current expression.

      function P_Expression_Part return Node_Id;
      --  XXX LP: Cannot parse comment
      --  Return a node describing an expression. It is either a
      --  binary operator (an operator with no right expression
      --  assigned) or an expression value (a scoped name, a literal
      --  or an expression with an unary operator - that is a binary
      --  operator with a right inner expression and no left inner
      --  expression - or an expression with both inner expressions
      --  assigned). Note that whether an operator is a binary or
      --  unary operator is resolved in this routine. For a unary
      --  operator, we check that the previous token was a binary
      --  operator.

      function Is_Binary_Operator (E : Node_Id) return Boolean;
      --  Return True when N is an operator with the right expression
      --  still not assigned. Otherwise, an operator with a right
      --  expression is a value expression.

      function Is_Expression_Value (E : Node_Id) return Boolean;
      --  Return True when N is not an operator (literal or scoped
      --  name) or else when its right expression is assigned (unary
      --  operator).

      function Precede (L, R : Node_Id) return Boolean;
      --  Does operator L precedes operator R

      Exp_Err_Msg : constant String := "cannot parse expression";
      --  Standard error message

      -----------------------------
      -- Is_Expression_Completed --
      -----------------------------

      function Is_Expression_Completed return Boolean
      is
         T : constant Token_Type := Next_Token;
      begin
         return T not in Literal_Type
           and then T /= T_Identifier
           and then T /= T_Colon_Colon
           and then T /= T_Left_Paren
           and then T not in Operator_Type;
      end Is_Expression_Completed;

      -------------------------
      -- Is_Expression_Value --
      -------------------------

      function Is_Expression_Value (E : Node_Id) return Boolean is
      begin
         return Kind (E) in K_Integer_Literal .. K_Boolean_Literal
           or else Kind (E) = K_Scoped_Name
           or else (Operator (E) in Unary_Operator_Type
                    and then Present (Right_Expr (E)))
           or else (Operator (E) in Binary_Operator_Type
                    and then Present (Left_Expr (E))
                    and then Present (Right_Expr (E)));
      end Is_Expression_Value;

      ------------------------
      -- Is_Binary_Operator --
      ------------------------

      function Is_Binary_Operator (E : Node_Id) return Boolean is
      begin
         return Kind (E) = K_Expression
           and then Operator (E) in Binary_Operator_Type
           and then No (Right_Expr (E));
      end Is_Binary_Operator;

      -----------------------
      -- P_Expression_Part --
      -----------------------

      function P_Expression_Part return Node_Id is
         Expression     : Node_Id := No_Node;
         Right_Expr     : Node_Id;
         Previous_Token : Token_Type;
      begin
         case Next_Token is
            when T_Identifier
              | T_Colon_Colon =>

               --  Look for a scoped name

               Expression := P_Scoped_Name;
               if No (Expression) then
                  return No_Node;
               end if;

            when T_Left_Paren =>

               --  Look for a parenthesized expression value

               Scan_Token;  --  past '('
               Expression := P_Constant_Expression;

               Scan_Token (T_Right_Paren);
               if Token = T_Error then
                  return No_Node;
               end if;

            when T_Integer_Literal        =>
               Scan_Token;  --  past literal
               Expression :=
                 New_Node (K_Integer_Literal, Token_Location);
               Set_Value
                 (Expression,
                  New_Integer_Value (Value => Integer_Literal_Value,
                                     Sign  => 1,
                                     Base  => Integer_Literal_Base));

            when T_Fixed_Point_Literal    =>
               Scan_Token;  --  past literal
               Expression :=
                 New_Node (K_Fixed_Point_Literal, Token_Location);
               Set_Value
                 (Expression,
                  New_Fixed_Point_Value
                  (Value => Integer_Literal_Value,
                   Sign  => 1,
                   Total => Unsigned_Short_Short (Name_Len),
                   Scale => Decimal_Point_Position));

            when T_Boolean_Literal    =>
               Scan_Token;  --  past literal
               Expression :=
                 New_Node (K_Boolean_Literal, Token_Location);
               Set_Value
                 (Expression,
                  New_Boolean_Value (Value => (Integer_Literal_Value = 1)));

            when T_Floating_Point_Literal =>
               Scan_Token;  --  past literal
               Expression :=
                 New_Node (K_Floating_Point_Literal, Token_Location);
               Set_Value
                 (Expression,
                  New_Floating_Point_Value (Float_Literal_Value));

            when T_Character_Literal
              |  T_Wide_Character_Literal =>
               Scan_Token;  --  past literal
               if Character_Literal_Value /= Incorrect_Character then
                  Expression :=
                    New_Node (K_Character_Literal, Token_Location);
                  Set_Value
                    (Expression,
                     New_Character_Value
                     (Character_Literal_Value,
                      (Token /= T_Character_Literal)));
               end if;

            when T_String_Literal
              |  T_Wide_String_Literal =>
               Scan_Token;  --  past literal
               if String_Literal_Value /= Incorrect_String then
                  Expression :=
                    New_Node (K_String_Literal, Token_Location);
                  Set_Value
                    (Expression,
                     New_String_Value (String_Literal_Value,
                                       (Token /= T_String_Literal)));
               end if;

            when T_Tilde .. T_Less_Less =>

               --  Look for a binary/unary operator

               Previous_Token := Token;
               Scan_Token;  --  past binary/unary operator

               Expression := New_Node (K_Expression, Token_Location);
               Set_Operator (Expression, Token);

               --  Token is a real unary operator

               if Token = T_Tilde
                 or else (Token in T_Minus .. T_Plus
                          and then not Is_Literal (Previous_Token)
                          and then not Is_Scoped_Name (Previous_Token)
                          and then Previous_Token /= T_Right_Paren)
               then
                  case Next_Token is
                     when T_Identifier
                       | T_Colon_Colon
                       | T_Left_Paren
                       | T_Integer_Literal .. T_Wide_String_Literal =>

                        --  Look for an expression value (a scoped
                        --  name, a literal or a parenthesized
                        --  expression).

                        Right_Expr := P_Constant_Expression;
                        if No (Right_Expr) then
                           Error_Loc (1) := Loc (Expression);
                           DE (Exp_Err_Msg);
                           return No_Node;
                        end if;
                        Set_Right_Expr (Expression, Right_Expr);

                     when others =>
                        Unexpected_Token (Token, "expression");
                        return No_Node;
                  end case;

               --  Cannot have two following operators except in the
               --  special case above.

               elsif Is_Operator (Previous_Token) then
                  Unexpected_Token (Token, "expression");
                  return No_Node;
               end if;

            when others =>
               Error_Loc (1) := Token_Location;
               DE (Exp_Err_Msg);
               return No_Node;
         end case;

         return Expression;
      end P_Expression_Part;

      -------------
      -- Precede --
      -------------

      function Precede (L, R : Node_Id) return Boolean is
         Op_L : constant Token_Type := Operator (L);
         Op_R : constant Token_Type := Operator (R);
      begin
         return Preferences (Op_L) <= Preferences (Op_R);
      end Precede;

      Expr     : Node_Id;
      First    : Natural;

   begin

      --  Read enough expressions to push as first expression a binary
      --  operator with no right expression

      Expr := P_Expression_Part;
      if No (Expr) then
         return No_Node;
      end if;

      --  We must have first an expression value

      if Is_Binary_Operator (Expr) then
         Error_Loc (1) := Loc (Expr);
         DE (Exp_Err_Msg);
         return No_Node;
      end if;

      --  We have only one expression value

      if Is_Expression_Completed then
         return Expr;
      end if;

      Increment_Last;
      Table (Last) := Expr;
      First := Last;

      Expr := P_Expression_Part;
      if No (Expr) then
         Set_Last (First - 1);
         return No_Node;
      end if;

      --  We must have a binary operator as the first expression is an
      --  expression value.

      if not Is_Binary_Operator (Expr) then
         Error_Loc (1) := Loc (Expr);
         DE (Exp_Err_Msg);
         Set_Last (First - 1);
         return No_Node;
      end if;

      Set_Left_Expr (Expr, Table (Last));
      Table (Last) := Expr;

      --  Push expressions in stack and check that the top of the
      --  stack consists in one or more binary operators with no
      --  right expr and zero or one expression value.

      while not Is_Expression_Completed loop
         Expr := P_Expression_Part;
         if No (Expr) then
            return No_Node;
         end if;

         Increment_Last;
         Table (Last) := Expr;

         --  Check that this new expression is not a binary operator
         --  when the previous one is a binary operator with no right
         --  expression.

         if First < Last
           and then Is_Binary_Operator (Expr)
           and then No (Left_Expr (Expr))
           and then Is_Binary_Operator (Table (Last - 1))
         then
            Error_Loc (1) := Loc (Expr);
            DE (Exp_Err_Msg);
            Set_Last (First - 1);
            return No_Node;
         end if;

         --  Check whether we have a sequence of a binary operator
         --  (left operator), an expression value and another binary
         --  operator (right operator). In this case, if the left
         --  operator has a better precedence than the right one, we
         --  can reduce the global expression by assigning the
         --  expression value to the right expression of the left
         --  operator. Then as the left operator has already a left
         --  expression, it becomes an expression value which can be
         --  assign to the left expression of the right operation.
         --  Recompute the size of the expression stack.

         while First + 1 < Last
           and then Is_Expression_Value (Table (Last - 1))
           and then Precede (Table (Last - 2), Expr)
         loop
            Set_Right_Expr (Table (Last - 2), Table (Last - 1));
            Table (Last - 1) := Table (Last);
            Set_Last (Last - 1);
            if No (Left_Expr (Table (Last - 1))) then
               Set_Left_Expr (Table (Last - 1), Table (Last - 2));
               Table (Last - 2) := Table (Last - 1);
               Table (Last - 1) := Table (Last);
               Set_Last (Last - 1);
            end if;
         end loop;
      end loop;

      --  The last expression is not a value. We cannot reduce the
      --  global expression

      if Is_Binary_Operator (Table (Last)) then
         Error_Loc (1) := Loc (Table (Last));
         DE (Exp_Err_Msg);
         Set_Last (First - 1);
         return No_Node;
      end if;

      --  Reduce the global expression

      while First < Last loop
         Set_Right_Expr (Table (Last - 1), Table (Last));
         Set_Last (Last - 1);
         if No (Left_Expr (Table (Last))) then
            Set_Left_Expr (Table (Last), Table (Last - 1));
            Table (Last - 1) := Table (Last);
            Set_Last (Last - 1);
         end if;
      end loop;

      Expr := Table (First);
      Set_Last (First - 1);

      return Expr;
   end P_Constant_Expression;

   ---------------------
   -- P_Constant_Type --
   ---------------------

   --  (28) <const_type> ::= <floating_pt_type>
   --                      | <integer_type>
   --                      | <char_type>
   --                      | <wide_char_type>
   --                      | <boolean_type>
   --                      | <octet_type>
   --                      | <string_type>
   --                      | <wide_string_type>
   --                      | <fixed_pt_const_type>
   --                      | <scoped_name>

   function P_Constant_Type return Node_Id is
      Const_Type : Node_Id;
      State      : Location;
   begin

      --  Use P_Simple_Type_Spec and reject incorrect type specifiers.

      Save_Lexer (State);
      Const_Type := P_Simple_Type_Spec;
      case Kind (Const_Type) is
         when K_Any
           | K_Object
           | K_Value_Base
           | K_Sequence_Type
           | K_Fixed_Point_Type =>
            Restore_Lexer (State);
            Unexpected_Token (Next_Token, "type specifier");
            return No_Node;

         when others =>
            return Const_Type;
      end case;
   end P_Constant_Type;

   ------------------
   -- P_Declarator --
   ------------------

   --  (50) <declarator> ::= <simple_declarator>
   --                      | <complex_declarator>
   --  (51) <simple_declarator> ::= <identifier>
   --  (52) <complex_declarator> ::= <array_declarator>
   --
   --  (83) <array_declarator> ::= <identifier> <fixed_array_size> +
   --  (84) <fixed_array_size> ::= "[" <positive_int_const> "]"

   function P_Declarator return Node_Id is
      Identifier  : Node_Id;
      Array_Sizes : List_Id;
      Array_Size  : Node_Id;
      Node        : Node_Id;

   begin
      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;

      if Next_Token /= T_Left_Bracket then
         Node := New_Node (K_Simple_Declarator, Loc (Identifier));
         Bind_Identifier_To_Entity (Identifier, Node);
         return Node;
      end if;

      Node := New_Node (K_Complex_Declarator, Loc (Identifier));
      Bind_Identifier_To_Entity (Identifier, Node);

      Array_Sizes := New_List (K_Array_Size_List, Token_Location);
      Set_Array_Sizes (Node, Array_Sizes);

      loop
         Scan_Token; --  past '['

         Array_Size := P_Constant_Expression;
         if No (Array_Size) then
            return No_Node;
         end if;

         Append_Node_To_List (Array_Size, Array_Sizes);

         Scan_Token (T_Right_Bracket);
         if Token = T_Error then
            return No_Node;
         end if;

         exit when Next_Token /= T_Left_Bracket;
      end loop;

      return Node;
   end P_Declarator;

   -----------------------
   -- P_Declarator_List --
   -----------------------

   --  (49) <declarators> ::= <declarator> { "," <declarator> }

   function P_Declarator_List return List_Id
   is
      List : List_Id;
      Node : Node_Id;
   begin
      List := New_List (K_Declarators, Token_Location);
      loop
         Node := P_Declarator;
         if No (Node) then
            return List;
         end if;

         Append_Node_To_List (Node, List);
         exit when Next_Token /= T_Comma;
         Scan_Token; --  past ','
      end loop;

      return List;
   end P_Declarator_List;

   ------------------
   -- P_Definition --
   ------------------

   --  (2) <definition> ::= <type_dcl> ";"
   --                     | <const_dcl> ";"
   --                     | <except_dcl> ";"
   --                     | <interface> ";"
   --                     | <module> ";"
   --                     | <value> ";"

   function P_Definition return Node_Id is
      Definition : Node_Id := No_Node;
      State      : Location;
   begin
      Save_Lexer (State);
      Scan_Token;
      case Token is
         when T_Typedef
           | T_Struct
           | T_Union
           | T_Enum
           | T_Native =>
            Restore_Lexer (State);
            Definition := P_Type_Declaration;

         when T_Const =>
            Restore_Lexer (State);
            Definition := P_Constant_Declaration;

         when T_Exception =>
            Restore_Lexer (State);
            Definition := P_Exception_Declaration;

         when T_Abstract =>
            Scan_Token ((T_Interface, T_Value_Type));
            if Token = T_Interface then
               Restore_Lexer (State);
               Definition := P_Interface;

            elsif Token = T_Value_Type then
               Restore_Lexer (State);
               Definition := P_Value;
            end if;

         when T_Interface =>
            Restore_Lexer (State);
            Definition := P_Interface;

         when T_Module =>
            Restore_Lexer (State);
            Definition := Node_Id (P_Module);

         when T_Value_Type
           | T_Custom =>
            Restore_Lexer (State);
            Definition := P_Value;

         when T_Pragma =>
            Restore_Lexer (State);
            Definition := P_Pragma;

         when others =>
            Unexpected_Token (Token, "definition");
      end case;

      --  The definition is successfully parsed

      if Present (Definition) then
         Save_Lexer (State);
         Scan_Token (T_Semi_Colon);
         if Token = T_Error then
            Definition := No_Node;
         end if;
      end if;

      if No (Definition) then
         Restore_Lexer (State);
         Skip_Declaration (T_Semi_Colon);
      end if;

      return Definition;
   end P_Definition;

   ------------------------
   -- P_Enumeration_Type --
   ------------------------

   --  (78) <enum_type> ::= "enum" <identifier>
   --                       "{" <enumerator> { "," <enumerator> } "}"
   --
   --  (79) <enumerator> ::= <identifier>

   function P_Enumeration_Type return Node_Id is
      Identifier  : Node_Id;
      Node        : Node_Id;
      Enumerator  : Node_Id;
      Enumerators : List_Id;
      State       : Location;
      Position    : Unsigned_Long_Long := 0;

   begin
      Scan_Token; --  past "enum"
      Node := New_Node (K_Enumeration_Type, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         return No_Node;
      end if;

      Enumerators := New_List (K_Enumerator_List, Token_Location);
      Set_Enumerators (Node, Enumerators);

      loop

         --  Save lexer state in order to skip the enumerator list on error

         Save_Lexer (State);
         Identifier := P_Identifier;
         if No (Identifier) then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit;
         end if;

         Enumerator := New_Node (K_Enumerator, Loc (Identifier));
         Bind_Identifier_To_Entity (Identifier, Enumerator);

         Append_Node_To_List (Enumerator, Enumerators);
         Position := Position + 1;
         Set_Value
           (Enumerator,
            New_Enumerator (IDL_Name (Identifier), Position));

         Save_Lexer (State);
         Scan_Token ((T_Comma, T_Right_Brace));
         if Token /= T_Comma then
            if Token = T_Error then
               Restore_Lexer (State);
               Skip_Declaration (T_Right_Brace);
            end if;
            exit;
         end if;
      end loop;

      return Node;
   end P_Enumeration_Type;

   -----------------------------
   -- P_Exception_Declaration --
   -----------------------------

   --  (86) <except_dcl> ::= "exception" <identifier> "{" <member>* "}"

   function P_Exception_Declaration return Node_Id is
      Identifier : Node_Id;
      Node     : Node_Id;
      Member     : Node_Id;
      Members    : List_Id;
      State      : Location;

   begin
      Scan_Token; --  past "exception"
      Node := New_Node (K_Exception_Declaration, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         return No_Node;
      end if;

      Members := New_List (K_Member_List, Token_Location);
      Set_Members      (Node, Members);

      loop
         if Next_Token = T_Right_Brace then
            Scan_Token; --  past '}'
            exit;
         end if;

         --  Save lexer state to skip exception member list on error

         Save_Lexer (State);
         Member := P_Member;
         if No (Member) then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit;
         end if;

         Append_Node_To_List (Member, Members);
      end loop;

      return Node;
   end P_Exception_Declaration;

   --------------
   -- P_Export --
   --------------

   --  (9) <export> ::= <type_dcl> ";"
   --                 | <const_dcl> ";"
   --                 | <except_dcl> ";"
   --                 | <attr_dcl> ";"
   --                 | <op_dcl> ";"

   function P_Export return Node_Id is
      State  : Location;
      Export : Node_Id;

   begin

      --  Save lexer state to skip declaration on error

      Save_Lexer (State);
      Scan_Token;
      case Token is
         when T_Const =>
            Restore_Lexer (State);
            Export := P_Constant_Declaration;

         when T_Exception =>
            Restore_Lexer (State);
            Export := P_Exception_Declaration;

         when T_Attribute
           | T_Readonly =>
            Restore_Lexer (State);
            Export := P_Attribute_Declaration;

         when T_Typedef
           | T_Struct
           | T_Union
           | T_Enum
           | T_Native =>
            Restore_Lexer (State);
            Export := P_Type_Declaration;

         when others =>
            Restore_Lexer (State);
            Export := P_Operation_Declaration;
      end case;

      Scan_Token (T_Semi_Colon);
      if Token = T_Error then
         Export := No_Node;
      end if;

      if No (Export) then
         Restore_Lexer (State);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      return Export;
   end P_Export;

   ------------------------
   -- P_Fixed_Point_Type --
   ------------------------

   --  (96) <fixed_pt_type> ::= "fixed" "<" <positive_int_const> ","
   --                                       <positive_int_const> ">"
   --
   --  (97) <fixed_pt_const_type> ::= "fixed"

   function P_Fixed_Point_Type return Node_Id is
      Node : Node_Id;

   begin
      Scan_Token; --  past "fixed"
      Node := New_Node (K_Fixed_Point_Type, Token_Location);

      if Next_Token = T_Less then
         Scan_Token; --  past '<'

         Scan_Token (T_Integer_Literal);
         if Token = T_Error then
            return No_Node;
         end if;
         if Integer_Literal_Sign < 0
           or else Integer_Literal_Value = 0
           or else Integer_Literal_Value > 31
         then
            Error_Loc (1) := Token_Location;
            DE ("fixed point values must have between 1 and 31 digits");
            return No_Node;
         end if;
         Set_N_Total (Node, Int (Integer_Literal_Value));

         Scan_Token (T_Comma);
         if Token = T_Error then
            return No_Node;
         end if;

         Scan_Token (T_Integer_Literal);
         if Token = T_Error then
            return No_Node;
         end if;
         if Integer_Literal_Sign < 0
           or else Integer_Literal_Value = 0
           or else Integer_Literal_Value > 31
         then
            Error_Loc (1) := Token_Location;
            DE ("fixed point values must have between 1 and 31 digits");
            return No_Node;
         end if;
         if N_Total (Node) < Int (Integer_Literal_Value) then
            Error_Loc (1) := Token_Location;
            DE ("fixed point scale factor is greater than number of digits");
            return No_Node;
         end if;
         Set_N_Scale (Node, Int (Integer_Literal_Value));

         Scan_Token (T_Greater);
         if Token = T_Error then
            return No_Node;
         end if;
      end if;

      return Node;
   end P_Fixed_Point_Type;

   ------------------
   -- P_Identifier --
   ------------------

   function P_Identifier return Node_Id is
   begin
      Scan_Token (T_Identifier);
      if Token = T_Error then
         return No_Node;
      end if;
      return Make_Identifier (Token_Location, Token_Name, No_Node, No_Node);
   end P_Identifier;

   -------------------------------
   -- P_Initializer_Declaration --
   -------------------------------

   --  (23) <init_dcl> ::= "factory" <identifier>
   --                           "(" [ <init_param_decls> ] ")" ";"
   --  (24) <init_param_decls> ::= <init_param_decl>
   --                        { "," <init_param_decl> }*
   --  (25) <init_param_decl> ::= <init_param_attribute> <param_type_spec>
   --                                                    <simple_declarator>

   function P_Initializer_Declaration return Node_Id is
      Identifier : Node_Id;
      Node     : Node_Id;
      Parameters : List_Id;
      Parameter  : Node_Id;
      State      : Location;

   begin
      Scan_Token; -- past "factory"
      Node := New_Node (K_Initializer_Declaration, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         return No_Node;
      end if;

      Parameters := New_List (K_Parameter_List, Token_Location);
      Set_Parameters   (Node, Parameters);

      loop
         --  Check the parameter mode is "in". Then parse a general
         --  parameter declaration.

         Save_Lexer (State);
         if Next_Token = T_In then
            Parameter := P_Parameter_Declaration;
            if No (Parameter) then
               Restore_Lexer (State);
               Skip_Declaration (T_Right_Paren);
               exit;
            end if;

            Append_Node_To_List (Parameter, Parameters);
         end if;

         Save_Lexer (State);
         Scan_Token ((T_Right_Paren, T_Comma));
         if Token /= T_Comma then
            if Token = T_Error then
               Restore_Lexer (State);
               Skip_Declaration (T_Right_Paren);
            end if;
            exit;
         end if;
      end loop;

      Scan_Token (T_Semi_Colon);
      if Token = T_Error then
         Restore_Lexer (State);
         Skip_Declaration (T_Semi_Colon);
         Node := No_Node;
      end if;

      return Node;
   end P_Initializer_Declaration;

   -----------------
   -- P_Interface --
   -----------------

   --  (4) <interface> ::= <interface_dcl>
   --                    | <forward_dcl>

   function P_Interface return Node_Id is
      Identifier  : Node_Id;
      Node        : Node_Id;
      Is_Abstract : Boolean := False;
      Is_Local    : Boolean := False;
      State       : Location;
      Fwd_Loc     : Location;

   begin
      Save_Lexer (State);

      Scan_Token; --  past "abstract", "local" or "interface"
      Fwd_Loc := Token_Location;
      if Token = T_Abstract then
         Is_Abstract := True;
         Scan_Token; --  past "interface"

      elsif Token = T_Local then
         Is_Local := True;
         Scan_Token; --  past "interface"
      end if;

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;

      case Next_Token is
         when T_Semi_Colon =>
            Node := New_Node (K_Forward_Interface_Declaration, Fwd_Loc);
            Bind_Identifier_To_Entity (Identifier, Node);
            Set_Is_Abstract_Interface (Node, Is_Abstract);
            Set_Is_Local_Interface (Node, Is_Local);

         when T_Left_Brace
           | T_Colon =>
            Restore_Lexer (State);
            return P_Interface_Declaration;

         when others =>
            return No_Node;
      end case;

      return Node;
   end P_Interface;

   -----------------------------
   -- P_Interface_Declaration --
   -----------------------------

   --  (5) <interface_dcl> ::= <interface_header> "{" <interface_body> "}"
   --  (7) <interface_header> ::=
   --         [ "abstract" | "local" ] "interface" <identifier>
   --            [ <interface_inheritance_spec> ]
   --  (8) <interface_body> ::=  <export> *
   --  (10) <interface_inheritance_spec>::= ":" <interface_name>
   --                                     { "," <interface_name> } *

   function P_Interface_Declaration return Node_Id is
      Identifier     : Node_Id;
      Node           : Node_Id;
      Interface_Body : List_Id;
      Export         : Node_Id;
      Interface_Spec : List_Id;
      Interface_Name : Node_Id;
      State          : Location;

   begin
      Scan_Token; --  past "abstract" or "interface"
      Node := New_Node (K_Interface_Declaration, Token_Location);

      if Token = T_Abstract then
         Set_Is_Abstract_Interface (Node, True);
         Scan_Token; --  past "interface"

      elsif Token = T_Local then
         Set_Is_Local_Interface (Node, True);
         Scan_Token; --  past "interface"
      end if;

      Identifier := P_Identifier;
      Bind_Identifier_To_Entity (Identifier, Node);

      --  Always create an interface inheritance specifier even if it
      --  is left empty.

      Interface_Spec := New_List (K_Interface_Name_List, Token_Location);
      Set_Interface_Spec (Node, Interface_Spec);

      --  Parse interface inheritance specifier

      if Next_Token = T_Colon then
         Scan_Token; --  past ':'

         loop
            Interface_Name := P_Interface_Name;
            if No (Interface_Name) then
               return No_Node;
            end if;

            Append_Node_To_List (Interface_Name, Interface_Spec);

            exit when Next_Token /= T_Comma;
            Scan_Token; --  past ','
         end loop;
      end if;

      --  Parse interface body

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         return No_Node;
      end if;

      Interface_Body := New_List (K_Interface_Body, Token_Location);
      Set_Interface_Body (Node, Interface_Body);

      loop
         if Next_Token = T_Right_Brace then
            Scan_Token; --  past '}'
            exit;
         end if;

         --  Parse export. Save lexer state to skip interface body on
         --  error.

         Save_Lexer (State);
         Export := P_Export;
         if No (Export) then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit;
         end if;

         Append_Node_To_List (Export, Interface_Body);
      end loop;

      return Node;
   end P_Interface_Declaration;

   ----------------------
   -- P_Interface_Name --
   ----------------------

   --  (11) <interface_name> ::= <scoped_name>

   function P_Interface_Name return Node_Id is
   begin
      return P_Scoped_Name;
   end P_Interface_Name;

   --------------
   -- P_Member --
   --------------

   --  (71) <member> ::= <type_spec> <declarators> ";"
   --  (49) <declarators> ::= <declarator> { "," <declarator> }

   function P_Member return Node_Id is
      Member       : Node_Id;
      Declarators  : List_Id;
      Type_Spec    : Node_Id;
      State        : Location;
   begin
      --  Parse type specifier. Save lexer state to skip declaration
      --  on error.

      Save_Lexer (State);
      Type_Spec := P_Type_Spec;
      if No (Type_Spec) then
         Restore_Lexer (State);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      --  Parse declarators. Save lexer state to skip declarators on
      --  error.

      Save_Lexer (State);
      Declarators := P_Declarator_List;
      if No (Declarators) then
         Restore_Lexer (State);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Member := New_Node (K_Member, Loc (Type_Spec));
      Set_Type_Spec   (Member, Type_Spec);
      Set_Declarators (Member, Declarators);
      Bind_Declarators_To_Entity (Declarators, Member);

      Scan_Token (T_Semi_Colon);
      if Token = T_Error then
         return No_Node;
      end if;

      return Member;
   end P_Member;

   --------------
   -- P_Module --
   --------------

   --  (3) <module> ::= "module" <identifier> "{" <definition> + "}"

   function P_Module return Node_Id is
      Identifier  : Node_Id;
      Node      : Node_Id;
      Definitions : List_Id;
      Definition  : Node_Id;

   begin
      Scan_Token; -- past "module"
      Node := New_Node (K_Module, Token_Location);

      --  Save module declaration location since we may have to reopen
      --  a previous declaration.

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         return No_Node;
      end if;

      Definitions := New_List (K_Definition_List, Token_Location);
      Set_Definitions  (Node, Definitions);

      loop
         Definition := P_Definition;
         if Present (Definition) then
            Append_Node_To_List (Definition, Definitions);
         end if;

         case Next_Token is
            when T_Right_Brace =>
               Scan_Token; --  past '}'
               exit;

            when T_EOF =>
               exit;

            when others =>
               null;
         end case;
      end loop;

      return Node;
   end P_Module;

   --------------------
   -- P_No_Such_Node --
   --------------------

   function P_No_Such_Node return Node_Id is
   begin
      Scan_Token;
      Error_Loc (1) := Token_Location;
      DE ("not implemented");
      return No_Node;
   end P_No_Such_Node;

   -----------------------------
   -- P_Operation_Declaration --
   -----------------------------

   --  (87) <op_dcl> ::= [ <op_attribute> ] <op_type_spec>
   --                                       <identifier> <parameter_dcls>
   --                    [ <raises_expr> ] [ <context_expr> ]
   --
   --  (88) <op_attribute> ::= "oneway"
   --  (89) <op_type_spec> ::= <param_type_spec>
   --                        | "void"
   --
   --  (90) <parameter_dcls> ::= "(" <param_dcl> { "," <param_dcl> } ")"
   --                          | "(" ")"
   --
   --  (91) <param_dcl> ::= <param_attribute> <param_type_spec>
   --                                         <simple_declarator>
   --
   --  (92) <param_attribute> ::= "in"
   --                           | "out"
   --                           | "inout"
   --
   --  (93) <raises_expr> ::= "raises" "(" <scoped_name>
   --                                { "," <scoped_name> } ")"
   --
   --  (94) <context_expr> ::= "context" "(" <string_literal>
   --                                  { "," <string_literal> } ")"

   function P_Operation_Declaration return Node_Id is
      function P_Context_List return List_Id;
      function P_Exception_List return List_Id;

      --------------------
      -- P_Context_List --
      --------------------

      --  (94) <context_expr> ::= "context" "(" <string_literal>
      --                                  { "," <string_literal> } ")"

      function P_Context_List return List_Id is
         Context_List   : List_Id;
         String_Literal : Node_Id;
         State          : Location;
      begin
         Scan_Token; --  past "context"
         Scan_Token (T_Left_Paren);
         if Token = T_Error then
            return No_List;
         end if;

         Context_List := New_List (K_Context_List, Token_Location);
         loop
            --  Parse string literal. Save lexer state to skip
            --  literals on error.

            Save_Lexer (State);
            Scan_Token ((T_String_Literal, T_Wide_String_Literal));
            if Token = T_Error then
               Restore_Lexer (State);
               Skip_Declaration (T_Right_Paren);
               exit;
            end if;

            String_Literal := New_Node (K_Literal, Token_Location);
            Set_Value
              (String_Literal,
               New_String_Value (Value => String_Literal_Value,
                                 Wide  => Is_Wide_Literal_Value));

            Append_Node_To_List (String_Literal, Context_List);

            Save_Lexer (State);
            Scan_Token ((T_Right_Paren, T_Comma));
            if Token /= T_Comma then
               if Token = T_Error then
                  Restore_Lexer (State);
                  Skip_Declaration (T_Right_Paren);
               end if;
               exit;
            end if;
         end loop;

         return Context_List;
      end P_Context_List;

      ----------------------
      -- P_Exception_List --
      ----------------------

      --  (93) <raises_expr> ::= "raises" "(" <scoped_name>
      --                                { "," <scoped_name> } ")"

      function P_Exception_List return List_Id is
         Exception_List : List_Id;
         Scoped_Name    : Node_Id;
         State          : Location;
      begin
         Scan_Token; --  past "raises"
         Scan_Token (T_Left_Paren);
         if Token = T_Error then
            return No_List;
         end if;

         Exception_List := New_List (K_Exception_List, Token_Location);
         loop
            Save_Lexer (State);
            Scoped_Name := P_Scoped_Name;
            if No (Scoped_Name) then
               Restore_Lexer (State);
               Skip_Declaration (T_Right_Paren);
               exit;
            end if;

            Append_Node_To_List (Scoped_Name, Exception_List);

            Save_Lexer (State);
            Scan_Token ((T_Comma, T_Right_Paren));
            if Token /= T_Comma then
               if Token = T_Error then
                  Restore_Lexer (State);
                  Skip_Declaration (T_Right_Paren);
               end if;
               exit;
            end if;
         end loop;

         return Exception_List;
      end P_Exception_List;

      Identifier      : Node_Id;
      Node          : Node_Id;
      Parameter       : Node_Id;
      Parameters      : List_Id;
      Param_Type_Spec : Node_Id;
      Contexts        : List_Id;
      Exceptions      : List_Id;
      State           : Location;

   begin
      Save_Lexer (State);
      Scan_Token;
      Node := New_Node (K_Operation_Declaration, Token_Location);

      if Token = T_Oneway then
         Set_Is_Oneway (Node, True);
         Save_Lexer (State);
         Scan_Token;
      end if;

      if Token = T_Void then
         Param_Type_Spec := Resolve_Base_Type ((1 => T_Void));

      else
         Restore_Lexer (State);

         Param_Type_Spec := P_Simple_Type_Spec;
         if No (Param_Type_Spec) then
            return No_Node;
         end if;

         if not Is_Param_Type_Spec (Param_Type_Spec) then
            Error_Loc (1) := Loc (Param_Type_Spec);
            DE ("incorrect param type spec");
            return No_Node;
         end if;
      end if;
      Set_Type_Spec (Node, Param_Type_Spec);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         return No_Node;
      end if;

      Parameters := New_List (K_Parameter_List, Token_Location);
      Set_Parameters   (Node, Parameters);

      if Next_Token = T_Right_Paren then
         Scan_Token; --  past ')'

      else
         Save_Lexer (State);
         loop
            Parameter := P_Parameter_Declaration;
            if No (Parameter) then
               Restore_Lexer (State);
               Skip_Declaration (T_Right_Paren);
               exit;
            end if;

            Append_Node_To_List (Parameter, Parameters);

            Save_Lexer (State);
            Scan_Token ((T_Right_Paren, T_Comma));
            if Token /= T_Comma then
               if Token = T_Error then
                  Restore_Lexer (State);
                  Skip_Declaration (T_Right_Paren);
               end if;
               exit;
            end if;
         end loop;
      end if;

      if Next_Token = T_Raises then
         Exceptions := P_Exception_List;
         if Exceptions = No_List then
            return No_Node;
         end if;
         Set_Exceptions (Node, Exceptions);
      end if;

      if Next_Token = T_Context then
         Contexts := P_Context_List;
         if Contexts = No_List then
            return No_Node;
         end if;
         Set_Contexts (Node, Contexts);
      end if;

      return Node;
   end P_Operation_Declaration;

   -----------------------------
   -- P_Parameter_Declaration --
   -----------------------------

   --  (91) <param_dcl> ::= <param_attribute> <param_type_spec>
   --                                         <simple_declarator>
   --
   --  (92) <param_attribute> ::= "in"
   --                           | "out"
   --                           | "inout"

   function P_Parameter_Declaration return Node_Id is
      Param_Declaration : Node_Id;
      Param_Declarator  : Node_Id;
      Param_Type_Spec   : Node_Id;
      Param_Mode        : Mode_Id;
      Param_Location    : Location;

   begin
      Scan_Token ((T_In, T_Inout, T_Out));
      Param_Location := Token_Location;
      if Token = T_Error then
         return No_Node;
      end if;
      Param_Mode := Parameter_Mode (Token);

      Param_Type_Spec := P_Simple_Type_Spec;
      if not Is_Param_Type_Spec (Param_Type_Spec) then
         Error_Loc (1) := Loc (Param_Type_Spec);
         DE ("incorrect param type spec");
         return No_Node;
      end if;

      Param_Declarator := P_Simple_Declarator;
      if No (Param_Declarator) then
         return No_Node;
      end if;

      Param_Declaration :=
        New_Node (K_Parameter_Declaration, Param_Location);
      Set_Parameter_Mode (Param_Declaration, Param_Mode);
      Set_Type_Spec      (Param_Declaration, Param_Type_Spec);
      Set_Declarator     (Param_Declaration, Param_Declarator);
      Bind_Declarator_To_Entity (Param_Declarator, Param_Declaration);

      return Param_Declaration;
   end P_Parameter_Declaration;

   -------------------
   -- P_Scoped_Name --
   -------------------

   --  (12) <scoped_name> ::= <identifier>
   --                       | "::" <identifier>
   --                       | <scoped_name> "::" <identifier>

   function P_Scoped_Name return Node_Id is
      Scoped_Name : Node_Id := No_Node;
      Parent      : Node_Id := No_Node;
      Identifier  : Node_Id;
      Scope_Depth : Int;

   begin
      --  Scoped name starts with a '::'

      if Next_Token = T_Colon_Colon then
         Scan_Token;  --  past '::'
         Identifier  := Make_Identifier
           (Token_Location, No_Name, No_Node, No_Node);
         Scoped_Name := New_Node
           (K_Scoped_Name, Token_Location);
         Bind_Identifier_To_Entity
           (Identifier, Scoped_Name);
      end if;

      --  start loop with an identifier

      loop
         Identifier := P_Identifier;
         if No (Identifier) then
            return No_Node;
         end if;

         Parent      := Scoped_Name;
         Scoped_Name := New_Node (K_Scoped_Name, Token_Location);
         Bind_Identifier_To_Entity  (Identifier, Scoped_Name);
         Set_Parent_Entity (Scoped_Name, Parent);

         exit when Next_Token /= T_Colon_Colon;
         Scan_Token; --  past '::'
      end loop;

      Parent      := Parent_Entity (Scoped_Name);
      Scope_Depth := Depth (Scoped_Name);
      while Present (Parent) loop
         Scope_Depth := Scope_Depth + 1;
         Set_Depth (Parent, Scope_Depth);
         Parent := Parent_Entity (Parent);
      end loop;

      return Scoped_Name;
   end P_Scoped_Name;

   ---------------------
   -- P_Sequence_Type --
   ---------------------

   --  (80) <sequence_type> ::= "sequence" "<" <simple_type_spec> ","
   --                                          <positive_int_const> ">"
   --                         | "sequence" "<" <simple_type_spec> ">"

   function P_Sequence_Type return Node_Id is
      Node          : Node_Id;
      Seq_Type_Spec : Node_Id;
      Seq_Level     : Natural;
      Size          : Node_Id;

   begin
      Scan_Token; --  past "sequence"
      Node := New_Node (K_Sequence_Type, Token_Location);

      Scan_Token (T_Less);
      if Token = T_Error then
         return No_Node;
      end if;
      Sequencing_Level := Sequencing_Level + 1;
      Seq_Level := Sequencing_Level;

      Seq_Type_Spec := P_Type_Spec;
      if No (Seq_Type_Spec) then
         return No_Node;
      end if;
      Set_Type_Spec (Node, Seq_Type_Spec);

      if Seq_Level > Sequencing_Level then
         return Node;
      end if;

      if Sequencing_Level > 1 then
         Scan_Token ((T_Comma, T_Greater, T_Greater_Greater));
      else
         Scan_Token ((T_Comma, T_Greater));
      end if;

      if Token = T_Error then
         return No_Node;
      end if;

      if Token = T_Comma then
         Scan_Token (T_Integer_Literal);
         if Token = T_Error then
            return No_Node;
         end if;

         Size := New_Node (K_Integer_Literal, Token_Location);
         Set_Value
           (Size,
            New_Integer_Value (Value => Integer_Literal_Value,
                               Sign  => 1,
                               Base  => Integer_Literal_Base));

         if Sequencing_Level > 1 then
            Scan_Token ((T_Greater, T_Greater_Greater));
         else
            Scan_Token (T_Greater);
         end if;

         if Token = T_Error then
            return No_Node;
         end if;

      --  No max size means no size

      else
         Size := No_Node;
      end if;
      Set_Max_Size (Node, Size);

      if Token = T_Greater_Greater then
         Sequencing_Level := Sequencing_Level - 2;
      else
         Sequencing_Level := Sequencing_Level - 1;
      end if;

      return Node;
   end P_Sequence_Type;

   -------------------------
   -- P_Simple_Declarator --
   -------------------------

   --  (51) <simple_declarator> ::= <identifier>

   function P_Simple_Declarator return Node_Id is
      Identifier : Node_Id;
      Node     : Node_Id;

   begin
      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;

      Node := New_Node (K_Simple_Declarator, Loc (Identifier));
      Bind_Identifier_To_Entity (Identifier, Node);

      return Node;
   end P_Simple_Declarator;

   ------------------------
   -- P_Simple_Type_Spec --
   ------------------------

   --  (45) <simple_type_spec> ::= <base_type_spec>
   --                            | <template_type_spec>
   --                            | <scoped_name>
   --
   --  (46) <base_type_spec> ::= <floating_pt_type>
   --                          | <integer_type>
   --                          | <char_type>
   --                          | <wide_char_type>
   --                          | <boolean_type>
   --                          | <octet_type>
   --                          | <any_type>
   --                          | <object_type>
   --                          | <value_base_type>
   --
   --  (47) <template_type_spec> ::= <sequence_type>
   --                              | <string_type>
   --                              | <wide_string_type>
   --                              | <fixed_pt_type>

   function P_Simple_Type_Spec return Node_Id is
      List  : Token_List_Type (1 .. 3) := (others => T_Error);
      Size  : Natural := 0;
      Next  : Token_Type;

      procedure Push_Base_Type_Token (T : Token_Type);
      --  Push token in the list above. This token is either T_Float,
      --  T_Double, T_Short, T_Long, T_Unsigned, T_Char, T_Wchar or T_Octet.

      function Resolve_Base_Type return Node_Id;

      --------------------------
      -- Push_Base_Type_Token --
      --------------------------

      procedure Push_Base_Type_Token (T : Token_Type) is
      begin
         Size := Size + 1;
         List (Size) := T;
      end Push_Base_Type_Token;

      -----------------------
      -- Resolve_Base_Type --
      -----------------------

      function Resolve_Base_Type return Node_Id is
      begin
         return Resolve_Base_Type (List (1 .. Size));
      end Resolve_Base_Type;

   begin
      Size := 0;
      Next := Next_Token;
      Push_Base_Type_Token (Next);
      case Next is
         when T_Long =>
            Scan_Token; -- skip long
            Next := Next_Token;
            if Next = T_Double
              or else Next = T_Long
            then
               Scan_Token;
               Push_Base_Type_Token (Next);
            end if;
            return Resolve_Base_Type;

         when T_Short
           | T_Float
           | T_Double
           | T_Char
           | T_Wchar
           | T_Boolean
           | T_Octet
           | T_Any
           | T_Object
           | T_Value_Base =>
            Scan_Token;
            return Resolve_Base_Type;

         when T_Unsigned =>
            Scan_Token; --  skip unsigned
            Scan_Token ((T_Short, T_Long));
            Push_Base_Type_Token (Token);
            if Token = T_Error then
               return No_Node;

            elsif Token = T_Long then
               Next := Next_Token;
               if Next = T_Long then
                  Scan_Token; -- skip long
                  Push_Base_Type_Token (Next);
               end if;
            end if;
            return Resolve_Base_Type;

         when T_String
           | T_Wstring =>
            return P_String_Type;

         when T_Fixed =>
            return P_Fixed_Point_Type;

         when T_Identifier
           | T_Colon_Colon =>
            return P_Scoped_Name;

         when T_Sequence =>
            return P_Sequence_Type;

         when others =>
            Scan_Token;
            Unexpected_Token (Token, "type specifier");
            return No_Node;
      end case;
   end P_Simple_Type_Spec;

   ---------------------
   -- P_Specification --
   ---------------------

   --  (1) <specification> ::= <definition> +

   function P_Specification return Node_Id is
      Definitions : List_Id;
      Definition  : Node_Id;
      Identifier  : Node_Id;

   begin
      Identifier :=
        Make_Identifier (Token_Location, IDL_Spec_Name, No_Node, No_Node);
      Specification := New_Node (K_Specification, Token_Location);
      Bind_Identifier_To_Entity (Identifier, Specification);
      Definitions   := New_List (K_Definition_List, Token_Location);
      Set_Definitions (Specification, Definitions);

      loop
         Definition := P_Definition;
         if Present (Definition) then
            Append_Node_To_List (Definition, Definitions);
         end if;
         exit when Next_Token = T_EOF;
      end loop;

      return Specification;
   end P_Specification;

   --------------------
   -- P_State_Member --
   --------------------

   --  (22) <state_member> ::= ( "public" | "private" )
   --                          <type_spec> <declarators> ";"
   --  (49) <declarators> ::= <declarator> { "," <declarator> }

   function P_State_Member return Node_Id is
      Declarators  : List_Id;
      Type_Spec    : Node_Id;
      State_Member : Node_Id := No_Node;
      Is_Public    : Boolean := False;
      State        : Location;
   begin
      State := Token_Location;
      Scan_Token; --  past "public" or "private"
      if Token = T_Public then
         Is_Public := True;
      end if;

      Type_Spec := P_Type_Spec;
      if No (Type_Spec) then
         return No_Node;
      end if;

      Declarators := P_Declarator_List;
      if Is_Empty (Declarators) then
         return No_Node;
      end if;

      Scan_Token (T_Semi_Colon);
      if Token = T_Error then
         Restore_Lexer (State);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      State_Member := New_Node (K_State_Member, State);
      Set_Type_Spec   (State_Member, Type_Spec);
      Set_Is_Public   (State_Member, Is_Public);
      Set_Declarators (State_Member, Declarators);

      return State_Member;
   end P_State_Member;

   -------------------
   -- P_String_Type --
   -------------------

   --  (81) <string_type> ::= "string" "<" <positive_int_const> ">"
   --                       | "string"
   --
   --  (82) <wide_string_type> ::= "wstring" "<" <positive_int_const> ">"
   --                            | "wstring"

   function P_String_Type return Node_Id is
      Node     : Node_Id;
      Size     : Node_Id;
      Prev_Loc : Location;
      Prev_Tok : Token_Type;
   begin
      Scan_Token;
      Prev_Tok := Token;
      Prev_Loc := Token_Location;

      if Next_Token /= T_Less then
         return Resolve_Base_Type ((1 => Prev_Tok));
      end if;

      Scan_Token; --  past '<'
      Scan_Token (T_Integer_Literal);
      if Token = T_Error then
         return No_Node;
      end if;

      if Prev_Tok = T_String then
         Node := New_Node (K_String_Type, Prev_Loc);
      else
         Node := New_Node (K_Wide_String_Type, Prev_Loc);
      end if;

      Size := New_Node (K_Integer_Literal, Token_Location);
      Set_Value
        (Size,
         New_Integer_Value (Value => Integer_Literal_Value,
                            Sign  => 1,
                            Base  => Integer_Literal_Base));
      Set_Max_Size (Node, Size);

      Scan_Token (T_Greater);
      if Token = T_Error then
         return No_Node;
      end if;

      return Node;
   end P_String_Type;

   ----------------------
   -- P_Structure_Type --
   ----------------------

   --  (69) <struct_type> ::= "struct" <identifier> "{" <member_list> "}"
   --  (70) <member_list> ::= <member> +

   function P_Structure_Type return Node_Id is
      Identifier  : Node_Id;
      Node        : Node_Id;
      Members     : List_Id;
      Member      : Node_Id;
      State       : Location;

   begin
      Scan_Token; --  past "struct";
      Node := New_Node (K_Structure_Type, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Members := New_List (K_Member_List, Token_Location);
      Set_Members      (Node, Members);

      Scan_Token; --  past '{'

      loop
         Save_Lexer (State);
         Member := P_Member;
         if No (Member) then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit;
         end if;

         Append_Node_To_List (Member, Members);

         if Next_Token = T_Right_Brace then
            Scan_Token;
            exit;
         end if;
      end loop;

      return Node;
   end P_Structure_Type;

   ------------------------
   -- P_Type_Declaration --
   ------------------------

   --  (42) <type_dcl> ::= "typedef" <type_declarator>
   --                    | <struct_type>
   --                    | <union_type>
   --                    | <enum_type>
   --                    | "native" <simple_declarator>
   --                    | <constr_forward_decl>
   --  (43) <type_declarator> ::= <type_spec> <declarators>

   function P_Type_Declaration return Node_Id is
      Identifier  : Node_Id;
      Node        : Node_Id := No_Node;
      Type_Spec   : Node_Id;
      Declarator  : Node_Id;
      Declarators : List_Id;
      State       : Location;

   begin
      Save_Lexer (State);
      Scan_Token;

      case Token is
         when T_Typedef =>
            Type_Spec := P_Type_Spec;
            if No (Type_Spec) then
               return No_Node;
            end if;

            Declarators := P_Declarator_List;
            if Is_Empty (Declarators) then
               return No_Node;
            end if;
            Node := New_Node (K_Type_Declaration, State);
            Set_Type_Spec   (Node, Type_Spec);
            Set_Declarators (Node, Declarators);
            Bind_Declarators_To_Entity (Declarators, Node);

         when T_Native =>

            Declarator := P_Simple_Declarator;
            if No (Declarator) then
               return No_Node;
            end if;

            Node := New_Node (K_Native_Type, State);
            Set_Declarator (Node, Declarator);
            Bind_Declarator_To_Entity (Declarator, Node);

         when T_Struct =>
            Identifier := P_Identifier;
            if No (Identifier) then
               return No_Node;
            end if;

            if Next_Token = T_Semi_Colon then
               Node := New_Node (K_Forward_Structure_Type, State);
               Bind_Identifier_To_Entity (Identifier, Node);

            else
               Restore_Lexer (State);
               return P_Structure_Type;
            end if;

         when T_Union =>
            Identifier := P_Identifier;
            if No (Identifier) then
               return No_Node;
            end if;

            if Next_Token = T_Semi_Colon then
               Node := New_Node (K_Forward_Union_Type, State);
               Bind_Identifier_To_Entity (Identifier, Node);

            else
               Restore_Lexer (State);
               return P_Union_Type;
            end if;

         when T_Enum =>
            Restore_Lexer (State);
            return P_Enumeration_Type;

         when others =>
            return No_Node;
      end case;

      return Node;
   end P_Type_Declaration;

   -----------------
   -- P_Type_Spec --
   -----------------

   --  (44) <type_spec> ::= <simple_type_spec>
   --                     | <constr_type_spec>
   --
   --  (45) <simple_type_spec> ::= <base_type_spec>
   --                            | <template_type_spec>
   --                            | <scoped_name>
   --
   --  (46) <base_type_spec> ::= <floating_pt_type>
   --                          | <integer_type>
   --                          | <char_type>
   --                          | <wide_char_type>
   --                          | <boolean_type>
   --                          | <octet_type>
   --                          | <any_type>
   --                          | <object_type>
   --                          | <value_base_type>
   --
   --  (47) <template_type_spec> ::= <sequence_type>
   --                              | <string_type>
   --                              | <wide_string_type>
   --                              | <fixed_pt_type>
   --
   --  (48) <constr_type_spec> ::= <struct_type>
   --                            | <union_type>
   --                            | <enum_type>

   function P_Type_Spec return Node_Id is
   begin
      case Next_Token is
         when T_Struct =>
            return P_Structure_Type;

         when T_Enum =>
            return P_Enumeration_Type;

         when T_Union =>
            return P_Union_Type;

         when others =>
            return P_Simple_Type_Spec;
      end case;
   end P_Type_Spec;

   ------------------
   -- P_Union_Type --
   ------------------

   --  (72) <union_type> ::= "union" <identifier> "switch"
   --                        "(" <switch_type_spec> ")"
   --                        "{" <switch_body> "}"
   --
   --  (73) <switch_type_spec> ::= <integer_type>
   --                            | <char_type>
   --                            | <boolean_type>
   --                            | <enum_type>
   --                            | <scoped_name>
   --
   --  (74) <switch_body> ::= <case> +
   --  (75) <case> ::= <case_label> + <element_spec> ";"
   --  (76) <case_label> ::= "case" <const_exp> ":"
   --                      | "default" ":"
   --
   --  (77) <element_spec> ::= <type_spec> <declarator>

   function P_Union_Type return Node_Id is
      function Is_Switch_Type_Spec (K : Node_Kind) return Boolean;

      --------------------
      -- Is_Switch_Spec --
      --------------------

      --  (73) <switch_type_spec> ::= <integer_type>
      --                            | <char_type>
      --                            | <boolean_type>
      --                            | <enum_type>
      --                            | <scoped_name>

      function Is_Switch_Type_Spec (K : Node_Kind) return Boolean is
      begin
         case K is
            when K_Short
              | K_Long
              | K_Long_Long
              | K_Unsigned_Short
              | K_Unsigned_Long
              | K_Unsigned_Long_Long
              | K_Char
              | K_Boolean
              | K_Enumeration_Type
              | K_Scoped_Name =>
               return True;

            when others =>
               return False;
         end case;
      end Is_Switch_Type_Spec;

      Identifier         : Node_Id;
      Node               : Node_Id;
      Switch_Type_Spec   : Node_Id;
      Switch_Type_Body   : List_Id;
      Switch_Alt_Decl    : Node_Id;
      Element_Type_Spec  : Node_Id;
      Element_Declarator : Node_Id;
      Element            : Node_Id;
      Case_Labels        : List_Id;
      Case_Label         : Node_Id;
      Expression         : Node_Id;
      State              : Location;

   begin

      --  (72) <union_type> ::= "union" <identifier> "switch"
      --                        "(" <switch_type_spec> ")"
      --                        "{" <switch_body> "}"

      Scan_Token; --  past "union"
      Node := New_Node (K_Union_Type, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Scan_Token (T_Switch);
      if Token = T_Error then
         return No_Node;
      end if;

      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         return No_Node;
      end if;

      --  (73) <switch_type_spec> ::= <integer_type>
      --                            | <char_type>
      --                            | <boolean_type>
      --                            | <enum_type>
      --                            | <scoped_name>

      Save_Lexer (State);
      Switch_Type_Spec := P_Type_Spec;
      if Present (Switch_Type_Spec)
        and then not Is_Switch_Type_Spec (Kind (Switch_Type_Spec))
      then
         Error_Loc (1) := Loc (Switch_Type_Spec);
         DE ("unexpected switch type spec");
         Switch_Type_Spec := No_Node;
      end if;

      if No (Switch_Type_Spec) then
         Restore_Lexer (State);
         Skip_Declaration (T_Right_Paren);

      else
         Save_Lexer (State);
         Scan_Token (T_Right_Paren);
         if Token = T_Error then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Paren);
         end if;
      end if;
      Set_Switch_Type_Spec (Node, Switch_Type_Spec);

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         return No_Node;
      end if;

      Switch_Type_Body := New_List (K_Switch_Type_Body, Token_Location);
      Set_Switch_Type_Body (Node, Switch_Type_Body);

      Switch_Alternative_Declaration :
      loop
         Save_Lexer (State);
         Scan_Token ((T_Default, T_Case));
         if Token = T_Error then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit Switch_Alternative_Declaration;
         end if;

         Switch_Alt_Decl := New_Node (K_Switch_Alternative, Token_Location);
         Case_Labels := New_List (K_Case_Label_List, Token_Location);
         Set_Labels (Switch_Alt_Decl, Case_Labels);

         --  (74) <switch_body> ::= <case> +

         Case_Label_List :
         loop
            Save_Lexer (State);
            Case_Label := New_Node (K_Case_Label, Token_Location);
            Set_Declaration (Case_Label, Switch_Alt_Decl);

            --  (75) <case> ::= <case_label> + <element_spec> ";"

            Expression := No_Node;
            if Token = T_Case then
               Expression := P_Constant_Expression;
               if No (Expression) then
                  Restore_Lexer (State);
                  Skip_Declaration (T_Right_Brace);
                  exit Switch_Alternative_Declaration;
               end if;
            end if;

            Scan_Token (T_Colon);
            if Token = T_Error then
               Restore_Lexer (State);
               Skip_Declaration (T_Right_Brace);
               exit Switch_Alternative_Declaration;
            end if;

            Set_Expression (Case_Label, Expression);
            Append_Node_To_List (Case_Label, Case_Labels);

            --  (76) <case_label> ::= "case" <const_exp> ":"
            --                      | "default" ":"

            case Next_Token is
               when T_Case
                 | T_Default =>
                  Scan_Token;

               when others =>
                  exit Case_Label_List;
            end case;
         end loop Case_Label_List;

         --  (77) <element_spec> ::= <type_spec> <declarator>

         Save_Lexer (State);
         Element_Type_Spec := P_Type_Spec;
         if No (Element_Type_Spec) then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit Switch_Alternative_Declaration;
         end if;

         Save_Lexer (State);
         Element_Declarator := P_Declarator;
         if No (Element_Declarator) then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit Switch_Alternative_Declaration;
         end if;

         --  Assemble Element and append it to Switch Alternative

         Element := New_Node (K_Element, Loc (Element_Type_Spec));
         Set_Type_Spec  (Element, Element_Type_Spec);
         Set_Declarator (Element, Element_Declarator);
         Bind_Declarator_To_Entity (Element_Declarator, Element);

         Set_Element     (Switch_Alt_Decl, Element);
         Set_Declaration (Switch_Alt_Decl, Node);

         Append_Node_To_List (Switch_Alt_Decl, Switch_Type_Body);

         Save_Lexer (State);
         Scan_Token (T_Semi_Colon);
         if Token = T_Error then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit Switch_Alternative_Declaration;
         end if;

         if Next_Token = T_Right_Brace then
            Scan_Token; --  past '}'
            exit Switch_Alternative_Declaration;
         end if;
      end loop Switch_Alternative_Declaration;

      return Node;
   end P_Union_Type;

   -------------
   -- P_Value --
   -------------

   --  (13) <value> ::= <value_dcl>
   --                 | <value_abs_dcl>
   --                 | <value_box_dcl>
   --                 | <value_forward_dcl>)

   function P_Value return Node_Id is
      State      : Location;
      Value_Abs  : Boolean := False;

   begin
      Save_Lexer (State);

      Scan_Token; --  past "abstract" or "custom" or "valuetype"
      if Token = T_Abstract then
         Value_Abs := True;
         Scan_Token; --  past "valuetype"

      elsif Token = T_Custom then
         Scan_Token; --  past "valuetype"
      end if;

      if No (P_Identifier) then
         return No_Node;
      end if;

      Scan_Token;
      case Token is
         when T_Semi_Colon =>
            Restore_Lexer (State);
            return P_Value_Forward_Declaration;

         when T_Custom =>
            Restore_Lexer (State);
            return P_Value_Declaration;

         when T_Colon | T_Left_Brace | T_Supports =>
            Restore_Lexer (State);
            if Value_Abs then
               return P_Value_Abstract_Declaration;
            else
               return P_Value_Declaration;
            end if;

         when others =>
            Restore_Lexer (State);
            return P_Value_Box_Declaration;
      end case;
   end P_Value;

   ----------------------------------
   -- P_Value_Abstract_Declaration --
   ----------------------------------

   --  (16) <value_abs_dcl> ::= "abstract" "valuetype" <identifier>
   --                         [ <value_inheritance_spec> ] "{" <export>* "}"

   function P_Value_Abstract_Declaration return Node_Id is
      Identifier  : Node_Id;
      Node      : Node_Id;
      Value_Spec  : Node_Id;
      Value_Body  : List_Id;
      Export      : Node_Id;
      State       : Location;

   begin
      Scan_Token; --  past "abstract"
      Node := New_Node (K_Abstract_Value_Declaration, Token_Location);

      Scan_Token; --  past "valuetype"

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Value_Spec := P_Value_Spec;
      Set_Value_Spec (Node, Value_Spec);

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         return No_Node;
      end if;

      Value_Body := New_List (K_Value_Body, Token_Location);
      Set_Value_Body   (Node, Value_Body);

      if Next_Token = T_Right_Brace then
         Scan_Token;

      else
         loop
            Save_Lexer (State);
            Export := P_Export;
            if No (Export) then
               Restore_Lexer (State);
               Skip_Declaration (T_Right_Brace);
               exit;
            end if;

            Append_Node_To_List (Export, Value_Body);

            if Next_Token = T_Right_Brace then
               Scan_Token;
               exit;
            end if;
         end loop;
      end if;

      return Node;
   end P_Value_Abstract_Declaration;

   -----------------------------
   -- P_Value_Box_Declaration --
   -----------------------------

   --  (15) <value_box_dcl> ::= "valuetype" <identifier> <type_spec>

   function P_Value_Box_Declaration return Node_Id is
      Identifier : Node_Id;
      Node     : Node_Id;
      Type_Spec  : Node_Id;

   begin
      Scan_Token; --  past "valuetype"
      Node := New_Node (K_Value_Box_Declaration, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Type_Spec := P_Type_Spec;
      if No (Type_Spec) then
         return No_Node;
      end if;
      Set_Type_Spec (Node, Type_Spec);

      return Node;
   end P_Value_Box_Declaration;

   -------------------------
   -- P_Value_Declaration --
   -------------------------

   --  (17) <value_dcl> ::= <value_header> "{"  <value_element>* "}"
   --  (18) <value_header> ::= [" custom" ] "valuetype" <identifier>
   --                          [ <value_inheritance_spec> ]
   --
   --  (20) <value_name> ::= <scoped_name>
   --  (21) <value_element> ::= <export> |  <state_member> | <init_dcl>

   function P_Value_Declaration return Node_Id is
      Identifier    : Node_Id;
      Node        : Node_Id;
      Value_Spec    : Node_Id;
      Value_Body    : List_Id;
      Value_Element : Node_Id;
      State         : Location;

   begin
      Scan_Token; --  past "custom" or "valuetype"
      Node := New_Node (K_Value_Declaration, Token_Location);

      if Token = T_Custom then
         Set_Is_Custom (Node, True);

         Scan_Token (T_Value_Type);
         if Token = T_Error then
            return No_Node;
         end if;
      end if;

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      Value_Spec := P_Value_Spec;
      Set_Value_Spec (Node, Value_Spec);

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         return No_Node;
      end if;

      Value_Body := New_List (K_Value_Body, Token_Location);
      Set_Value_Body   (Node, Value_Body);

      loop
         Save_Lexer (State);
         case Next_Token is
            when T_Factory =>
               Value_Element := P_Initializer_Declaration;

            when T_Public | T_Private =>
               Value_Element := P_State_Member;

            when T_Right_Brace =>
               Scan_Token;  -- past "}"
               exit;

            when others =>
               Value_Element := P_Export;
         end case;

         if No (Value_Element) then
            Restore_Lexer (State);
            Skip_Declaration (T_Right_Brace);
            exit;
         end if;

         Append_Node_To_List (Value_Element, Value_Body);
      end loop;

      return Node;
   end P_Value_Declaration;

   ---------------------------------
   -- P_Value_Forward_Declaration --
   ---------------------------------

   --  (14) <value_forward_dcl> ::= [ "abstract" ] "valuetype" <identifier>

   function P_Value_Forward_Declaration return Node_Id is
      Identifier : Node_Id;
      Node       : Node_Id;

   begin
      Scan_Token; --  past "valuetype" or "abstract"
      Node := New_Node (K_Value_Forward_Declaration, Token_Location);

      if Token = T_Abstract then
         Set_Is_Abstract_Value (Node, True);
         Scan_Token (T_Value_Type);
      end if;

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Bind_Identifier_To_Entity (Identifier, Node);

      return Node;
   end P_Value_Forward_Declaration;

   ------------------
   -- P_Value_Spec --
   ------------------

   --  (19) <value_inheritance_spec> ::=
   --            [ ":" [ "truncatable" ] <value_name>
   --                              { "," <value_name> }* ]
   --            [ "supports" <interface_name>
   --                   { "," <interface_name> }* ]

   function P_Value_Spec return Node_Id is
      Value_Spec      : Node_Id := No_Node;
      Value_Names     : List_Id;
      Interface_Names : List_Id;
      Scoped_Name     : Node_Id;
      Interface_Name  : Node_Id;

   begin
      Value_Spec := New_Node (K_Value_Spec, Token_Location);

      if Next_Token = T_Colon then
         Scan_Token; --  past ":"
         if Next_Token = T_Truncatable then
            Scan_Token; --  past "truncatable"
            Set_Is_Truncatable (Value_Spec, True);
         end if;

         Value_Names := New_List (K_Value_Name_List, Token_Location);
         Set_Value_Names (Value_Spec, Value_Names);

         loop
            Scoped_Name := P_Scoped_Name;
            if No (Scoped_Name) then
               return No_Node;
            end if;

            Append_Node_To_List (Scoped_Name, Value_Names);

            exit when Next_Token /= T_Comma;
            Scan_Token; --  past ','
         end loop;
      end if;

      if Next_Token = T_Supports then
         Scan_Token;  --  past "supports"
         Interface_Names := New_List (K_Interface_Name_List, Token_Location);
         Set_Interface_Names (Value_Spec, Interface_Names);

         loop
            Interface_Name := P_Interface_Name;
            if No (Interface_Name) then
               return No_Node;
            end if;

            Append_Node_To_List (Interface_Name, Interface_Names);

            exit when Next_Token /= T_Comma;
            Scan_Token;  --  past ','
         end loop;
      end if;

      return Value_Spec;
   end P_Value_Spec;

   -------------
   -- Process --
   -------------

   procedure Process (IDL_Spec : out Node_Id) is
   begin
      --  (53) <floating_pt_type> ::= "float"
      --                            | "double"
      --                            | "long" "double"

      Declare_Base_Type ((1 => T_Float), K_Float);
      Declare_Base_Type ((1 => T_Double), K_Double);
      Declare_Base_Type ((T_Long, T_Double), K_Long_Double);

      --  (54) <integer_type> ::= <signed_int>
      --                        | <unsigned_int>
      --
      --  (55) <signed_int> ::= <signed_short_int>
      --                      | <signed_long_int>
      --                      | <signed_longlong_int>
      --
      --  (56) <signed_short_int> ::= "short"
      --  (57) <signed_long_int> ::= "long"
      --  (58) <signed_longlong_int> ::= "long" "long"
      --  (59) <unsigned_int> ::= <unsigned_short_int>
      --                        | <unsigned_long_int>
      --                        | <unsigned_longlong_int>
      --
      --  (60) <unsigned_short_int> ::= "unsigned" "short"
      --  (61) <unsigned_long_int> ::= "unsigned" "long"
      --  (62) <unsigned_longlong_int> ::= "unsigned" "long" "long"

      Declare_Base_Type ((1 => T_Short), K_Short);
      Declare_Base_Type ((1 => T_Long), K_Long);
      Declare_Base_Type ((T_Long, T_Long), K_Long_Long);
      Declare_Base_Type ((T_Unsigned, T_Short), K_Unsigned_Short);
      Declare_Base_Type ((T_Unsigned, T_Long), K_Unsigned_Long);
      Declare_Base_Type ((T_Unsigned, T_Long, T_Long), K_Unsigned_Long_Long);

      --  (63) <char_type> ::= "char"
      --  (64) <wide_char_type> ::= "wchar"

      Declare_Base_Type ((1 => T_Char), K_Char);
      Declare_Base_Type ((1 => T_Wchar), K_Wide_Char);

      Declare_Base_Type ((1 => T_String), K_String);
      Declare_Base_Type ((1 => T_Wstring), K_Wide_String);

      --  (65) <boolean_type> ::= "boolean"
      --  (66) <octet_type> ::= "octet"
      --  (67) <any_type> ::= "any"
      --  (68) <object_type> ::= "Object"

      Declare_Base_Type ((1 => T_Boolean), K_Boolean);
      Declare_Base_Type ((1 => T_Octet), K_Octet);
      Declare_Base_Type ((1 => T_Any), K_Any);
      Declare_Base_Type ((1 => T_Object), K_Object);
      Declare_Base_Type ((1 => T_Value_Base), K_Value_Base);

      --  89) <op_type_spec> ::= <param_type_spec>
      --                       | "void"

      Declare_Base_Type ((1 => T_Void), K_Void);

      IDL_Spec := P_Specification;
   end Process;

   -----------------------
   -- Resolve_Base_Type --
   -----------------------

   function Resolve_Base_Type (L : Token_List_Type) return Node_Id is
   begin
      Set_Str_To_Name_Buffer (Image (L (L'First)));
      for I in L'First + 1 .. L'Last loop
         Add_Char_To_Name_Buffer (' ');
         Add_Str_To_Name_Buffer  (Image (L (I)));
      end loop;
      return Node_Id (Get_Name_Table_Info (Name_Find));
   end Resolve_Base_Type;

end Parser;
