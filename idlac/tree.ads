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
--  59 Temple Place - Suite 330,  Boston,
--  MA 02111-1307, USA.
--

with Types; use Types;

package Tree is

   -------------------------------------
   --  Some usefull type and methods  --
   -------------------------------------

   --  Different string types to be used for identifiers and versions
   type Identifier is new String_Cacc;
   type Version_Spec is new String_Cacc;

   --  Simple list type for a node list.
   --  The Nil atom is Nil_List.
   type Node_List is private;
   Nil_List : constant Node_List;

   --  Simple way to iterate over a node_list.
   --  NODE_ITERATOR is a type representing an iterator, which must
   --  be initialiazed by INIT.
   --  End of list is detected by IS_END.
   --  Until the end of list is reached, the node can be extracted
   --  with GET_NODE and the iterator can be incremented with NEXT.
   --  Therefore, usual way to use an iterator is:
   --  declare
   --    it: node_iterator;
   --    node: n_root_acc;
   --  begin
   --    init (it, rep.contents);
   --    while not is_end (it) loop
   --      node := get_node (it);
   --      ...
   --      next (it);
   --    end loop;
   type Node_Iterator is limited private;
   procedure Init (It : out Node_Iterator; List : Node_List);
   function Get_Node (It : Node_Iterator) return N_Root_Acc;
   procedure Next (It : in out Node_Iterator);
   function Is_End (It : Node_Iterator) return Boolean;

   --  Appends a node at the end of a list.
   procedure Append_Node (List : in out Node_List; Node : N_Root_Acc);

   --  Look whether node is in list or not
   function Is_In_List (List : Node_List; Node : N_Root_Acc) return Boolean;


   -------------------
   --  The idl tree --
   -------------------

   --  An idl is represented as a tree, which is inspired by the
   --  interface repository (ada IR).
   --  However, contrary to the IR, it doesn't use multiple
   --  inheritance.

   --  Top of the repository.
   --  A repository is a list of elements.
   type N_Repository is new N_Scope with record
      Contents : Node_List;
   end record;
   function Get_Kind (N : N_Repository) return Node_Kind;
   type N_Repository_Acc is access all N_Repository;

   --  A module
   type N_Module is new N_Scope with record
      Contents : Node_List;
   end record;
   type N_Module_Acc is access all N_Module;
   function Get_Kind (N : N_Module) return Node_Kind;

   --  An interface.
   type N_Forward_Interface;
   type N_Forward_Interface_Acc is access all N_Forward_Interface;

   type N_Interface is new N_Scope with record
      Parents : Node_List := Nil_List;
      Contents : Node_List;
      Forward : N_Forward_Interface_Acc;
      Abst : Boolean;
   end record;
   type N_Interface_Acc is access all N_Interface;
   function Get_Kind (N : N_Interface) return Node_Kind;

   --  Forward declaration of an interface.
   type N_Forward_Interface is new N_Named with record
      Forward : N_Interface_Acc;
   end record;
   function Get_Kind (N : N_Forward_Interface) return Node_Kind;

   --  A ValueType.
   type N_Forward_ValueType;
   type N_Forward_ValueType_Acc is access all N_Forward_ValueType;

   type N_ValueType is new N_Scope with record
      Parents : Node_List := Nil_List;
      Supports : Node_List := Nil_List;
      Contents : Node_List := Nil_List;
      Forward : N_Forward_ValueType_Acc := null;
      Abst : Boolean;
      Custom : Boolean;
      Truncatable : Boolean := False;
   end record;
   type N_ValueType_Acc is access all N_ValueType;
   function Get_Kind (N : N_ValueType) return Node_Kind;

   --  Forward declaration of an interface.
   type N_Forward_ValueType is new N_Named with record
      Forward : N_ValueType_Acc;
      Abst : Boolean;
   end record;
   function Get_Kind (N : N_Forward_ValueType) return Node_Kind;

   --  A boxed ValueType
   type N_Boxed_ValueType is new N_Named with record
      Boxed_Type : N_Root_Acc;
   end record;
   type N_Boxed_ValueType_Acc is access all N_Boxed_ValueType;
   function Get_Kind (N : N_Boxed_ValueType) return Node_Kind;

   type N_State_Member is new N_Root with record
      null;
   end record;
   type N_State_Member_Acc is access all N_State_Member;
   function Get_Kind (N : N_State_Member) return Node_Kind;

   type N_Initializer is new N_Named with record
      null;
   end record;
   type N_Initializer_Acc is access all N_Initializer;
   function Get_Kind (N : N_Initializer) return Node_Kind;

   --  A scoped name.
   type N_Scoped_Name is new N_Root with record
      Value : N_Named_Acc;
   end record;
   type N_Scoped_Name_Acc is access all N_Scoped_Name;
   function Get_Kind (N : N_Scoped_Name) return Node_Kind;

--    --  Declaration of an operation.
--    type N_Operation is new N_Scope with record
--       Is_Oneway : Boolean;
--       Op_Type : N_Root_Acc;
--       Parameters : Node_List;
--       Raises : Node_List;
--       Contexts : Node_List;
--    end record;
--    type N_Operation_Acc is access all N_Operation;
--    function Get_Kind (N : N_Operation) return Node_Kind;

--    type N_Attribute is new N_Named with record
--       Is_Readonly : Boolean;
--       A_Type : N_Root_Acc;
--    end record;
--    type N_Attribute_Acc is access all N_Attribute;
--    function Get_Kind (N : N_Attribute) return Node_Kind;

--    --  Types.
--    type N_Types is abstract new N_Root with null record;
--    function Get_Kind (N : N_Types) return Node_Kind is abstract;

--    --  Void type.
--    type N_Void is new N_Types with null record;
--    type N_Void_Acc is access all N_Void;
--    function Get_Kind (N : N_Void) return Node_Kind;

--    --  float type.
--    type N_Float is new N_Types with null record;
--    function Get_Kind (N : N_Float) return Node_Kind;

--    --  Double type.
--    type N_Double is new N_Types with null record;
--    function Get_Kind (N : N_Double) return Node_Kind;

--    --  Long double type.
--    type N_Long_Double is new N_Types with null record;
--    function Get_Kind (N : N_Long_Double) return Node_Kind;

--    --  Short type.
--    type N_Short is new N_Types with null record;
--    function Get_Kind (N : N_Short) return Node_Kind;

--    --  Long type.
--    type N_Long is new N_Types with null record;
--    function Get_Kind (N : N_Long) return Node_Kind;

--    --  Long Long type.
--    type N_Long_Long is new N_Types with null record;
--    function Get_Kind (N : N_Long_Long) return Node_Kind;

--    --  Unsigned short type.
--    type N_Unsigned_Short is new N_Types with null record;
--    function Get_Kind (N : N_Unsigned_Short) return Node_Kind;

--    --  Unsigned long type.
--    type N_Unsigned_Long is new N_Types with null record;
--    function Get_Kind (N : N_Unsigned_Long) return Node_Kind;

--    --  Unsigned Long Long type.
--    type N_Unsigned_Long_Long is new N_Types with null record;
--    function Get_Kind (N : N_Unsigned_Long_Long) return Node_Kind;

--    --  Char type.
--    type N_Char is new N_Types with null record;
--    function Get_Kind (N : N_Char) return Node_Kind;

--    --  Wchar type.
--    type N_Wchar is new N_Types with null record;
--    function Get_Kind (N : N_Wchar) return Node_Kind;

--    --  Boolean type.
--    type N_Boolean is new N_Types with null record;
--    function Get_Kind (N : N_Boolean) return Node_Kind;

--    --  Octet type.
--    type N_Octet is new N_Types with null record;
--    function Get_Kind (N : N_Octet) return Node_Kind;

--    --  Any type.
--    type N_Any is new N_Types with null record;
--    function Get_Kind (N : N_Any) return Node_Kind;

--    --  Object type.
--    type N_Object is new N_Types with null record;
--    function Get_Kind (N : N_Object) return Node_Kind;

--    --  String type
--    --  If BOUND = null, then this is an unbounded string.
--    type N_String is new N_Types with record
--       Bound : N_Root_Acc;
--    end record;
--    type N_String_Acc is access all N_String;
--    function Get_Kind (N : N_String) return Node_Kind;

--    type Param_Mode is (Mode_In, Mode_Out, Mode_Inout);
--    type N_Param is new N_Named with record
--       Mode : Param_Mode;
--       P_Type : N_Root_Acc;
--    end record;
--    type N_Param_Acc is access all N_Param;
--    function Get_Kind (N : N_Param) return Node_Kind;

   type N_Exception is new N_Named with record
      Members : Node_List;
   end record;
   type N_Exception_Acc is access all N_Exception;
   function Get_Kind (N : N_Exception) return Node_Kind;

--    type N_Member is new N_Root with record
--       M_Type : N_Root_Acc;
--       Decl : Node_List;
--    end record;
--    type N_Member_Acc is access all N_Member;
--    function Get_Kind (N : N_Member) return Node_Kind;

--    type N_Declarator is new N_Named with record
--       Array_Bounds : Node_List;
--    end record;
--    type N_Declarator_Acc is access all N_Declarator;
--    function Get_Kind (N : N_Declarator) return Node_Kind;

--    type N_Native is new N_Root with record
--       Decl : N_Declarator_Acc;
--    end record;
--    type N_Native_Acc is access all N_Native;
--    function Get_Kind (N : N_Native) return Node_Kind;

--    type N_Union is new N_Scope with record
--       Switch_Type : N_Root_Acc;
--       Cases : Node_List;
--    end record;
--    type N_Union_Acc is access all N_Union;
--    function Get_Kind (N : N_Union) return Node_Kind;

--    --  Labels is a list of const_expression.  For the "default" label,
--    --  a null element is used.
--    type N_Case is new N_Root with record
--       Labels : Node_List;
--       C_Type : N_Root_Acc;
--       C_Decl : N_Declarator_Acc;
--    end record;
--    type N_Case_Acc is access all N_Case;
--    function Get_Kind (N : N_Case) return Node_Kind;

--    type N_Struct is new N_Scope with record
--       Members : Node_List;
--    end record;
--    type N_Struct_Acc is access all N_Struct;
--    function Get_Kind (N : N_Struct) return Node_Kind;

--    type N_Enum is new N_Named with record
--       Enumerators : Node_List;
--    end record;
--    type N_Enum_Acc is access all N_Enum;
--    function Get_Kind (N : N_Enum) return Node_Kind;

--    type N_Enumerator is new N_Named with null record;
--    type N_Enumerator_Acc is access all N_Enumerator;
--    function Get_Kind (N : N_Enumerator) return Node_Kind;

--    type N_Type_Declarator is new N_Root with record
--       T_Type : N_Root_Acc;
--       Declarators : Node_List;
--    end record;
--    type N_Type_Declarator_Acc is access all N_Type_Declarator;
--    function Get_Kind (N : N_Type_Declarator) return Node_Kind;

--    --  If BOUND is null, then this is an unbounded sequence.
--    type N_Sequence is new N_Root with record
--       S_Type : N_Root_Acc;
--       Bound : N_Root_Acc;
--    end record;
--    type N_Sequence_Acc is access all N_Sequence;
--    function Get_Kind (N : N_Sequence) return Node_Kind;

--    type N_Expr is abstract new N_Root with null record;
--    type N_Expr_Acc is access all N_Expr;
--    function Get_Kind (N : N_Expr) return Node_Kind is abstract;

--    type N_Binary_Expr is abstract new N_Expr with record
--       Left, Right : N_Root_Acc;
--    end record;
--    type N_Binary_Expr_Acc is access all N_Binary_Expr'Class;
--    function Get_Kind (N : N_Binary_Expr) return Node_Kind is abstract;

--    type N_Unary_Expr is abstract new N_Expr with record
--       Operand : N_Root_Acc;
--    end record;
--    type N_Unary_Expr_Acc is access all N_Unary_Expr'Class;
--    function Get_Kind (N : N_Unary_Expr) return Node_Kind is abstract;

--    type N_Or_Expr is new N_Binary_Expr with null record;
--    type N_Or_Expr_Acc is access N_Or_Expr;
--    function Get_Kind (N : N_Or_Expr) return Node_Kind;

--    type N_Xor_Expr is new N_Binary_Expr with null record;
--    type N_Xor_Expr_Acc is access N_Xor_Expr;
--    function Get_Kind (N : N_Xor_Expr) return Node_Kind;

--    type N_And_Expr is new N_Binary_Expr with null record;
--    type N_And_Expr_Acc is access N_And_Expr;
--    function Get_Kind (N : N_And_Expr) return Node_Kind;

--    type N_Shl_Expr is new N_Binary_Expr with null record;
--    type N_Shl_Expr_Acc is access N_Shl_Expr;
--    function Get_Kind (N : N_Shl_Expr) return Node_Kind;

--    type N_Shr_Expr is new N_Binary_Expr with null record;
--    type N_Shr_Expr_Acc is access N_Shr_Expr;
--    function Get_Kind (N : N_Shr_Expr) return Node_Kind;

--    type N_Add_Expr is new N_Binary_Expr with null record;
--    type N_Add_Expr_Acc is access N_Add_Expr;
--    function Get_Kind (N : N_Add_Expr) return Node_Kind;

--    type N_Sub_Expr is new N_Binary_Expr with null record;
--    type N_Sub_Expr_Acc is access N_Sub_Expr;
--    function Get_Kind (N : N_Sub_Expr) return Node_Kind;

--    type N_Mul_Expr is new N_Binary_Expr with null record;
--    type N_Mul_Expr_Acc is access N_Mul_Expr;
--    function Get_Kind (N : N_Mul_Expr) return Node_Kind;

--    type N_Div_Expr is new N_Binary_Expr with null record;
--    type N_Div_Expr_Acc is access N_Div_Expr;
--    function Get_Kind (N : N_Div_Expr) return Node_Kind;

--    type N_Mod_Expr is new N_Binary_Expr with null record;
--    type N_Mod_Expr_Acc is access N_Mod_Expr;
--    function Get_Kind (N : N_Mod_Expr) return Node_Kind;

--    type N_Neg_Expr is new N_Unary_Expr with null record;
--    type N_Neg_Expr_Acc is access N_Neg_Expr;
--    function Get_Kind (N : N_Neg_Expr) return Node_Kind;

--    type N_Id_Expr is new N_Unary_Expr with null record;
--    type N_Id_Expr_Acc is access N_Id_Expr;
--    function Get_Kind (N : N_Id_Expr) return Node_Kind;

--    type N_Not_Expr is new N_Unary_Expr with null record;
--    type N_Not_Expr_Acc is access N_Not_Expr;
--    function Get_Kind (N : N_Not_Expr) return Node_Kind;

--    type N_Literal is abstract new N_Root with record
--       Lit : String_Cacc;
--    end record;
--    type N_Literal_Acc is access all N_Literal'Class;
--    function Get_Kind (N : N_Literal) return Node_Kind is abstract;

--    type N_Lit_Integer is new N_Literal with null record;
--    type N_Lit_Integer_Acc is access all N_Lit_Integer;
--    function Get_Kind (N : N_Lit_Integer) return Node_Kind;

--    type N_Lit_Floating_Point is new N_Literal with null record;
--    type N_Lit_Floating_Point_Acc is access all N_Lit_Floating_Point;
--    function Get_Kind (N : N_Lit_Floating_Point) return Node_Kind;

   type N_Const is new N_Named with record
      Const_Type : N_Root_Acc;
      Expression : N_Root_Acc;
   end record;
   type N_Const_Acc is access all N_Const;
   function Get_Kind (N : N_Const) return Node_Kind;


private

   --  Definition in a lisp like style of a node list
   type Node_List_Cell;
   type Node_List is access Node_List_Cell;
   type Node_List_Cell is record
      Car : N_Root_Acc;
      Cdr : Node_List;
   end record;

   --  Definition of the iterator on a node list
   type Node_Iterator is new Node_List;

   --  the empty list
   Nil_List : constant Node_List := null;

end Tree;
