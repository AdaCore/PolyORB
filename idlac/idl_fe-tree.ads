with Idl_Fe.Types;
with Ada.Unchecked_Deallocation;

package Idl_Fe.Tree is

   ----------------------------------
   --  Management of const values  --
   ----------------------------------

   --  all possible types for an idl const
   type Idl_Short is new Long_Long_Integer range (-2 ** 15) .. (2 ** 15 - 1);
   type Idl_Long is new Long_Long_Integer range (-2 ** 31) .. (2 ** 31 - 1);
   type Idl_LongLong is new Long_Long_Integer range
     (-2 ** 63) .. (2 ** 63 - 1);
   type Idl_UShort is new Long_Long_Integer range 0 .. (2 ** 16 - 1);
   type Idl_ULong is new Long_Long_Integer range 0 .. (2 ** 32 - 1);
   type Idl_ULongLong is new Long_Long_Integer range
     (-2 ** 63) .. (2 ** 63 - 1);
   type Idl_Char is new Long_Long_Integer range 0 .. (2 ** 8 - 1);
   type Idl_WChar is new Long_Long_Integer range 0 .. (2 ** 16 - 1);
   type Idl_Boolean is new Long_Long_Integer range 0 .. 1;
   type Idl_Enum is new Long_Long_Integer range 0 .. (2 ** 32 - 1);

   --  all possible values for an idl const
   type Idl_Value is abstract tagged record
      Const_Type : Types.Const_Type_Ptr;
   end record;
   type Short_Value is new Idl_Value with record
     Value : Idl_Short;
   end record;
   type Long_Value is new Idl_Value with record
     Value : Idl_Long;
   end record;
   type LongLong_Value is new Idl_Value with record
     Value : Idl_LongLong;
   end record;
   type UShort_Value is new Idl_Value with record
     Value : Idl_UShort;
   end record;
   type ULong_Value is new Idl_Value with record
     Value : Idl_ULong;
   end record;
   type ULongLong_Value is new Idl_Value with record
     Value : Idl_ULongLong;
   end record;
   type Char_Value is new Idl_Value with record
     Value : Idl_Char;
   end record;
   type WChar_Value is new Idl_Value with record
     Value : Idl_WChar;
   end record;
   type Boolean_Value is new Idl_Value with record
     Value : Idl_Boolean;
   end record;
   type Enum_Value is new Idl_Value with record
     Value : Idl_Enum;
   end record;
   type Fixed_Value is new Idl_Value with record
      null;  --     Value : Idl_Short;
   end record;

   --  pointers on an idl values
   type Value_Ptr is access all Idl_Value'Class;
   type Short_Value_Ptr is access all Short_Value;
   type Long_Value_Ptr is access all Long_Value;
   type LongLong_Value_Ptr is access all LongLong_Value;
   type UShort_Value_Ptr is access all UShort_Value;
   type ULong_Value_Ptr is access all ULong_Value;
   type ULongLong_Value_Ptr is access all ULongLong_Value;
   type Char_Value_Ptr is access all Char_Value;
   type WChar_Value_Ptr is access all WChar_Value;
   type Boolean_Value_Ptr is access all Boolean_Value;
   type Enum_Value_Ptr is access all Enum_Value;
   type Fixed_Value_Ptr is access all Fixed_Value;

   --  to deallocate pointers on values
   procedure Free is new Ada.Unchecked_Deallocation
     (Short_Value, Short_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Long_Value, Long_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (LongLong_Value, LongLong_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (UShort_Value, UShort_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (ULong_Value, ULong_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (ULongLong_Value, ULongLong_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Char_Value, Char_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (WChar_Value, WChar_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Boolean_Value, Boolean_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Value, Enum_Value_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Fixed_Value, Fixed_Value_Ptr);

   --  compare two value_ptr
   --  actually compare the real values pointed by these pointers
   --  assuming that they are from the same type
   function "<" (X, Y : Value_Ptr) return Boolean;
   function ">" (X, Y : Value_Ptr) return Boolean;

   --  returns true if the value pointed by prec is the one pointed
   --  by next - 1, false else.
   function Is_Prec (Prec, Next : Value_Ptr) return Boolean;


   ------------------------------
   --  Management fo the tree  --
   ------------------------------

   --  An idl is represented as a tree, which is inspired by the
   --  interface repository specification (CORBA 2.3 Chap 10)
   --  However, contrary to the interface repository, it doesn't
   --  use multiple inheritance and doesn't define the container and
   --  contained types.

   --  Top of the repository.
   --  A repository is a list of elements.
   type N_Repository is new Types.N_Forward with record
      Contents : Types.Node_List;
   end record;
   function Get_Kind (N : N_Repository) return Types.Node_Kind;
   type N_Repository_Acc is access all N_Repository;

   --  A module
   type N_Module is new Types.N_Forward with record
      Contents : Types.Node_List;
   end record;
   type N_Module_Acc is access all N_Module;
   function Get_Kind (N : N_Module) return Types.Node_Kind;

   --  An interface.
   type N_Forward_Interface;
   type N_Forward_Interface_Acc is access all N_Forward_Interface;

   type N_Interface is new Types.N_Imports with record
      Parents : Types.Node_List := Types.Nil_List;
      Contents : Types.Node_List;
      Forward : N_Forward_Interface_Acc;
      Abst : Boolean;
   end record;
   type N_Interface_Acc is access all N_Interface;
   function Get_Kind (N : N_Interface) return Types.Node_Kind;

   --  Forward declaration of an interface.
   type N_Forward_Interface is new Types.N_Named with record
      Forward : N_Interface_Acc;
      Abst : Boolean;
   end record;
   function Get_Kind (N : N_Forward_Interface) return Types.Node_Kind;

   --  A ValueType.
   type N_Forward_ValueType;
   type N_Forward_ValueType_Acc is access all N_Forward_ValueType;

   type N_ValueType is new Types.N_Imports with record
      Parents : Types.Node_List := Types.Nil_List;
      Supports : Types.Node_List := Types.Nil_List;
      Contents : Types.Node_List := Types.Nil_List;
      Forward : N_Forward_ValueType_Acc := null;
      Abst : Boolean;
      Custom : Boolean;
      Truncatable : Boolean := False;
   end record;
   type N_ValueType_Acc is access all N_ValueType;
   function Get_Kind (N : N_ValueType) return Types.Node_Kind;

   --  Forward declaration of an interface.
   type N_Forward_ValueType is new Types.N_Named with record
      Forward : N_ValueType_Acc;
      Abst : Boolean;
   end record;
   function Get_Kind (N : N_Forward_ValueType) return Types.Node_Kind;

   --  A boxed ValueType
   type N_Boxed_ValueType is new Types.N_Named with record
      Boxed_Type : Types.N_Root_Acc;
   end record;
   type N_Boxed_ValueType_Acc is access all N_Boxed_ValueType;
   function Get_Kind (N : N_Boxed_ValueType) return Types.Node_Kind;

   type N_Declarator is new Types.N_Named with record
      Array_Bounds : Types.Node_List;
   end record;
   type N_Declarator_Acc is access all N_Declarator;
   function Get_Kind (N : N_Declarator) return Types.Node_Kind;

   type N_State_Member is new Types.N_Root with record
      State_Type : Types.N_Root_Acc;
      State_Declarators : Types.Node_List;
      Is_Public : Boolean;
   end record;
   type N_State_Member_Acc is access all N_State_Member;
   function Get_Kind (N : N_State_Member) return Types.Node_Kind;

   type N_Initializer is new Types.N_Scope with record
      Param_Decls : Types.Node_List;
   end record;
   type N_Initializer_Acc is access all N_Initializer;
   function Get_Kind (N : N_Initializer) return Types.Node_Kind;

   --  A scoped name.
   type N_Scoped_Name is new Types.N_Root with record
      Value : Types.N_Named_Acc;
   end record;
   type N_Scoped_Name_Acc is access all N_Scoped_Name;
   function Get_Kind (N : N_Scoped_Name) return Types.Node_Kind;

   type N_Operation is new Types.N_Scope with record
      Is_Oneway : Boolean;
      Operation_Type : Types.N_Root_Acc;
      Parameters : Types.Node_List;
      Raises : Types.Node_List;
      Contexts : Types.Node_List;
   end record;
   type N_Operation_Acc is access all N_Operation;
   function Get_Kind (N : N_Operation) return Types.Node_Kind;

--    type N_Attribute is new Types.N_Named with record
--       Is_Readonly : Boolean;
--       A_Type : Types.N_Root_Acc;
--    end record;
--    type N_Attribute_Acc is access all N_Attribute;
--    function Get_Kind (N : N_Attribute) return Types.Node_Kind;

   type N_Void is new Types.N_Root with null record;
   type N_Void_Acc is access all N_Void;
   function Get_Kind (N : N_Void) return Types.Node_Kind;

   --  float type.
   type N_Float is new Types.N_Root with null record;
   function Get_Kind (N : N_Float) return Types.Node_Kind;

   --  Double type.
   type N_Double is new Types.N_Root with null record;
   function Get_Kind (N : N_Double) return Types.Node_Kind;

   --  Long double type.
   type N_Long_Double is new Types.N_Root with null record;
   function Get_Kind (N : N_Long_Double) return Types.Node_Kind;

   --  Short type.
   type N_Short is new Types.N_Root with null record;
   function Get_Kind (N : N_Short) return Types.Node_Kind;

   --  Long type.
   type N_Long is new Types.N_Root with null record;
   function Get_Kind (N : N_Long) return Types.Node_Kind;

   --  Long Long type.
   type N_Long_Long is new Types.N_Root with null record;
   function Get_Kind (N : N_Long_Long) return Types.Node_Kind;

   --  Unsigned short type.
   type N_Unsigned_Short is new Types.N_Root with null record;
   function Get_Kind (N : N_Unsigned_Short) return Types.Node_Kind;

   --  Unsigned long type.
   type N_Unsigned_Long is new Types.N_Root with null record;
   function Get_Kind (N : N_Unsigned_Long) return Types.Node_Kind;

   --  Unsigned Long Long type.
   type N_Unsigned_Long_Long is new Types.N_Root with null record;
   function Get_Kind (N : N_Unsigned_Long_Long) return Types.Node_Kind;

   --  Char type.
   type N_Char is new Types.N_Root with null record;
   type N_Char_Acc is access all N_Char;
   function Get_Kind (N : N_Char) return Types.Node_Kind;

   --  Wchar type.
   type N_Wide_Char is new Types.N_Root with null record;
   type N_Wide_Char_Acc is access all N_Wide_Char;
   function Get_Kind (N : N_Wide_Char) return Types.Node_Kind;

   --  Boolean type.
   type N_Boolean is new Types.N_Root with null record;
   type N_Boolean_Acc is access all N_Boolean;
   function Get_Kind (N : N_Boolean) return Types.Node_Kind;

   --  Octet type.
   type N_Octet is new Types.N_Root with null record;
   type N_Octet_Acc is access all N_Octet;
   function Get_Kind (N : N_Octet) return Types.Node_Kind;

   --  Any type.
   type N_Any is new Types.N_Root with null record;
   type N_Any_Acc is access all N_Any;
   function Get_Kind (N : N_Any) return Types.Node_Kind;

   --  Object type.
   type N_Object is new Types.N_Root with null record;
   type N_Object_Acc is access all N_Object;
   function Get_Kind (N : N_Object) return Types.Node_Kind;

   type Param_Mode is (Mode_In, Mode_Out, Mode_Inout);
   type N_Param is new Types.N_Root with record
      Mode : Param_Mode;
      Param_Type : Types.N_Root_Acc;
      Declarator : N_Declarator_Acc;
   end record;
   type N_Param_Acc is access all N_Param;
   function Get_Kind (N : N_Param) return Types.Node_Kind;

   type N_Exception is new Types.N_Named with record
      Members : Types.Node_List;
   end record;
   type N_Exception_Acc is access all N_Exception;
   function Get_Kind (N : N_Exception) return Types.Node_Kind;

   type N_Member is new Types.N_Root with record
      M_Type : Types.N_Root_Acc;
      Decl : Types.Node_List;
   end record;
   type N_Member_Acc is access all N_Member;
   function Get_Kind (N : N_Member) return Types.Node_Kind;

   type N_Native is new Types.N_Root with record
      Declarator : N_Declarator_Acc;
   end record;
   type N_Native_Acc is access all N_Native;
   function Get_Kind (N : N_Native) return Types.Node_Kind;

   type N_Union is new Types.N_Scope with record
      Switch_Type : Types.N_Root_Acc;
      Cases : Types.Node_List;
   end record;
   type N_Union_Acc is access all N_Union;
   function Get_Kind (N : N_Union) return Types.Node_Kind;

   --  Labels is a list of const_expression.  For the "default" label,
   --  a null element is used.
   type N_Case is new Types.N_Root with record
      Labels : Types.Node_List;
      Case_Type : Types.N_Root_Acc;
      Case_Decl : N_Declarator_Acc;
   end record;
   type N_Case_Acc is access all N_Case;
   function Get_Kind (N : N_Case) return Types.Node_Kind;

   type N_Struct is new Types.N_Scope with record
      Members : Types.Node_List;
   end record;
   type N_Struct_Acc is access all N_Struct;
   function Get_Kind (N : N_Struct) return Types.Node_Kind;

   type N_Enum is new Types.N_Named with record
      Enumerators : Types.Node_List;
   end record;
   type N_Enum_Acc is access all N_Enum;
   function Get_Kind (N : N_Enum) return Types.Node_Kind;

   type N_Enumerator is new Types.N_Named with null record;
   type N_Enumerator_Acc is access all N_Enumerator;
   function Get_Kind (N : N_Enumerator) return Types.Node_Kind;

   type N_Type_Declarator is new Types.N_Root with record
      T_Type : Types.N_Root_Acc;
      Declarators : Types.Node_List;
   end record;
   type N_Type_Declarator_Acc is access all N_Type_Declarator;
   function Get_Kind (N : N_Type_Declarator) return Types.Node_Kind;

   type N_Expr is abstract new Types.N_Root with record
      Value : Value_Ptr;
   end record;
   type N_Expr_Acc is access all N_Expr;
   function Get_Kind (N : N_Expr) return Types.Node_Kind is abstract;

   type N_Binary_Expr is abstract new N_Expr with record
      Left, Right : N_Expr_Acc;
   end record;
   type N_Binary_Expr_Acc is access all N_Binary_Expr'Class;
   function Get_Kind (N : N_Binary_Expr) return Types.Node_Kind is abstract;

--    type N_Unary_Expr is abstract new N_Expr with record
--       Operand : Types.N_Root_Acc;
--    end record;
--    type N_Unary_Expr_Acc is access all N_Unary_Expr'Class;
--    function Get_Kind (N : N_Unary_Expr) return Types.Node_Kind is abstract;

   type N_Or_Expr is new N_Binary_Expr with null record;
   type N_Or_Expr_Acc is access N_Or_Expr;
   function Get_Kind (N : N_Or_Expr) return Types.Node_Kind;

   type N_Xor_Expr is new N_Binary_Expr with null record;
   type N_Xor_Expr_Acc is access N_Xor_Expr;
   function Get_Kind (N : N_Xor_Expr) return Types.Node_Kind;

--    type N_And_Expr is new N_Binary_Expr with null record;
--    type N_And_Expr_Acc is access N_And_Expr;
--    function Get_Kind (N : N_And_Expr) return Types.Node_Kind;

--    type N_Shl_Expr is new N_Binary_Expr with null record;
--    type N_Shl_Expr_Acc is access N_Shl_Expr;
--    function Get_Kind (N : N_Shl_Expr) return Types.Node_Kind;

--    type N_Shr_Expr is new N_Binary_Expr with null record;
--    type N_Shr_Expr_Acc is access N_Shr_Expr;
--    function Get_Kind (N : N_Shr_Expr) return Types.Node_Kind;

--    type N_Add_Expr is new N_Binary_Expr with null record;
--    type N_Add_Expr_Acc is access N_Add_Expr;
--    function Get_Kind (N : N_Add_Expr) return Types.Node_Kind;

--    type N_Sub_Expr is new N_Binary_Expr with null record;
--    type N_Sub_Expr_Acc is access N_Sub_Expr;
--    function Get_Kind (N : N_Sub_Expr) return Types.Node_Kind;

--    type N_Mul_Expr is new N_Binary_Expr with null record;
--    type N_Mul_Expr_Acc is access N_Mul_Expr;
--    function Get_Kind (N : N_Mul_Expr) return Types.Node_Kind;

--    type N_Div_Expr is new N_Binary_Expr with null record;
--    type N_Div_Expr_Acc is access N_Div_Expr;
--    function Get_Kind (N : N_Div_Expr) return Types.Node_Kind;

--    type N_Mod_Expr is new N_Binary_Expr with null record;
--    type N_Mod_Expr_Acc is access N_Mod_Expr;
--    function Get_Kind (N : N_Mod_Expr) return Types.Node_Kind;

--    type N_Neg_Expr is new N_Unary_Expr with null record;
--    type N_Neg_Expr_Acc is access N_Neg_Expr;
--    function Get_Kind (N : N_Neg_Expr) return Types.Node_Kind;

--    type N_Id_Expr is new N_Unary_Expr with null record;
--    type N_Id_Expr_Acc is access N_Id_Expr;
--    function Get_Kind (N : N_Id_Expr) return Types.Node_Kind;

--    type N_Not_Expr is new N_Unary_Expr with null record;
--    type N_Not_Expr_Acc is access N_Not_Expr;
--    function Get_Kind (N : N_Not_Expr) return Types.Node_Kind;

   type N_Literal is abstract new Types.N_Root with null record;
   type N_Literal_Acc is access all N_Literal'Class;
   function Get_Kind (N : N_Literal) return Types.Node_Kind is abstract;

   type N_Lit_String is new N_Literal with record
     Value : Types.String_Cacc;
   end record;
   type N_Lit_String_Acc is access all N_Lit_String;
   function Get_Kind (N : N_Lit_String) return Types.Node_Kind;

--    type N_Lit_Integer is new N_Literal with null record;
--    type N_Lit_Integer_Acc is access all N_Lit_Integer;
--    function Get_Kind (N : N_Lit_Integer) return Types.Node_Kind;

--    type N_Lit_Floating_Point is new N_Literal with null record;
--    type N_Lit_Floating_Point_Acc is access all N_Lit_Floating_Point;
--    function Get_Kind (N : N_Lit_Floating_Point) return Types.Node_Kind;

   type N_Const_Dcl is new Types.N_Named with record
      Constant_Type : Types.N_Root_Acc;
      Expression : N_Expr_Acc;
   end record;
   type N_Const_Dcl_Acc is access all N_Const_Dcl;
   function Get_Kind (N : N_Const_Dcl) return Types.Node_Kind;

   --  If BOUND is null, then this is an unbounded sequence.
   type N_Sequence is new Types.N_Root with record
      Sequence_Type : Types.N_Root_Acc;
      Bound : N_Expr_Acc;
   end record;
   type N_Sequence_Acc is access all N_Sequence;
   function Get_Kind (N : N_Sequence) return Types.Node_Kind;

   --  If BOUND = null, then this is an unbounded string.
   type N_String is new Types.N_Root with record
      Bound : N_Expr_Acc;
   end record;
   type N_String_Acc is access all N_String;
   function Get_Kind (N : N_String) return Types.Node_Kind;

   --  If BOUND = null, then this is an unbounded string.
   type N_Wide_String is new Types.N_Root with record
      Bound : N_Expr_Acc;
   end record;
   type N_Wide_String_Acc is access all N_Wide_String;
   function Get_Kind (N : N_Wide_String) return Types.Node_Kind;

--   type Fixed_Digits is new Natural range 1 .. 31;
   type N_Fixed is new Types.N_Root with record
      Digits_Nb : N_Expr_Acc;
      Scale : N_Expr_Acc;
   end record;
   type N_Fixed_Acc is access all N_Fixed;
   function Get_Kind (N : N_Fixed) return Types.Node_Kind;

   --  ValueBase type.
   type N_ValueBase is new Types.N_Root with null record;
   type N_ValueBase_Acc is access all N_ValueBase;
   function Get_Kind (N : N_ValueBase) return Types.Node_Kind;

--
--  INUTILE ???
--
--    --  Different string types to be used for identifiers and versions
--    type Identifier is new String_Cacc;
--    type Version_Spec is new String_Cacc;


end Idl_Fe.Tree;
