with Idl_Fe.Errors;

package body Idl_Fe.Tree is

   ----------------------------------
   --  Management of const values  --
   ----------------------------------

   ---------
   --  <  --
   ---------
   function "<" (X, Y : Value_Ptr) return Boolean is
   begin
      case X.Const_Type.Kind is
         when Types.C_Short =>
            return Short_Value_Ptr (X).Value < Short_Value_Ptr (Y).Value;
         when Types.C_Long =>
            return Long_Value_Ptr (X).Value < Long_Value_Ptr (Y).Value;
         when Types.C_LongLong =>
            return LongLong_Value_Ptr (X).Value < LongLong_Value_Ptr (Y).Value;
         when Types.C_UShort =>
            return UShort_Value_Ptr (X).Value < UShort_Value_Ptr (Y).Value;
         when Types.C_ULong =>
            return ULong_Value_Ptr (X).Value < ULong_Value_Ptr (Y).Value;
         when Types.C_ULongLong =>
            return ULongLong_Value_Ptr (X).Value <
              ULongLong_Value_Ptr (Y).Value;
         when Types.C_Char =>
            return Char_Value_Ptr (X).Value < Char_Value_Ptr (Y).Value;
         when Types.C_WChar =>
            return WChar_Value_Ptr (X).Value < WChar_Value_Ptr (Y).Value;
         when Types.C_Boolean =>
            return Boolean_Value_Ptr (X).Value < Boolean_Value_Ptr (Y).Value;
         when Types.C_Enum =>
            return Enum_Value_Ptr (X).Value < Enum_Value_Ptr (Y).Value;
         when Types.C_Fixed =>
            --  FIXME
            return True;
--            return Fixed_Value_Ptr (X).Value < Fixed_Value_Ptr (Y).Value;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
      --  Never reached
      return True;
   end "<";

   ---------
   --  >  --
   ---------
   function ">" (X, Y : Value_Ptr) return Boolean is
   begin
      case X.Const_Type.Kind is
         when Types.C_Short =>
            return Short_Value_Ptr (X).Value > Short_Value_Ptr (Y).Value;
         when Types.C_Long =>
            return Long_Value_Ptr (X).Value > Long_Value_Ptr (Y).Value;
         when Types.C_LongLong =>
            return LongLong_Value_Ptr (X).Value > LongLong_Value_Ptr (Y).Value;
         when Types.C_UShort =>
            return UShort_Value_Ptr (X).Value > UShort_Value_Ptr (Y).Value;
         when Types.C_ULong =>
            return ULong_Value_Ptr (X).Value > ULong_Value_Ptr (Y).Value;
         when Types.C_ULongLong =>
            return ULongLong_Value_Ptr (X).Value >
              ULongLong_Value_Ptr (Y).Value;
         when Types.C_Char =>
            return Char_Value_Ptr (X).Value > Char_Value_Ptr (Y).Value;
         when Types.C_WChar =>
            return WChar_Value_Ptr (X).Value > WChar_Value_Ptr (Y).Value;
         when Types.C_Boolean =>
            return Boolean_Value_Ptr (X).Value > Boolean_Value_Ptr (Y).Value;
         when Types.C_Enum =>
            return Enum_Value_Ptr (X).Value > Enum_Value_Ptr (Y).Value;
         when Types.C_Fixed =>
            --  FIXME
            return True;
--            return Fixed_Value_Ptr (X).Value > Fixed_Value_Ptr (Y).Value;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
      --  Never reached
      return True;
   end ">";

   ---------------
   --  Is_Prec  --
   ---------------
   function Is_Prec (Prec, Next : Value_Ptr) return Boolean is
   begin
      case Prec.Const_Type.Kind is
         when Types.C_Short =>
            return Short_Value_Ptr (Prec).Value =
              Idl_Short'Pred (Short_Value_Ptr (Next).Value);
         when Types.C_Long =>
            return Long_Value_Ptr (Prec).Value =
              Idl_Long'Pred (Long_Value_Ptr (Next).Value);
         when Types.C_LongLong =>
            return LongLong_Value_Ptr (Prec).Value =
              Idl_LongLong'Pred (LongLong_Value_Ptr (Next).Value);
         when Types.C_UShort =>
            return UShort_Value_Ptr (Prec).Value =
              Idl_UShort'Pred (UShort_Value_Ptr (Next).Value);
         when Types.C_ULong =>
            return ULong_Value_Ptr (Prec).Value =
              Idl_ULong'Pred (ULong_Value_Ptr (Next).Value);
         when Types.C_ULongLong =>
            return ULongLong_Value_Ptr (Prec).Value =
              Idl_ULongLong'Pred (ULongLong_Value_Ptr (Next).Value);
         when Types.C_Char =>
            return Char_Value_Ptr (Prec).Value =
              Idl_Char'Pred (Char_Value_Ptr (Next).Value);
         when Types.C_WChar =>
            return WChar_Value_Ptr (Prec).Value =
              Idl_WChar'Pred (WChar_Value_Ptr (Next).Value);
         when Types.C_Boolean =>
            return Boolean_Value_Ptr (Prec).Value =
              Idl_Boolean'Pred (Boolean_Value_Ptr (Next).Value);
         when Types.C_Enum =>
            return Enum_Value_Ptr (Prec).Value =
              Idl_Enum'Pred (Enum_Value_Ptr (Next).Value);
         when Types.C_Fixed =>
            --  FIXME
            return True;
--            return Fixed_Value_Ptr (Prec).Value =
--              Fixed_Value_Ptr (Next).Value'Pred;
         when others =>
            raise Idl_Fe.Errors.Internal_Error;
      end case;
      --  Never reached
      return True;
   end Is_Prec;



   ------------------------------
   --  Management of the tree  --
   ------------------------------

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Repository) return Types.Node_Kind is
   begin
      return Types.K_Repository;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Module) return Types.Node_Kind is
   begin
      return Types.K_Module;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Interface) return Types.Node_Kind is
   begin
      return Types.K_Interface;
   end Get_Kind;

   -------------------
   --  Get_Parents  --
   -------------------
   function Get_Parents (Node : N_Interface) return Types.Node_List is
   begin
      return Node.Parents;
   end Get_Parents;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Forward_Interface) return Types.Node_Kind is
   begin
      return Types.K_Forward_Interface;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_ValueType) return Types.Node_Kind is
   begin
      return Types.K_ValueType;
   end Get_Kind;

   -------------------
   --  Get_Parents  --
   -------------------
   function Get_Parents (Node : N_ValueType) return Types.Node_List is
   begin
      return Node.Parents;
   end Get_Parents;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Forward_ValueType) return Types.Node_Kind is
   begin
      return Types.K_Forward_ValueType;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Boxed_ValueType) return Types.Node_Kind is
   begin
      return Types.K_Boxed_ValueType;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_State_Member) return Types.Node_Kind is
   begin
      return Types.K_State_Member;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Initializer) return Types.Node_Kind is
   begin
      return Types.K_Initializer;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Scoped_Name) return Types.Node_Kind is
   begin
      return Types.K_Scoped_Name;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Operation) return Types.Node_Kind is
   begin
      return Types.K_Operation;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Attribute) return Types.Node_Kind is
--    begin
--       return Types.K_Attribute;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Void) return Types.Node_Kind is
   begin
      return Types.K_Void;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Float) return Types.Node_Kind is
   begin
      return Types.K_Float;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Double) return Types.Node_Kind is
   begin
      return Types.K_Double;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Long_Double) return Types.Node_Kind is
   begin
      return Types.K_Long_Double;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Short) return Types.Node_Kind is
   begin
      return Types.K_Short;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Long) return Types.Node_Kind is
   begin
      return Types.K_Long;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Long_Long) return Types.Node_Kind is
   begin
      return Types.K_Long_Long;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Unsigned_Short) return Types.Node_Kind is
   begin
      return Types.K_Unsigned_Short;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Unsigned_Long) return Types.Node_Kind is
   begin
      return Types.K_Unsigned_Long;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Unsigned_Long_Long) return Types.Node_Kind is
   begin
      return Types.K_Unsigned_Long_Long;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Char) return Types.Node_Kind is
   begin
      return Types.K_Char;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Wide_Char) return Types.Node_Kind is
   begin
      return Types.K_Wide_Char;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Boolean) return Types.Node_Kind is
   begin
      return Types.K_Boolean;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Octet) return Types.Node_Kind is
   begin
      return Types.K_Octet;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Any) return Types.Node_Kind is
   begin
      return Types.K_Any;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Object) return Types.Node_Kind is
   begin
      return Types.K_Object;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_String) return Types.Node_Kind is
   begin
      return Types.K_String;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Wide_String) return Types.Node_Kind is
   begin
      return Types.K_Wide_String;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Param) return Types.Node_Kind is
   begin
      return Types.K_Param;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Exception) return Types.Node_Kind is
   begin
      return Types.K_Exception;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Member) return Types.Node_Kind is
   begin
      return Types.K_Member;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Declarator) return Types.Node_Kind is
   begin
      return Types.K_Declarator;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Union) return Types.Node_Kind is
   begin
      return Types.K_Union;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Case) return Types.Node_Kind is
   begin
      return Types.K_Case;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Or_Expr) return Types.Node_Kind is
   begin
      return Types.K_Or;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Xor_Expr) return Types.Node_Kind is
   begin
      return Types.K_Xor;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_And_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_And;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Shl_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Shl;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Shr_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Shr;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Add_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Add;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Sub_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Sub;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Mul_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Mul;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Div_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Div;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Mod_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Mod;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Neg_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Neg;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Id_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Id;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Not_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Not;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Lit_String) return Types.Node_Kind is
   begin
      return Types.K_Lit_String;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Lit_Integer) return Types.Node_Kind is
--    begin
--       return Types.K_Lit_Integer;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Lit_Floating_Point) return Types.Node_Kind is
--    begin
--       return Types.K_Lit_Floating_Point;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Struct) return Types.Node_Kind is
   begin
      return Types.K_Struct;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Enum) return Types.Node_Kind is
   begin
      return Types.K_Enum;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Enumerator) return Types.Node_Kind is
   begin
      return Types.K_Enumerator;
   end Get_Kind;

   function Get_Kind (N : N_Type_Declarator) return Types.Node_Kind is
   begin
      return Types.K_Type_Declarator;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Sequence) return Types.Node_Kind is
   begin
      return Types.K_Sequence;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Const_Dcl) return Types.Node_Kind is
   begin
      return Types.K_Const_Dcl;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Fixed) return Types.Node_Kind is
   begin
      return Types.K_Fixed;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_Native) return Types.Node_Kind is
   begin
      return Types.K_Native;
   end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
   function Get_Kind (N : N_ValueBase) return Types.Node_Kind is
   begin
      return Types.K_ValueBase;
   end Get_Kind;

end Idl_Fe.Tree;

