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

package body Tree is

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
--    function Get_Kind (N : N_Operation) return Types.Node_Kind is
--    begin
--       return Types.K_Operation;
--    end Get_Kind;

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
--    function Get_Kind (N : N_Void) return Types.Node_Kind is
--    begin
--       return Types.K_Void;
--    end Get_Kind;

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
--    function Get_Kind (N : N_Member) return Types.Node_Kind is
--    begin
--       return Types.K_Member;
--    end Get_Kind;

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
--    function Get_Kind (N : N_Case) return Types.Node_Kind is
--    begin
--       return Types.K_Case;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Or_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Or;
--    end Get_Kind;

   ----------------
   --  Get_Kind  --
   ----------------
--    function Get_Kind (N : N_Xor_Expr) return Types.Node_Kind is
--    begin
--       return Types.K_Xor;
--    end Get_Kind;

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
--    function Get_Kind (N : N_Enumerator) return Types.Node_Kind is
--    begin
--       return Types.K_Enumerator;
--    end Get_Kind;

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
   function Get_Kind (N : N_Const) return Types.Node_Kind is
   begin
      return Types.K_Const;
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

end Tree;

