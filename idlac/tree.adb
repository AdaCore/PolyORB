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

   -------------------------------------
   --  Some usefull type and methods  --
   -------------------------------------

   procedure Init (It : out Node_Iterator; List : Node_List) is
   begin
      It := Node_Iterator (List);
   end Init;

   function Get_Node (It : Node_Iterator) return N_Root_Acc is
   begin
      return It.Car;
   end Get_Node;

   procedure Next (It : in out Node_Iterator) is
   begin
      It := Node_Iterator (It.Cdr);
   end Next;

   function Is_End (It : Node_Iterator) return Boolean is
   begin
      return It = null;
   end Is_End;

   procedure Append_Node (List : in out Node_List; Node : N_Root_Acc) is
      Cell, Last : Node_List;
   begin
      Cell := new Node_List_Cell'(Car => Node, Cdr => null);
      if List = null then
         List := Cell;
      else
         Last := List;
         while Last.Cdr /= null loop
            Last := Last.Cdr;
         end loop;
         Last.Cdr := Cell;
      end if;
   end Append_Node;



   -------------------
   --  The idl tree --
   -------------------

   function Get_Kind (N : N_Repository) return Node_Kind is
   begin
      return K_Repository;
   end Get_Kind;

   function Get_Kind (N : N_Module) return Node_Kind is
   begin
      return K_Module;
   end Get_Kind;

   function Get_Kind (N : N_Interface) return Node_Kind is
   begin
      return K_Interface;
   end Get_Kind;

   function Get_Kind (N : N_Forward_Interface) return Node_Kind is
   begin
      return K_Forward_Interface;
   end Get_Kind;

   function Get_Kind (N : N_ValueType) return Node_Kind is
   begin
      return K_ValueType;
   end Get_Kind;

   function Get_Kind (N : N_Forward_ValueType) return Node_Kind is
   begin
      return K_Forward_ValueType;
   end Get_Kind;

   function Get_Kind (N : N_Boxed_ValueType) return Node_Kind is
   begin
      return K_Boxed_ValueType;
   end Get_Kind;

   function Get_Kind (N : N_Scoped_Name) return Node_Kind is
   begin
      return K_Scoped_Name;
   end Get_Kind;

--    function Get_Kind (N : N_Operation) return Node_Kind is
--    begin
--       return K_Operation;
--    end Get_Kind;

--    function Get_Kind (N : N_Attribute) return Node_Kind is
--    begin
--       return K_Attribute;
--    end Get_Kind;

--    function Get_Kind (N : N_Void) return Node_Kind is
--    begin
--       return K_Void;
--    end Get_Kind;

--    function Get_Kind (N : N_Float) return Node_Kind is
--    begin
--       return K_Float;
--    end Get_Kind;

--    function Get_Kind (N : N_Double) return Node_Kind is
--    begin
--       return K_Double;
--    end Get_Kind;

--    function Get_Kind (N : N_Long_Double) return Node_Kind is
--    begin
--       return K_Long_Double;
--    end Get_Kind;

--    function Get_Kind (N : N_Short) return Node_Kind is
--    begin
--       return K_Short;
--    end Get_Kind;

--    function Get_Kind (N : N_Long) return Node_Kind is
--    begin
--       return K_Long;
--    end Get_Kind;

--    function Get_Kind (N : N_Long_Long) return Node_Kind is
--    begin
--       return K_Long_Long;
--    end Get_Kind;

--    function Get_Kind (N : N_Unsigned_Short) return Node_Kind is
--    begin
--       return K_Unsigned_Short;
--    end Get_Kind;

--    function Get_Kind (N : N_Unsigned_Long) return Node_Kind is
--    begin
--       return K_Unsigned_Long;
--    end Get_Kind;

--    function Get_Kind (N : N_Unsigned_Long_Long) return Node_Kind is
--    begin
--       return K_Unsigned_Long_Long;
--    end Get_Kind;

--    function Get_Kind (N : N_Char) return Node_Kind is
--    begin
--       return K_Char;
--    end Get_Kind;

--    function Get_Kind (N : N_Wchar) return Node_Kind is
--    begin
--       return K_Wchar;
--    end Get_Kind;

--    function Get_Kind (N : N_Boolean) return Node_Kind is
--    begin
--       return K_Boolean;
--    end Get_Kind;

--    function Get_Kind (N : N_Octet) return Node_Kind is
--    begin
--       return K_Octet;
--    end Get_Kind;

--    function Get_Kind (N : N_Any) return Node_Kind is
--    begin
--       return K_Any;
--    end Get_Kind;

--    function Get_Kind (N : N_Object) return Node_Kind is
--    begin
--       return K_Object;
--    end Get_Kind;

--    function Get_Kind (N : N_String) return Node_Kind is
--    begin
--       return K_String;
--    end Get_Kind;

--    function Get_Kind (N : N_Param) return Node_Kind is
--    begin
--       return K_Param;
--    end Get_Kind;

   function Get_Kind (N : N_Exception) return Node_Kind is
   begin
      return K_Exception;
   end Get_Kind;

--    function Get_Kind (N : N_Member) return Node_Kind is
--    begin
--       return K_Member;
--    end Get_Kind;

--    function Get_Kind (N : N_Declarator) return Node_Kind is
--    begin
--       return K_Declarator;
--    end Get_Kind;

--    function Get_Kind (N : N_Union) return Node_Kind is
--    begin
--       return K_Union;
--    end Get_Kind;

--    function Get_Kind (N : N_Case) return Node_Kind is
--    begin
--       return K_Case;
--    end Get_Kind;

--    function Get_Kind (N : N_Or_Expr) return Node_Kind is
--    begin
--       return K_Or;
--    end Get_Kind;

--    function Get_Kind (N : N_Xor_Expr) return Node_Kind is
--    begin
--       return K_Xor;
--    end Get_Kind;

--    function Get_Kind (N : N_And_Expr) return Node_Kind is
--    begin
--       return K_And;
--    end Get_Kind;

--    function Get_Kind (N : N_Shl_Expr) return Node_Kind is
--    begin
--       return K_Shl;
--    end Get_Kind;

--    function Get_Kind (N : N_Shr_Expr) return Node_Kind is
--    begin
--       return K_Shr;
--    end Get_Kind;

--    function Get_Kind (N : N_Add_Expr) return Node_Kind is
--    begin
--       return K_Add;
--    end Get_Kind;

--    function Get_Kind (N : N_Sub_Expr) return Node_Kind is
--    begin
--       return K_Sub;
--    end Get_Kind;

--    function Get_Kind (N : N_Mul_Expr) return Node_Kind is
--    begin
--       return K_Mul;
--    end Get_Kind;

--    function Get_Kind (N : N_Div_Expr) return Node_Kind is
--    begin
--       return K_Div;
--    end Get_Kind;

--    function Get_Kind (N : N_Mod_Expr) return Node_Kind is
--    begin
--       return K_Mod;
--    end Get_Kind;

--    function Get_Kind (N : N_Neg_Expr) return Node_Kind is
--    begin
--       return K_Neg;
--    end Get_Kind;

--    function Get_Kind (N : N_Id_Expr) return Node_Kind is
--    begin
--       return K_Id;
--    end Get_Kind;

--    function Get_Kind (N : N_Not_Expr) return Node_Kind is
--    begin
--       return K_Not;
--    end Get_Kind;

--    function Get_Kind (N : N_Lit_Integer) return Node_Kind is
--    begin
--       return K_Lit_Integer;
--    end Get_Kind;

--    function Get_Kind (N : N_Lit_Floating_Point) return Node_Kind is
--    begin
--       return K_Lit_Floating_Point;
--    end Get_Kind;

--    function Get_Kind (N : N_Struct) return Node_Kind is
--    begin
--       return K_Struct;
--    end Get_Kind;

--    function Get_Kind (N : N_Enum) return Node_Kind is
--    begin
--       return K_Enum;
--    end Get_Kind;

--    function Get_Kind (N : N_Enumerator) return Node_Kind is
--    begin
--       return K_Enumerator;
--    end Get_Kind;

--    function Get_Kind (N : N_Type_Declarator) return Node_Kind is
--    begin
--       return K_Type_Declarator;
--    end Get_Kind;

--    function Get_Kind (N : N_Sequence) return Node_Kind is
--    begin
--       return K_Sequence;
--    end Get_Kind;

   function Get_Kind (N : N_Const) return Node_Kind is
   begin
      return K_Const;
   end Get_Kind;

--    function Get_Kind (N : N_Native) return Node_Kind is
--    begin
--       return K_Native;
--    end Get_Kind;

end Tree;

