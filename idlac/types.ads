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

package Types is
   type String_Cacc is access constant String;
   Internal_Error : exception;
   Fatal_Error : exception;

   type Location is record
      --  Filename: String_Cacc;
      Line : Positive;
      Col : Natural;
   end record;

   type Node_Kind is
      (K_Repository, K_Scoped_Name,
       K_Module,
       K_Interface, K_Forward_Interface,
       K_Operation,
       K_Attribute,
       K_Void, K_Float, K_Double, K_Long_Double, K_Short, K_Long, K_Long_Long,
       K_Unsigned_Short, K_Unsigned_Long, K_Unsigned_Long_Long,
       K_Char, K_Wchar, K_Boolean, K_Octet, K_Any, K_Object,
       K_String, K_Native,
       K_Param,
       K_Exception, K_Member,
       K_Declarator, K_Type_Declarator, K_Const,
       K_Union, K_Case, K_Sequence, K_Struct,
       K_Enum, K_Enumerator,
   --  Binary operators.
       K_Or, K_And, K_Xor,
       K_Sub, K_Add,
       K_Shr, K_Shl,
       K_Mul, K_Div, K_Mod,
   --  Unary operators.
       K_Id, K_Neg, K_Not,
   --  Literals.
       K_Lit_Integer, K_Lit_Floating_Point, K_Lit_Fixed_Point, K_Lit_Char,
       K_Lit_Wchar, K_Lit_String, K_Lit_Wstring, K_Lit_True, K_Lit_False);

   subtype Binary_Node_Kind is Node_Kind range K_Or .. K_Mod;

   --  The basic way to add back end information to a node.
   type N_Back_End is abstract tagged private;
   type N_Back_End_Acc is access all N_Back_End'Class;

   --  The basic type of the tree.
   type N_Root is abstract tagged private;

   --  Primitives of a node.
   procedure Set_Back_End (N : in out N_Root'Class;
                           Be : access N_Back_End'Class);
   function Get_Back_End (N : N_Root'Class) return N_Back_End_Acc;

   procedure Set_Loc (N : in out N_Root'Class; Loc : Location);
   function Get_Loc (N : N_Root'Class) return Location;
   function Get_Kind (N : N_Root) return Node_Kind is abstract;

   type Node_Acc is access all N_Root'Class;
   Nil_Node : constant Node_Acc;

   --  An identifier refers to a scope_cell.
   --  A scope cells is directly linked to Node, to the node that has
   --   created the scope and the the previous scope cell (if the identifier
   --   has been redefined inside an enclosed scope).
   type Scope_Cell is private;
   type Scope_Cell_Acc is access Scope_Cell;

   --  An identifier can be attached only to a named node.
   --  Ie, the meaning of an identifier is a named node.
   --  However, a named node can be anonymous.
   type Named_Node is abstract new N_Root with private;
   type Named_Node_Acc is access all Named_Node'Class;
   function Get_Name (Node : in Named_Node'Class) return String;

   --  Return the named node from an identifier.
   --  CELL must not be null.
   function Get_Node (Cell : Scope_Cell_Acc) return Named_Node_Acc;

   --  Change the definition (associed node) of CELL.
   --  Must be called with care.
   procedure Redefine_Identifier
     (Cell : Scope_Cell_Acc; Node : access Named_Node'Class);

   --  A N_Scope defines a scope.
   type N_Scope is abstract new Named_Node with private;
   type N_Scope_Acc is access all N_Scope'Class;

   --  Scope handling.
   --  Scopes are stacked and create an identifiers space.
   --  In a scope, an identifier has at most one meaning, a node.
   --  Therefore, there is no overload.
   --  For the research, the current scope is considered, and if an identifier
   --  has no meaning in the scope, the parent scope is looked for.

   --  Get the root (the oldest) and current (the newest) scope.
   function Get_Root_Scope return N_Scope_Acc;
   function Get_Current_Scope return N_Scope_Acc;

   --  Create a new scope, defined by a node, add it in the current scope,
   --  and activate it.
   procedure Push_Scope (Scope : access N_Scope'Class);

   --  Unstack the current scope.  SCOPE is used to check coherency.
   procedure Pop_Scope (Scope : access N_Scope'Class);

   --  Identifiers are numbered, in order to make comparaison easier and
   --  static.
   --  Package Idents provide the list of well-known identifiers.
   type Uniq_Id is new Natural;

   --  Get the uniq_id from a string.
   function Add_Identifier (Id : String_Cacc) return Uniq_Id;
   function Get_identifier (Id : String) return Uniq_Id;

   --  Find the current identifier (this that was just scaned) and returns
   --  the node that defined it.
   --  Return null if no node defines it.
   function Find_Identifier return Named_Node_Acc;

   function Find_Identifier return Scope_Cell_Acc;

   --  Find the current identifier in a scope.
   function Find_Identifier_In_Scope (Scope : N_Scope_Acc)
                                      return Named_Node_Acc;

   --  Add the current identifier in the current scope, with its node.
   --  Raise identifier_redefined if the identifier was already in the
   --  scope.
   Identifier_Redefined : exception;
   procedure Add_Identifier (Node : access Named_Node'Class);

   --  Import an identifier from an other scope (this is checked) to the
   --  current scope.
   --  If the identifier was already defined (but not imported) in the current
   --  scope, this has no effect (except the check).
   --  If the identifier was already imported in the current scope, this
   --  cancel the meaning of the identifier, so that find_identifier returns
   --  null.  However, the identifier can be defined.
   procedure Import_Identifier (Node : Named_Node_Acc);

   --  Import an identifier from an other scope to the current scope.
   --  The identifier must not have been defined (or it returns false), and
   --  can't be imported or defined.
   --  Return TRUE in case of success.
   function Import_Uniq_Identifier (Node : Named_Node_Acc) return Boolean;

private
   type N_Back_End is abstract tagged null record;

   type N_Root is abstract tagged record
      Loc : Location;
      Back_End : N_Back_End_Acc := null;
   end record;

   Nil_Node : constant Node_Acc := null;

   type Named_Node is abstract new N_Root with record
      Cell : Scope_Cell_Acc;
   end record;

   type N_Scope is abstract new Named_Node with record
      Identifier_List : Scope_Cell_Acc;
   end record;

   --  Identifiers are looked for throught an hash table.
   --  Currently, the number of entries in the hash table is fixed, and
   --  collision are stored in another array.

   --  Overview of scope:
   --
   --  hash_table:
   --                 +---------------+-------------+-----------+
   --                 |  id_A         |    id_B     |  id_C     |
   --                 | link     mean |             |           |
   --                 +--+----------+-+-------------+-----------+
   --                    | ^        |         ^
   --        +-----------|-|--------|+        |
   --        V           V |        ||        |
   --  +-------+      +----+-----+  || +------+---+
   --  | Scope |<-----+ scope    |  |+-+ scope    |
   --  | node  |      |  cell    +--|->|  cell    +--->null
   --  +-------+      +--+-----+-+  |  +-+--------+
   --                    |     +----|-----------------> Node
   --                    |   +------+    |
   --                    V   V           V
   --                 +----------+      null
   --           <-----+ scope    |
   --                 |  cell    +----> null
   --                 +--+-----+-+
   --                    |     |
   --                    V     +----------------------> Node
   --                   null
   type Scope_Cell is record
      --  Index of the identifier.
      --  This is mainly used to restore the previous meaning of the
      --  identifier, when the scope is exited.
      Identifier : Uniq_Id;

      --  The node refered by the identifier.
      Node : Named_Node_Acc;

      --  The previous scope_cell, ie OLD is the previous meaning of the
      --  identifier.
      Old : Scope_Cell_Acc;

      --  scope_cell of a scope are linked.
      Next : Scope_Cell_Acc;

      --  cells with the same identifier are linked.
      Link : Scope_Cell_Acc;

      --  Node that has created the scope.
      Parent : N_Scope_Acc;
   end record;

end Types;
