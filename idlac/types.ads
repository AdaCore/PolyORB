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

with Errors;

package Types is

   ------------------------
   --  type definitions  --
   ------------------------

   --  used for the identifiers
   type String_Cacc is access constant String;


   --  all the possible kinds of node
   type Node_Kind is
      (K_Repository,
       K_Scoped_Name,
       K_Module,
       K_Interface,
       K_Forward_Interface,
       K_Operation,
       K_Attribute,
       K_Void,
       K_Float,
       K_Double,
       K_Long_Double,
       K_Short,
       K_Long,
       K_Long_Long,
       K_Unsigned_Short,
       K_Unsigned_Long,
       K_Unsigned_Long_Long,
       K_Char,
       K_Wchar,
       K_Boolean,
       K_Octet,
       K_Any,
       K_Object,
       K_String,
       K_Native,
       K_Param,
       K_Exception,
       K_Member,
       K_Declarator,
       K_Type_Declarator,
       K_Const,
       K_Union,
       K_Case,
       K_Sequence,
       K_Struct,
       K_Enum,
       K_Enumerator,
       K_Or,                   --  Binary operators.
       K_And,
       K_Xor,
       K_Sub,
       K_Add,
       K_Shr,
       K_Shl,
       K_Mul,
       K_Div,
       K_Mod,
       K_Id,                   --  Unary operators.
       K_Neg,
       K_Not,
       K_Lit_Integer,          --  Literals.
       K_Lit_Floating_Point,
       K_Lit_Fixed_Point,
       K_Lit_Char,
       K_Lit_Wchar,
       K_Lit_String,
       K_Lit_Wstring,
       K_Lit_True,
       K_Lit_False
       );


   --  The basic type of the tree. Every Node type is a descendant
   --  of this one.
   --  In all this file, N_ means that the type is a node type
   type N_Root is abstract tagged private;
   type N_Root_Acc is access all N_Root'Class;
   Nil_Node : constant N_Root_Acc;


   --  Basic type for a named_node.
   --  An identifier can be attached only to a named node.
   --  Ie, the meaning of an identifier is a named node.
   --  However, a named node can be anonymous.
   --  FIXME : bizarre ce commentaire...
   type N_Named is abstract new N_Root with private;
   type N_Named_Acc is access all N_Named'Class;


   --  Basic type for nodes that define a scope
   type N_Scope is abstract new N_Named with private;
   type N_Scope_Acc is access all N_Scope'Class;


   --  this type represents an identifier definition
   type Identifier_Definition is private;
   type Identifier_Definition_Acc is access Identifier_Definition;



   ---------------------------
   --  operations on Nodes  --
   ---------------------------

   --  To manipulate the location of a node
   procedure Set_location (N : in out N_Root'Class; Loc : Errors.Location);
   function Get_location (N : N_Root'Class) return Errors.Location;

   --  To get the name of a named node
   function Get_Name (Node : in N_Named'Class) return String;

   --  To get the kind of a node. Each node type has to redifine
   --  this method, returning the right type.
   --  For a N_root, this method is abstract
   function Get_Kind (N : N_Root) return Node_Kind is abstract;



   --------------------------------------------
   --  operations on identifier definitions  --
   --------------------------------------------

   --  Return the named node corresponding to the identifier
   --  definition.
   --  Raises fatal_error if Cell is a null pointer
   function Get_Node (Cell : Identifier_Definition_Acc) return N_Named_Acc;



   ----------------------
   --  scope handling  --
   ----------------------

   --  Scopes are stacked and create an identifier space.
   --  In a scope, an identifier has at most one meaning : a node.
   --  Therefore, there is no overload.
   --  For the research, the current scope is considered, and if an
   --  identifier has no meaning in the scope, the parent scope is
   --  looked for.

   --  Get the root (the oldest) and current (the newest) scope.
   function Get_Root_Scope return N_Scope_Acc;
   function Get_Current_Scope return N_Scope_Acc;

   --  Create a new scope, defined by a node, add it in the current scope,
   --  and activate it.
   procedure Push_Scope (Scope : access N_Scope'Class);

   --  Unstack the current scope.  SCOPE is used to check coherency.
   procedure Pop_Scope (Scope : access N_Scope'Class);



   ----------------------------
   --  identifiers handling  --
   ----------------------------

   --  Identifiers are numbered, in order to make comparaison easier and
   --  static.
   --  Package Idents provide the list of well-known identifiers.
   type Uniq_Id is new Natural;


   --  Get the uniq_id from a string.
   function Add_Identifier (Id : String_Cacc) return Uniq_Id;
   function Get_identifier (Id : String) return Uniq_Id;

   --  Change the definition (associed node) of CELL.
   --  Must be called with care.
   procedure Redefine_Identifier
     (Cell : Identifier_Definition_Acc; Node : access N_Named'Class);

   --  Find the current identifier (this that was just scaned) and returns
   --  the node that defined it.
   --  Return null if no node defines it.
   function Find_Identifier return N_Named_Acc;

   function Find_Identifier return Identifier_Definition_Acc;

   --  Find the current identifier in a scope.
   function Find_Identifier_In_Scope (Scope : N_Scope_Acc)
                                      return N_Named_Acc;

   --  Add the current identifier in the current scope, with its node.
   --  Raise identifier_redefined if the identifier was already in the
   --  scope.
   Identifier_Redefined : exception;
   procedure Add_Identifier (Node : access N_Named'Class);

   --  Import an identifier from an other scope (this is checked) to the
   --  current scope.
   --  If the identifier was already defined (but not imported) in the current
   --  scope, this has no effect (except the check).
   --  If the identifier was already imported in the current scope, this
   --  cancel the meaning of the identifier, so that find_identifier returns
   --  null.  However, the identifier can be defined.
   procedure Import_Identifier (Node : N_Named_Acc);

   --  Import an identifier from an other scope to the current scope.
   --  The identifier must not have been defined (or it returns false), and
   --  can't be imported or defined.
   --  Return TRUE in case of success.
   function Import_Uniq_Identifier (Node : N_Named_Acc) return Boolean;


private

   type N_Root is abstract tagged record
      Loc : Errors.Location;
--      Back_End : N_Back_End_Acc := null;
   end record;

   Nil_Node : constant N_Root_Acc := null;

   type N_Named is abstract new N_Root with record
      Cell : Identifier_Definition_Acc;
   end record;

   type N_Scope is abstract new N_Named with record
      Identifier_List : Identifier_Definition_Acc;
   end record;

   --  A scope cells is directly linked to Node, to the node that has
   --  created the scope and the the previous scope cell (if the identifier
   --  has been redefined inside an enclosed scope).
   type Identifier_Definition is record
      --  Index of the identifier.
      --  This is mainly used to restore the previous meaning of the
      --  identifier, when the scope is exited.
      Identifier : Uniq_Id;

      --  The node refered by the identifier.
      Node : N_Named_Acc;

      --  The previous scope_cell, ie OLD is the previous meaning of the
      --  identifier.
      Old : Identifier_Definition_Acc;

      --  scope_cell of a scope are linked.
      Next : Identifier_Definition_Acc;

      --  cells with the same identifier are linked.
      Link : Identifier_Definition_Acc;

      --  Node that has created the scope.
      Parent : N_Scope_Acc;
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
--
--   INUTILE ???
--
--    subtype Binary_Node_Kind is Node_Kind range K_Or .. K_Mod;
--
--    --  The basic way to add back end information to a node.
--    type N_Back_End is abstract tagged private;
--    type N_Back_End_Acc is access all N_Back_End'Class;

--    --  The basic type of the tree.
--    type N_Root is abstract tagged private;

--    --  Primitives of a node.
--    procedure Set_Back_End (N : in out N_Root'Class;
--                            Be : access N_Back_End'Class);
--    function Get_Back_End (N : N_Root'Class) return N_Back_End_Acc;
--
--  private
--
--    type N_Back_End is abstract tagged null record;


end Types;
