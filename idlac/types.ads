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

with GNAT.Table;
with Errors;

package Types is

   -------------------------------
   --  simple type definitions  --
   -------------------------------

   --  used for the identifiers
   type String_Cacc is access constant String;


   --  all the possible kinds of node
   type Node_Kind is
      (K_Repository,
       K_Scoped_Name,
       K_Module,
       K_Interface,
       K_Forward_Interface,
       K_ValueType,
       K_Forward_ValueType,
       K_Boxed_ValueType,
       K_State_Member,
       K_Initializer,
--        K_Operation,
--        K_Attribute,
--        K_Void,
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
       K_Wide_Char,
       K_Boolean,
       K_Octet,
       K_Any,
       K_Object,
       K_String,
       K_Wide_String,
       K_Native,
       K_Param,
       K_Exception,
       K_Member,
       K_Declarator,
       K_Type_Declarator,
       K_Const,
       K_Fixed,
       K_Union,
       K_Case,
       K_Sequence,
       K_Struct,
       K_Enum,
       K_ValueBase  --  ,
--        K_Enumerator,
--        K_Or,                   --  Binary operators.
--        K_And,
--        K_Xor,
--        K_Sub,
--        K_Add,
--        K_Shr,
--        K_Shl,
--        K_Mul,
--        K_Div,
--        K_Mod,
--        K_Id,                   --  Unary operators.
--        K_Neg,
--        K_Not,
--        K_Lit_Integer,          --  Literals.
--        K_Lit_Floating_Point,
--        K_Lit_Fixed_Point,
--        K_Lit_Char,
--        K_Lit_Wchar,
--        K_Lit_String,
--        K_Lit_Wstring,
--        K_Lit_True,
--        K_Lit_False
       );



   --  Identifiers are numbered, in order to make comparaison
   --  easier and static. Each number is unique.
   type Uniq_Id is new Natural;
   Nil_Uniq_Id : constant Uniq_Id;



   --------------------------------------------
   --  Root of the tree parsed from the idl  --
   --------------------------------------------

   --  The basic type of the tree. Every Node type is a descendant
   --  of this one.
   --  In all this file, N_ means that the type is a node type
   type N_Root is abstract tagged private;
   type N_Root_Acc is access all N_Root'Class;
   Nil_Node : constant N_Root_Acc;

   --  To get the kind of a node. Each node type has to redifine
   --  this method, returning the right type.
   --  For a N_root, this method is abstract
   function Get_Kind (N : N_Root) return Node_Kind is abstract;

   --  To manipulate the location of a node
   procedure Set_Location (N : in out N_Root'Class; Loc : Errors.Location);
   function Get_Location (N : N_Root'Class) return Errors.Location;


   ------------------------------------
   --  A usefull list of root nodes  --
   ------------------------------------

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

   --  Removes a node from the list. Actually only removes the first
   --  occurence of the node or does nothing if the node was not in
   --  the list.
   procedure Remove_Node (List : in out Node_List; Node : N_Root_Acc);

   --  Frees all the list
   procedure Free (List : in out Node_List);

   ---------------------------------------------------
   --  Named nodes in the tree parsed from the idl  --
   ---------------------------------------------------

   --  Basic type for a named_node.
   --  This is a node with a name, such as an identifier, a module,
   --  an interface, a function...
   type N_Named is abstract new N_Root with private;
   type N_Named_Acc is access all N_Named'Class;

   --  To get the name of a named node
   function Get_Name (Node : in N_Named'Class) return String;


   --------------------------------------------------------------
   --  Nodes defining a scope in the tree parsed from the idl  --
   --------------------------------------------------------------

   --  Basic type for nodes that define a scope
   type N_Scope is abstract new N_Named with private;
   type N_Scope_Acc is access all N_Scope'Class;


   ----------------------------------------
   --  Type of an identifier definition  --
   ----------------------------------------

   --  An identifier definition contains the following :
   --    - the name of the identifier
   --    - the uniq_id of the identifier
   --    - the node in which it was defined
   --    - the previous definition of the same identifier (if overloaded)
   --    - a pointer on the parent scope of the node
   type Identifier_Definition;
   type Identifier_Definition_Acc is access Identifier_Definition;
   type Identifier_Definition is record
      Name : String_Cacc := null;
      Id : Uniq_Id;
      Node : N_Named_Acc;
      Previous_Definition : Identifier_Definition_Acc;
      Parent_Scope : N_Scope_Acc;
   end record;

   --  Definition of a list of identifier_definition
   type Identifier_Definition_List is private;

   --  Return the named node corresponding to the identifier
   --  definition.
   --  Raises fatal_error if Cell is a null pointer
   function Get_Node (Definition : Identifier_Definition_Acc)
                      return N_Named_Acc;



   ----------------------
   --  scope handling  --
   ----------------------

   --  Scopes are stacked and create an identifier space.
   --  In a scope, an identifier has at most one meaning.

   --  Get the root (the oldest) and current (the newest) scope.
   function Get_Root_Scope return N_Scope_Acc;
   function Get_Current_Scope return N_Scope_Acc;

   --  Create a new scope, defined by a N_scope node, add it in
   --  the current scope, and activate it.
   procedure Push_Scope (Scope : access N_Scope'Class);

   --  Unstack the current scope.
   procedure Pop_Scope;

   --  In order to ensure that each forward definition of a value
   --  or an interface is implemented in the same scope, here are
   --  some methods to take forward declarations and implementations
   --  into account

   --  To add a forward declaration in the list
   procedure Add_Int_Val_Forward (Node : in N_Named_Acc);

   --  To take an implementation into account and remove the
   --  corresponding forward declaration from the list.
   procedure Add_Int_Val_Definition (Node : in N_Named_Acc);



   ----------------------------
   --  identifiers handling  --
   ----------------------------

   --  Check if the  uniq_id from an identifier is already defined
   --  return it or Nil_Uniq_Id
   function Check_Identifier_Index (Identifier : String) return Uniq_Id;

   --  Create the uniq_id entry for an identifier if it doesn't exist
   --  return it
   function Create_Identifier_Index (Identifier : String) return Uniq_Id;

   --  Find the current identifier definition.
   --  The current identifier is the one just scanned by the lexer
   --  If this identifier is not defined, returns a null pointer.
   function Find_Identifier_Definition (Name : String)
                                        return Identifier_Definition_Acc;

   --  Find the node corresponding to the current identifier.
   --  The current identifier is the one just scanned by the lexer
   --  If this identifier is not defined, returns a null pointer.
   function Find_Identifier_Node (Name : String) return N_Named_Acc;

   --  Change the definition (associed node) of CELL.
   --  only used in the case of a forward interface definition
   procedure Redefine_Identifier
     (Definition : Identifier_Definition_Acc; Node : access N_Named'Class);

   --  Creates an identifier definition for the current identifier
   --  and add it to the current scope.
   --  Node is the node where the identifier is defined.
   --  Returns true if successfull, false if the identifier was
   --  already in this scope.
   function Add_Identifier (Node : access N_Named'Class;
                            Name : String) return Boolean;


private

   --------------------------------------------
   --  Root of the tree parsed from the idl  --
   --------------------------------------------

   --  The basic node only contains its location (filename, line,
   --  column)
   type N_Root is abstract tagged record
      Loc : Errors.Location;
   end record;

   Nil_Node : constant N_Root_Acc := null;

   ------------------------------------
   --  A usefull list of root nodes  --
   ------------------------------------

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

   ---------------------------------------------------
   --  Named nodes in the tree parsed from the idl  --
   ---------------------------------------------------

   --  A named node contains its identifier definition
   type N_Named is abstract new N_Root with record
      Definition : Identifier_Definition_Acc;
   end record;

   --------------------------------------------------------------
   --  Nodes defining a scope in the tree parsed from the idl  --
   --------------------------------------------------------------

   --  A scope contains all the definitions of the enclosed
   --  identifiers as well as a list of the forwarded interfaces
   --  and values still not defined
   type N_Scope is abstract new N_Named with record
      Identifier_List : Identifier_Definition_List;
      Unimplemented_Forwards : Node_List := null;
   end record;

   ----------------------------------------
   --  Type of an identifier definition  --
   ----------------------------------------

   --  classical definition of a list for the identifier_definition_list
   type Identifier_Definition_Cell;
   type Identifier_Definition_List is access Identifier_Definition_Cell;
   type Identifier_Definition_Cell is record
      Definition : Identifier_Definition;
      Next : Identifier_Definition_List;
   end record;

   Nil_Uniq_Id : constant Uniq_Id := 0;

   --  Adds an identifier definition to a scope
   procedure Add_Identifier_Definition (Scope : in out N_Scope'Class;
                                        Identifier : in Identifier_Definition);

   ----------------------------
   --  identifiers handling  --
   ----------------------------

   --  dimension of the hashtable
   type Hash_Value_Type is mod 2**32;

   --  The hashing function. Takes an identifier and return its hash
   --  value
   function Hash (Str : in String) return Hash_Value_Type;


--
--   INUTILE ???
--
--   type N_Root is abstract tagged record
--      Loc : Errors.Location;
--      Back_End : N_Back_End_Acc := null;
--   end record;
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
--
--   function Add_Identifier (Id : String_Cacc) return Uniq_Id;
--
--
--    --  Import an identifier from an other scope (this is checked) to the
--    --  current scope.
--    --  If the identifier was already defined (but not imported) in the
--    --  current
--    --  scope, this has no effect (except the check).
--    --  If the identifier was already imported in the current scope, this
--    --  cancel the meaning of the identifier, so that find_identifier returns
--    --  null.  However, the identifier can be defined.
--    procedure Import_Identifier (Node : N_Named_Acc);
--
--    --  Import an identifier from an other scope to the current scope.
--    --  Return TRUE in case of success, false if the identifier was
--    --  already defined in the current scope and cannot be imported.
--    function Import_Uniq_Identifier (Node : N_Named_Acc) return Boolean;
--
   --  Find the node corresponding to the current identifier in a
   --  given scope.
   --  The current identifier is the one just scanned by the lexer
   --  If this identifier is not defined in the given scope,
   --  returns a null pointer.
--   function Find_Identifier_Node (Scope : N_Scope_Acc)
--                                  return N_Named_Acc;



end Types;
