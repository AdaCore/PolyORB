with GNAT.Table;
with Idl_Fe.Errors;
with Ada.Unchecked_Deallocation;

package Idl_Fe.Types is

   -------------------------------
   --  simple type definitions  --
   -------------------------------

   --  used for the identifiers
   type String_Cacc is access constant String;


   --  all the possible kinds of node
   type Node_Kind is
      (K_Repository,                --  structuring nodes
       K_Module,
       K_Interface,
       K_Forward_Interface,
       K_ValueType,
       K_Forward_ValueType,
       K_Boxed_ValueType,
       K_State_Member,
       K_Initializer,
       K_Operation,
--        K_Attribute,
       K_Param,
       K_Exception,
       K_Member,
       K_Declarator,
       K_Type_Declarator,
       K_Const_Dcl,
       K_Union,
       K_Case,
       K_Sequence,
       K_Struct,
       K_ValueBase,
       K_Enumerator,
       K_Native,
       K_Scoped_Name,
       K_Object,
       K_Any,
       K_Void,
       K_Fixed,
       K_Short,                     --  type nodes
       K_Long,
       K_Long_Long,
       K_Unsigned_Short,
       K_Unsigned_Long,
       K_Unsigned_Long_Long,
       K_Char,
       K_Wide_Char,
       K_Boolean,
       K_Float,
       K_Double,
       K_Long_Double,
       K_String,
       K_Wide_String,
       K_Octet,
       K_Enum,
       K_Or,                   --  Binary operators.
       K_Xor,
       K_And,
       K_Shr,
       K_Shl,
       K_Sub,
       K_Add,
       K_Mul,
       K_Div,
       K_Mod,
       K_Id,                   --  Unary operators.
       K_Neg,
       K_Not,
       K_Primary,              --  Primary expression
--        K_Lit_Integer,          --  Literals.
--        K_Lit_Floating_Point,
--        K_Lit_Fixed_Point,
--        K_Lit_Char,
--        K_Lit_Wchar,
       K_Lit_String  --  ,
--        K_Lit_Wstring,
--        K_Lit_True,
--        K_Lit_False
       );

   --  all the possible kind of constants
   type Const_Kind is
     (C_Short,
      C_Long,
      C_LongLong,
      C_UShort,
      C_ULong,
      C_ULongLong,
      C_Char,
      C_WChar,
      C_Boolean,
      C_Float,
      C_Double,
      C_LongDouble,
      C_Fixed,
      C_String,
      C_WString,
      C_Octet,
      C_Enum);

   --  type of a constant
   type Idl_Fixed_Digits_Nb is new Long_Long_Integer range 0 .. 31;
   type Idl_Fixed_Scale is new Long_Long_Integer range 0 .. 31;
   type Const_Type (Kind : Const_Kind) is record
      case Kind is
         when C_Fixed =>
            Digits_Nb : Idl_Fixed_Digits_Nb;
            Scale : Idl_Fixed_Scale;
         when others =>
            null;
      end case;
   end record;
   type Const_Type_Ptr is access Const_Type;

   --  to deallocate a const_type_ptr
   procedure Free is new Ada.Unchecked_Deallocation
     (Const_Type, Const_Type_Ptr);


   --  Identifiers are numbered, in order to make comparaison
   --  easier and static. Each number is unique.
   type Uniq_Id is new Natural;
   Nil_Uniq_Id : constant Uniq_Id := 0;




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
   procedure Set_Location (N : in out N_Root'Class;
                           Loc : Idl_Fe.Errors.Location);
   function Get_Location (N : N_Root'Class) return Idl_Fe.Errors.Location;


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

   --  computes the length of the list
   function Get_Length (List : in Node_List) return Integer;

   --  Function that take a node list and remove all the redondant items
   --  returns the resulting node list
   --  usefull for the inheritance treatement
   function Simplify_Node_List (In_List : Node_List) return Node_List;

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


   --------------------------------------------------------------
   --  Nodes defining a scope containig forward declaration    --
   --------------------------------------------------------------

   --  Basic type for nodes that define a scope
   type N_Forward is abstract new N_Scope with private;
   type N_Forward_Acc is access all N_Forward'Class;

   ---------------------------------------------------------------------
   --  Nodes defining a scope that could contain imported identifier  --
   ---------------------------------------------------------------------

   --  Basic type for nodes that define a scope
   type N_Imports is abstract new N_Scope with private;
   type N_Imports_Acc is access all N_Imports'Class;


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

   --  Return the identifier definition corresponding to the node
   --  Raises fatal_error if Node is a null pointer
   function Get_Definition (Node : N_Named_Acc)
                            return Identifier_Definition_Acc;

   --  Return the node list containing the inherited interfaces
   function Get_Parents (Node : N_Imports)
                         return Node_List is abstract;

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

--   function Find_Identifier_Node (Scope : N_Scope_Acc; Name : String)
--                                  return N_Named_Acc;


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


   --  Check if the  uniq_id from an identifier is already defined
   --  in the scope and return it or Nil_Uniq_Id
   function Check_Identifier_In_Storage (Scope : N_Scope_Acc;
                                         Identifier : String)
                                             return Uniq_Id;

   --  Find the identifier definition in Scope.
   --  If this identifier is not defined, returns a null pointer.
   function Find_Identifier_In_Storage (Scope : N_Scope_Acc; Name : String)
                                        return Identifier_Definition_Acc;

   --  Create the uniq_id entry for an identifier in the storage table
   --  at the end of the scope parsing
   --  return it
   function Create_Identifier_In_Storage (Identifier : String) return Uniq_Id;


   --  add the definition to the current scope storage table.
   --  It is done at the end of the scope parsing (called by pop_scope)
   procedure Add_Definition_To_Storage
     (Definition : in Identifier_Definition_Acc);

   --  Check if the  uniq_id from an identifier is already defined
   --  in the imported table.
   --  return it or Nil_Uniq_Id
   function Check_Imported_Identifier_Index (Identifier : String)
                                             return Uniq_Id;

   --  Find the identifier definition in the imported table.
   --  If this identifier is not defined, returns a null pointer.
   function Find_Imported_Identifier_Definition (Name : String)
                                          return Identifier_Definition_Acc;

   --  Create the uniq_id entry for an identifier in the imported table
   --  return it
   function Create_Identifier_In_Imported (Identifier : String) return Uniq_Id;


   --  add the imported definition to the current scope imported table.
   procedure Add_Definition_To_Imported
     (Definition : in Identifier_Definition_Acc);

   --  Find the identifier in the scope's parents (in each one recursively)
   --  add the different definitions to the node list
   --  it is usefull for looking in the inherited interfaces or value types
   procedure Find_Identifier_In_Inheritance (Name : in String;
                                             Scope : in N_Imports_Acc;
                                             List : in out Node_List);

   --  Find the identifier definition in the inherited interface.
   --  If this identifier is not defined, returns a null pointer.
   function Find_Inherited_Identifier_Definition (Name : String)
                                          return Identifier_Definition_Acc;

   ----------------------------
   --  identifiers handling  --
   ----------------------------

   --  Each identifier is given a unique id number. This number is
   --  its location in the table of all the identifiers definitions :
   --  the id_table.
   --  In order to find easily a given identifier in this id_table,
   --  an hashtable of the position of the identifiers in the
   --  id_table is maintained : the Hash_table. This one keeps the
   --  position in the id_table of the first identifier defined for
   --  each possible hash value. All the identifiers having the same
   --  hash_value are then linked : each one has a pointer on the
   --  next defined.

   --  dimension of the hashtable
   type Hash_Value_Type is mod 2**32;

   --  dimension of the hashtable
   Hash_Mod : constant Hash_Value_Type := 2053;

   --  The hash table of the location of the identifiers in the
   --  id_table
   type Hash_Table_Type is array (0 .. Hash_Mod - 1) of Uniq_Id;
   Hash_Table : Hash_Table_Type := (others => Nil_Uniq_Id);

   --  Type of an entry in the id_table.
   --  it contains the following :
   --    - the identifier_definition,
   --    - a pointer on the entry correponding to the definition
   --  of an identifier with the same hash value.
   type Hash_Entry is record
      Definition : Identifier_Definition_Acc := null;
      Next : Uniq_Id;
   end record;

   ----------------------------------
   --  The Gnat_Table adapted type --
   ----------------------------------
   --  This section provides an implementation of dynamically resizable one
   --  dimensional array type.The idea is to mimic the normal Ada semantics for
   --  arrays as closely as possible with the one additional capability of
   --  dynamically modifying the value of the Last attribute.


   --  we are defining the type of the table
   type Table_Type is
      array (Uniq_Id range <>) of Hash_Entry;

   subtype Big_Table_Type is
     Table_Type (Nil_Uniq_Id + 1 .. Uniq_Id'Last);

   --  The table is actually represented as a pointer to allow reallocation
   type Table_Ptr is access all Big_Table_Type;

   --  the table type that will be instanciated
   type Table is record
      --  the table
      Table : Table_Ptr := null;
      --  Subscript of the maximum entry in the currently allocated table
      Max : Integer := Integer (Nil_Uniq_Id);
      --  Number of entries in currently allocated table. The value of zero
      --  ensures that we initially allocate the table.
      Length : Integer := 0;
      --  Current value of Last.
      Last_Val : Integer := Integer (Nil_Uniq_Id);
   end record;

   --  the location of the first element of the table (it is constant)
   First : constant Uniq_Id := Nil_Uniq_Id + 1;

   --  Table expansion is permitted only if this switch is set to False. A
   --  client may set Locked to True, in which case any attempt to expand
   --  the table will cause an assertion failure. Note that while a table
   --  is locked, its address in memory remains fixed and unchanging.
   Locked : Boolean := False;

   --  This procedure allocates a new table of size Initial (freeing any
   --  previously allocated larger table). It is not necessary to call
   --  Init when a table is first instantiated (since reallocate works
   --  with a null table). However, it is harmless to do so, and
   --  Init is convenient in reestablishing a table for new use.
   procedure Init (T : in out Table);

   --  Returns the current value of the last used entry in the table, which
   --  can then be used as a subscript for Table. Note that the only way to
   --  modify Last is to call the Set_Last procedure. Last must always be
   --  used to determine the logically last entry.
   function Last (T : Table) return Uniq_Id;

   --  Storage is allocated in chunks according to the values given in the
   --  Initial and Increment parameters. A call to Release releases all
   --  storage that is allocated, but is not logically part of the current
   --  array value. Current array values are not affected by this call.
   procedure Release (T : in out Table);

   --  This procedure sets Last to the indicated value. If necessary the
   --  table is reallocated to accomodate the new value (i.e. on return
   --  the allocated table has an upper bound of at least Last). If Set_Last
   --  reduces the size of the table, then logically entries are removed
   --  from the table. If Set_Last increases the size of the table, then
   --  new entries are logically added to the table.
   procedure Set_Last (T : in out Table; New_Val : in Uniq_Id);

   --  Adds 1 to Last (same as Set_Last (Last + 1).
   procedure Increment_Last (T : in out Table);

   --  Subtracts 1 from Last (same as Set_Last (Last - 1).
   procedure Decrement_Last (T : in out Table);

   --  Adds Num to T.Last_val, and returns the old value of T.Last_Val + 1.
   procedure Allocate (T : in out Table;
                       Num : in Integer := 1;
                       Result : out Uniq_Id);

   -------------------------------------------------
   --  the structure used for storing identifiers --
   -------------------------------------------------
   type Storage is record
      Hash_Table : Hash_Table_Type := (others => Nil_Uniq_Id);
      Content_Table : Table;
   end record;

private

   --------------------------------------------
   --  Root of the tree parsed from the idl  --
   --------------------------------------------

   --  The basic node only contains its location (filename, line,
   --  column)
   type N_Root is abstract tagged record
      Loc : Idl_Fe.Errors.Location;
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
      Identifier_Table : Storage;
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

   --  Adds an identifier definition to a scope
   procedure Add_Identifier_Definition (Scope : in out N_Scope'Class;
                                        Identifier : in Identifier_Definition);

   -----------------------------------------------------
   --  Nodes defining particular scopes in the tree   --
   -----------------------------------------------------

   --  A scope that can contained interfaces and value types.
   --  These can be forward declared, thus the following field.
   --  This type represent for instance the repository or a module.
   type N_Forward is abstract new N_Scope with record
      Unimplemented_Forwards : Node_List := null;
   end record;

   --  the following node is a scope
   --  where you can import identifier from other scopes.
   --  It is for instance interfaces or value types.
   --  So it contains the imported identifiers hash table.
   --  This can also be forward declared.
   type N_Imports is abstract new N_Scope with record
      Imported_Table : Storage;
   end record;






   --  The hashing function. Takes an identifier and return its hash
   --  value
   function Hash (Str : in String) return Hash_Value_Type;

--
--   INUTILE ???
--
--   type N_Root is abstract tagged record
--      Loc : Idl_Fe.Errors.Location;
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


end Idl_Fe.Types;
