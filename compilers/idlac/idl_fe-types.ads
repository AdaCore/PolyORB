------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . T Y P E S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Base types for the IDL front-end (standard version).

with Ada.Unchecked_Deallocation;
with Interfaces;

with Idlac_Errors;

package Idl_Fe.Types is

   -----------------------------
   -- Simple type definitions --
   -----------------------------

   type Node_Id is new Integer;
   No_Node : constant Node_Id := 0;

   function No (N : Node_Id) return Boolean;
   --  True when N is No_Node

   function Present (N : Node_Id) return Boolean;
   --  True when N is not No_Node

   --  used for the identifiers
   type String_Cacc is access constant String;

   --  Identifiers are numbered, in order to make comparaison
   --  easier and static. Each number is unique.
   type Uniq_Id is new Natural;
   Nil_Uniq_Id : constant Uniq_Id := 0;

   type Param_Mode is (Mode_In, Mode_Inout, Mode_Out);

   --  To manipulate the location of a node
   subtype Location is Idlac_Errors.Location;
   procedure Set_Location (N : Node_Id;
                           Loc : Location);
   function Get_Location (N : Node_Id) return Location;

   --  Version types for repository_ids
   type Version_Type is
      record
         Major : Interfaces.Unsigned_16;
         Minor : Interfaces.Unsigned_16;
      end record;

   function Image (V : Version_Type) return String;

   ----------------------------------
   --  Management of const values  --
   ----------------------------------

   --  all the possible kind of constants
   --  These types are used in the evaluation of constants to check
   --  that each subexpression of an expression does not exceed the
   --  precision of the final expression. In this context, there's
   --  no use to distinguish signed and unsigned integers (see CORBA
   --  V2.3 - 3.9.2), so C_Short for example could be a short or an
   --  unsigned short.
   --  In case a subexpression exceeds its supposed precision, the
   --  types C_general_... can be used to avoid further precision
   --  checking.
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
      C_Enum,
      C_No_Kind,
      C_General_Integer,
      C_General_Float,
      C_General_Fixed);

   --  Idl types.
   --  No distinction between intergers or floats here, the same
   --  type will be used for a short and a long long or for a float
   --  and a long double. However, the value will be checked and
   --  correspond to the type of the constant
   subtype Idl_Integer is Long_Long_Integer;
   type Idl_String is access String;
   type Idl_Wide_String is access Wide_String;
   subtype Idl_Character is Character;
   subtype Idl_Wide_Character is Wide_Character;
   subtype Idl_Float is Long_Long_Float;
   subtype Idl_Boolean is Boolean;

   --  To deallocate Idl strings
   procedure Free_Idl_String is new Ada.Unchecked_Deallocation
     (String, Idl_String);
   procedure Free_Idl_Wide_String is new Ada.Unchecked_Deallocation
     (Wide_String, Idl_Wide_String);

   --  These are the limits for each Idl type.
   --  This time, the different integer types are distinguished
   Idl_Octet_Min : constant Idl_Integer := 0;
   Idl_Octet_Max : constant Idl_Integer := (2 ** 8) - 1;
   Idl_Short_Min : constant Idl_Integer := (-2 ** 15);
   Idl_Short_Max : constant Idl_Integer := (2 ** 15) - 1;
   Idl_Long_Min : constant Idl_Integer := (-2 ** 31);
   Idl_Long_Max : constant Idl_Integer := (2 ** 31) - 1;
   Idl_LongLong_Min : constant Idl_Integer := (-2 ** 63);
   Idl_LongLong_Max : constant Idl_Integer := (2 ** 63) - 1;
   Idl_UShort_Min : constant Idl_Integer := 0;
   Idl_UShort_Max : constant Idl_Integer := (2 ** 16) - 1;
   Idl_ULong_Min : constant Idl_Integer := 0;
   Idl_ULong_Max : constant Idl_Integer := (2 ** 32) - 1;
   Idl_ULongLong_Min : constant Idl_Integer := 0;
   Idl_ULongLong_Max : constant Idl_Integer := Idl_ULong_Max
; --  (2 ** 64) - 1;
   Idl_Float_Min : constant Idl_Float := Long_Long_Float (Float'First);
   Idl_Float_Max : constant Idl_Float := Long_Long_Float (Float'Last);
   Idl_Double_Min : constant Idl_Float := Long_Long_Float (Long_Float'First);
   Idl_Double_Max : constant Idl_Float := Long_Long_Float (Long_Float'Last);
   Idl_Long_Double_Min : constant Idl_Float := Long_Long_Float'First;
   Idl_Long_Double_Max : constant Idl_Float := Long_Long_Float'Last;
   Idl_Enum_Max : constant Long_Long_Integer := (2 ** 32) - 1;

   --  definition of a constant, depending on its kind
   --  This type is also used to specify a constant type
   type Constant_Value (Kind : Const_Kind) is record
      case Kind is
         when C_Octet
           | C_Short
           | C_Long
           | C_LongLong
           | C_UShort
           | C_ULong
           | C_ULongLong
           | C_General_Integer =>
            Integer_Value : Idl_Integer;
         when C_Char =>
            Char_Value : Idl_Character;
         when C_WChar =>
            WChar_Value : Idl_Wide_Character;
         when C_Boolean =>
            Boolean_Value : Idl_Boolean;
         when C_Float
           | C_Double
           | C_LongDouble
           | C_General_Float =>
            Float_Value : Idl_Float;
         when C_String =>
            String_Length : Idl_Integer;
            String_Value : Idl_String;
         when C_WString =>
            WString_Length : Idl_Integer;
            WString_Value : Idl_Wide_String;
         when C_Fixed
           | C_General_Fixed =>
            Fixed_Value : Idl_Integer;
            Digits_Nb : Idl_Integer;
            Scale : Idl_Integer;
         when C_Enum =>
            Enum_Name : Node_Id;
            Enum_Value : Node_Id;
         when C_No_Kind =>
            null;
      end case;
   end record;
   type Constant_Value_Ptr is access Constant_Value;

   --  to duplicate a constant_value_ptr
   function Duplicate (C : Constant_Value_Ptr)
                       return Constant_Value_Ptr;

   --  to deallocate a constant_value_ptr
   procedure Free (C : in out Constant_Value_Ptr);

   ---------------------------------
   -- A useful list of root nodes --
   ---------------------------------

   type Node_List is private;
   --  A list of nodes.

   type Node_Iterator is private;
   --  An iterator on a node list.

   --  the empty list
   Nil_List : constant Node_List;

   function Head
     (NL : Node_List)
     return Node_Id;
   --  Return the first node in NL.

   function Is_Empty
     (NL : Node_List)
     return Boolean;
   --  True iff NL is empty.

   function Length
     (NL : Node_List)
     return Natural;
   --  The length of a list.

   --  Simple way to iterate over a node_list.
   --  NODE_ITERATOR is a type representing an iterator, which must
   --  be initialiazed by INIT.
   --  End of list is detected by IS_END.
   --  Until the end of list is reached, the node can be extracted
   --  with GET_NODE and the iterator can be incremented with NEXT.
   --  Therefore, usual way to use an iterator is:
   --  declare
   --    it: node_iterator;
   --    node: node_id;
   --  begin
   --    init (it, rep.contents);
   --    while not is_end (it) loop
   --      get_next_node (it, node);
   --      ...
   --    end loop;
   --  end;

   procedure Init
     (It : out Node_Iterator;
      List : Node_List);

   function Get_Node (It : Node_Iterator) return Node_Id;
   --  Get the current node corresponding to It, leaving It at its current
   --  position in the list.

   procedure Next (It : in out Node_Iterator);
   --  Advance It to the next position in the list

   procedure Get_Next_Node (It : in out Node_Iterator; Node : out Node_Id);
   --  Get the current node corresponding to It, and avande It to the next
   --  position in the list.

   function Is_End (It : Node_Iterator) return Boolean;
   --  Indicates when It has been advanced to the next position after the
   --  last node.

   procedure Append_Node (List : in out Node_List; Node : Node_Id);
   --  Append a node at the end of a list

   function Append_Node (List : Node_List; Node : Node_Id) return Node_List;
   --  Appends a node at the end of a list, and return the list.

   procedure Remove_Node (List : in out Node_List; Node : Node_Id);
   function Remove_Node (List : Node_List; Node : Node_Id) return Node_List;
   --  Remove the first occurrence of Node from List

   procedure Insert_Before
     (List   : in out Node_List;
      Node   : Node_Id;
      Before : Node_Id);
   --  Insert Node into List immediately before the first occurrence of Before

   procedure Insert_After
     (List : Node_List;
      Node : Node_Id;
      After : Node_Id);
   --  Insert Node into List immediately after the first occurrence of After

   function Is_In_List (List : Node_List; Node : Node_Id) return Boolean;
   --  Test whether node is in list

   function Is_In_Pointed_List
     (List : Node_List; Node : Node_Id) return Boolean;
   --  Look whether the entity denoted by scoped name Node is also denoted
   --  by an element of List (which must be scoped names as well).

   procedure Free (List : in out Node_List);
   --  Deallocate List

   function Simplify_Node_List (In_List : Node_List) return Node_List;
   --  Function that take a node list and remove all the redundant items
   --  returns the resulting node list
   --  useful for the inheritance treatement

   procedure Merge_List (Into : in out Node_List; From : Node_List);
   --  Appends all nodes of list From to list Into, unless they are already
   --  there.

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
      Node : Node_Id;
      Previous_Definition : Identifier_Definition_Acc;
      Parent_Scope : Node_Id;
   end record;

   --  Definition of a list of identifier_definition
   type Identifier_Definition_List is private;

   --  Return the named node corresponding to the identifier
   --  definition.
   --  Raises fatal_error if Cell is a null pointer
   function Get_Node
     (Definition : Identifier_Definition_Acc)
     return Node_Id;

   ----------------------
   --  scope handling  --
   ----------------------

   --  Scopes are stacked and create an identifier space.
   --  In a scope, an identifier has at most one meaning.

   function Get_Root_Scope return Node_Id;
   function Get_Current_Scope return Node_Id;
   --  Get the root (the oldest) and current (the newest) scope.

   function Get_Current_Gen_Scope
     return Node_Id;
   --  Return the current repository, idl file, module,
   --  interface or valuetype.

   function Get_Previous_Scope return Node_Id;
   --  Get the scope of the current scope

   procedure Push_Scope (Scope : Node_Id);
   --  Create a new scope, defined by a Scope node, add it in
   --  the current scope, and activate it.

   procedure Pop_Scope;
   --  Unstack the current scope.

   --  In order to ensure that each forward definition of a value
   --  or an interface is implemented in the same scope, here are
   --  some methods to take forward declarations and implementations
   --  into account

   procedure Add_Int_Val_Forward (Node : Node_Id);
   --  To add a forward declaration in the list

   procedure Add_Int_Val_Definition (Node : Node_Id);
   --  To take an implementation into account and remove the
   --  corresponding forward declaration from the list.

   --------------------------
   -- Identifiers handling --
   --------------------------

   function Is_Redefinable
     (Name  : String;
      Loc   : Idlac_Errors.Location;
      Scope : Node_Id := No_Node)
     return Boolean;
   --  Check if the name is redefinable in Scope or in the current scope
   --  (default). If result is false, means that Find_Identifier_Definition
   --  has a NOT NULL result!
   --  Loc is the location of the attempted redefinition.

   function Find_Identifier_Definition
     (Name : String;
      Loc  : Idlac_Errors.Location)
     return Identifier_Definition_Acc;
   --  Find the definition associated with the usage occurence of
   --  identifier Name located at Loc.
   --  If this identifier is not defined, returns a null pointer.

   function Find_Identifier_Node
     (Name : String;
      Loc  : Idlac_Errors.Location)
     return Node_Id;
   --  Find the node associated with the usage occurence of
   --  identifier Name located at Loc.
   --  If this identifier is not defined, returns a null pointer.

   procedure Redefine_Identifier
     (A_Definition : Identifier_Definition_Acc;
      Node : Node_Id);
   --  Change the definition (associed node) of CELL.
   --  only used in the case of a forward interface definition

   function Add_Identifier
     (Node  : Node_Id;
      Name  : String;
      Scope : Node_Id := No_Node;
      Is_Inheritable : Boolean := True)
     return Boolean;
   --  Creates an identifier definition for the current identifier
   --  and add it to scope Scope or the current scope if Scope is No_Node.
   --  Node is the node where the identifier is defined. If Is_Inheritable
   --  is False, then this identifier will not be considered when resolving
   --  names in scopes that inherit from this one.
   --  Returns true if successful, False if the identifier was
   --  already in this scope.

   function Find_Identifier_In_Storage
     (Scope            : Node_Id;
      Name             : String;
      Inheritable_Only : Boolean := False)
     return Identifier_Definition_Acc;
   --  Find the identifier definition in Scope. If Inheritable_Only,
   --  do not consider identifiers that were marked as not eligible
   --  for inheritance.
   --  If this identifier is not defined, returns a null pointer.

   function Find_Imported_Identifier_Definition
     (Name : String)
     return Identifier_Definition_Acc;
   --  Find the identifier definition in the imported table.
   --  If this identifier is not defined, returns a null pointer.

   procedure Add_Definition_To_Imported
     (Definition : Identifier_Definition_Acc;
      Scope : Node_Id);
   --  Add the imported definition to the given scope imported table.

   procedure Find_Identifier_In_Inheritance
     (Name : String;
      Scope : Node_Id;
      List : in out Node_List);
   --  Find the identifier in the scope's parents (in each one recursively)
   --  add the different definitions to the node list
   --  it is useful for looking in the inherited interfaces or value types

   -----------------------
   -- Identifiers table --
   -----------------------

   --  Each identifier is assigned a unique id number. This number is
   --  its location in the table of all the identifiers definitions:
   --  the Id_Table.
   --  In order to easily find a given identifier in the Id_Table,
   --  a hash table is used to store the mapping of identifier names
   --  to unique identifiers: the Hash_Table.

   --  The Hash_Table retains the position in the Id_Table of the first
   --  identifier defined for each possible hash value. All the
   --  identifiers having the same hash value are then linked: each one
   --  has a pointer on the next defined: Next.

   --  dimension of the hashtable
   type Hash_Value_Type is mod 2**32;

   --  dimension of the hashtable
   Hash_Mod : constant Hash_Value_Type := 2053;

   --  The hash table of the location of the identifiers in the
   --  id_table
   type Hash_Table_Type is array (0 .. Hash_Mod - 1) of Uniq_Id;
   Hash_Table : Hash_Table_Type := (others => Nil_Uniq_Id);

   --  An entry in the ID table, containing:
   --    - the Identifier_Definition;
   --    - a flag indicating whether this entry can be incorporated
   --      into another (interface or valuetype) scope by inheritance
   --      (meant for use only during expansion);
   --    - a pointer to the entry correponding to the next definition
   --      of an identifier with the same hash value.

   type Hash_Entry is record
      Definition     : Identifier_Definition_Acc
        := null;
      Is_Inheritable : Boolean := True;
      Next           : Uniq_Id;
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

   --  the table type that will be instantiated
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
   procedure Set_Last (T : in out Table; New_Val : Uniq_Id);

   --  Adds 1 to Last (same as Set_Last (Last + 1).
   procedure Increment_Last (T : in out Table);

   --  Subtracts 1 from Last (same as Set_Last (Last - 1).
   procedure Decrement_Last (T : in out Table);

   --  Adds Num to T.Last_val, and returns the old value of T.Last_Val + 1.
   procedure Allocate (T : in out Table;
                       Num : Integer := 1;
                       Result : out Uniq_Id);

   -------------------------------------------------
   --  the structure used for storing identifiers --
   -------------------------------------------------

   type Storage is record
      Hash_Table : Hash_Table_Type := (others => Nil_Uniq_Id);
      Content_Table : Table;
   end record;

   -----------------------------------
   --  dealing with Repository_Ids  --
   -----------------------------------

   procedure Set_Default_Repository_Id
     (Node : Node_Id);
   --  Set Node's default repository id.

   procedure Set_Initial_Current_Prefix
     (Node : Node_Id);
   --  Set the current prefix for scope Node
   --  from its parent's.

private

   ----------------------------------------
   --  Type of an identifier definition  --
   ----------------------------------------

   --  classical definition of a list for the identifier_definition_list
   type Identifier_Definition_Cell;
   type Identifier_Definition_List is access Identifier_Definition_Cell;
   type Identifier_Definition_Cell is record
      Definition : Identifier_Definition_Acc;
      Next : Identifier_Definition_List;
   end record;

   --  The hashing function. Takes an identifier and return its hash
   --  value
   function Hash (Str : String) return Hash_Value_Type;

   ---------------
   -- Node list --
   ---------------

   type Node_List_Cell;
   type Node_List is access Node_List_Cell;
   type Node_List_Cell is record
      Car : Node_Id;
      Cdr : Node_List;
   end record;

   Nil_List : constant Node_List := null;

   type Node_Iterator is new Node_List;

end Idl_Fe.Types;
