--  Some code in this package is taken from GNAT's Atree.
--  Copyright (C) 1992-1998 Free Software Fundation, Inc.

----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  IDL syntax tree management.
--  $Id: //depot/ciao/main/ciao-idl_tree.ads#15 $

with GNAT.Table;

with Asis;

with CIAO.Alloc;
with CIAO.Types; use CIAO.Types;

package CIAO.IDL_Tree is

   -----------------------------------
   -- Types for an IDL syntax tree. --
   -----------------------------------

   --  A syntax tree is an instance of the Composite pattern,
   --  a technique that consists in recursively defining
   --  a data structure and the items that go within the
   --  structure in such a way that the structure and the
   --  items can be handled in the same way.

   --  To ensure the legality of the tree, fields of Node
   --  objects should not be accessed directly, and accessor
   --  functions should always be used.

   --  This package is modeled after GNAT's Atree, and some
   --  of it was directly incorporated.

   procedure Initialize;

   type Nkind is
     (N_Empty,
      N_Error,
      N_Unused_At_Start,

      --  Psuedo-nodes for preprocessor directives
      N_Preprocessor_Include,

      --  Keywords
      N_Keyword_Default,
      N_Keyword_Void,
      N_Keyword_In,
      N_Keyword_Out,
      N_Keyword_Inout,

      --  Base types
      N_Base_Type_Char,
      N_Base_Type_Boolean,
      N_Base_Type_Long,
      N_Base_Type_Double,
      N_Base_Type_Unsigned_Long,
      N_Base_Type_Long_Long,
      N_Base_Type_Long_Double,
      N_Base_Type_Unsigned_Long_Long,
      N_Base_Type_String,
      N_Base_Type_Octet,

      -- N_Identifier,
      N_Absolute,
      --  The "::" token.

      N_Specification,
      -- XXX N_Definition,
      N_Module,
      N_Interface,
      N_Interface_Dcl,
      N_Forward_Dcl,
      N_Interface_Header,
      N_Scoped_Name,
      N_Type_Dcl,
      N_Type_Declarator,
      N_Declarator,
      N_Simple_Declarator,
      N_Fixed_Array_Size,
      N_Type_Spec,
      N_Simple_Type_Spec,
      N_Constr_Type_Spec,
      N_Struct_Type,
      N_Union_Type,
      N_Member,
      N_Case_Element,
      N_Element_Spec,
      N_Enum_Type,
      N_Enumerator,
      N_Sequence_Type,
      N_Array_Declarator,
      N_Op_Dcl,
      N_Op_Type_Spec,
      N_Param_Type_Spec,
      N_Param_Dcl,
      N_Param_Attribute,
      N_Raises_Expr);

   subtype N_Base_Type_Spec is Nkind
     range N_Base_Type_Char .. N_Base_Type_Octet;
   --  All IDL base types.

   subtype N_Compound_Type is NKind
     range N_Struct_Type .. N_Union_Type;
   --  Types that contain objects of other types.

   function New_Node (New_Node_Kind : Nkind) return Node_Id;
   --  Allocate a new Node, and return its Node_Id

   function Node_Kind (N : Node_Id) return Nkind;
   pragma Inline (Node_Kind);
   --  Returns the Kind of node N

   function Origin (N : Node_Id) return Asis.Element;
   pragma Inline (Origin);
   --  Returns the Ada element of which node N is the translation, if any.

   function Parent (N : Node_Id) return Node_Id;
   pragma Inline (Parent);
   --  Returns the parent of a node if the node is not a list member, or
   --  else the parent of the list containing the node if the node is a
   --  list member.

   function No (N : Node_Id) return Boolean;
   pragma Inline (No);
   --  Tests given Id for equality with the Empty node. This allows notations
   --  like "if No (Variant_Part)" as opposed to "if Variant_Part = Empty".

   function Present           (N : Node_Id) return Boolean;
   pragma Inline (Present);
   --  Tests given Id for inequality with the Empty node. This allows notations
   --  like "if Present (Statement)" as opposed to "if Statement /= Empty".

   --  The fields of node N, typed as Node_Ids
   function Node1 (N : Node_Id) return Node_Id;
   pragma Inline (Node1);

   function Node2 (N : Node_Id) return Node_Id;
   pragma Inline (Node2);

   function Node3 (N : Node_Id) return Node_Id;
   pragma Inline (Node3);

   function Node4 (N : Node_Id) return Node_Id;
   pragma Inline (Node4);

   --  The fields of node N, typed as List_Ids
   function List1 (N : Node_Id) return List_Id;
   pragma Inline (List1);

   function List2 (N : Node_Id) return List_Id;
   pragma Inline (List2);

   function List3 (N : Node_Id) return List_Id;
   pragma Inline (List3);

   function List4 (N : Node_Id) return List_Id;
   pragma Inline (List4);

   --  The fields of node N, typed as Name_Ids
   function Name1 (N : Node_Id) return Name_Id;
   pragma Inline (Name1);

   --  The fields of node N, typed as Uints
   function Uint1 (N : Node_Id) return Uint;
   pragma Inline (Uint1);

   --  Boolean flags
   function Flag1 (N : Node_Id) return Boolean;
   pragma Inline (Flag1);

   -------------------------------------
   -- Corresponding Set_* subprograms --
   -------------------------------------

   procedure Set_Origin (N : Node_Id; Val : Asis.Element);
   pragma Inline (Set_Origin);

   procedure Set_Parent (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Parent);

   procedure Set_Node1 (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Node1);
   procedure Set_Node2 (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Node2);
   procedure Set_Node3 (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Node3);
   procedure Set_Node4 (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Node4);

   procedure Set_List1 (N : Node_Id; Val : List_Id);
   pragma Inline (Set_List1);
   procedure Set_List2 (N : Node_Id; Val : List_Id);
   pragma Inline (Set_List2);
   procedure Set_List3 (N : Node_Id; Val : List_Id);
   pragma Inline (Set_List3);
   procedure Set_List4 (N : Node_Id; Val : List_Id);
   pragma Inline (Set_List4);

   procedure Set_Name1 (N : Node_Id; Val : Name_Id);
   pragma Inline (Set_Name1);

   procedure Set_Uint1 (N : Node_Id; Val : Uint);
   pragma Inline (Set_Uint1);

   procedure Add_List1 (N : Node_Id; Val : Node_Id);
   pragma Inline (Add_List1);
   procedure Add_List2 (N : Node_Id; Val : Node_Id);
   pragma Inline (Add_List1);
   procedure Add_List3 (N : Node_Id; Val : Node_Id);
   pragma Inline (Add_List1);
   procedure Add_List4 (N : Node_Id; Val : Node_Id);
   pragma Inline (Add_List1);

   procedure Set_Flag1 (N : Node_Id; Val : Boolean);
   pragma Inline (Set_Flag1);

   -----------------------------
   -- Private Part Subpackage --
   -----------------------------

   --  The following package contains the definition of the data structure
   --  used by the implementation of the IDL_Tree package. Logically it really
   --  corresponds to the private part, hence the name. The reason that it
   --  is defined as a sub-package is to allow special access from clients
   --  that need to see the internals of the data structures.

   package IDL_Tree_Private_Part is
      type Node is record
         Kind    : Nkind := N_Empty;
         --  Indicates the kind of the node. Type Nkind is defined above.

         Origin : Asis.Element;
         --  The Ada element of which this node is the translation, if any.

         In_List : Boolean;
         --  True iff the node is part of a list.

         Link : Union_Id;
         --  This field is used either as the Parent pointer (if In_List
         --  is False), or to point to the list header (if In_List is
         --  True). This field is considered private and can be modified
         --  only by CIAO.IDL_Tree or by CIAO.Nlists.

         Field1  : Union_Id;
         Field2  : Union_Id;
         Field3  : Union_Id;
         Field4  : Union_Id;
         --  The actual usage of FieldN (i.e. whether it contains a Char_Code,
         --  Elist_Id, List_Id, Name_Id, Node_Id, String_Id, Uint or Ureal), depends
         --  on the value in Kind. Generally the access to this field is always via
         --  the functional interface, so the field names Char_CodeN, ElistN, ListN,
         --  NameN, NodeN, StrN, UintN and UrealN are used only in the bodies of the
         --  access functions (i.e. in the body of CIAO.IDL_Syntax). These access
         --  functions contain debugging code that checks that the use is consistent
         --  with the value in Kind.

         Flag1   : Boolean;
         --  Boolean flags (use depends on Nkind and Ekind, as described for Fieldn).
         --  Again the access is usually via subprograms in Sinfo and Einfo which
         --  provide high-level synonyms for these flags, and contain debugging
         --  code that checks that the values in Nkind and Ekind are appropriate
         --  for the access.

      end record;

      package Nodes is new GNAT.Table
        (Table_Component_Type => Node,
         Table_Index_Type     => Node_Id,
         Table_Low_Bound      => First_Node_Id,
         Table_Initial        => CIAO.Alloc.Nodes_Initial,
         Table_Increment      => CIAO.Alloc.Nodes_Increment);
   end IDL_Tree_Private_Part;

end CIAO.IDL_Tree;
