------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                I D L _ F E . T R E E . S Y N T H E T I C                 --
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

--  Synthetised attributes of the IDL tree nodes.

package Idl_Fe.Tree.Synthetic is

   ---------------------------------------
   -- Synthetic attributes of IDL nodes --
   ---------------------------------------

   function S_Type (Node : Node_Id) return Node_Id;
   --  If Node is a Scoped_Name whose Value declares a type, then S_Type
   --  returns that type, else S_Type is No_Node.

   function Default_Repository_Id (Node : Node_Id) return String;
   --  The string of "/"-separated identifiers that makes up the default
   --  repository id for Node.
   --  Must be called only by the parser, before the tree is expanded.

   function Is_Interface_Type
     (Node : Node_Id; Or_ValueType : Boolean := False) return Boolean;
   --  True iff Node is a <type_spec> that denotes an object reference type
   --  If Or_ValueType is True, also return true for valuetype reference types.

   function Is_Gen_Scope (Node : Node_Id) return Boolean;
   --  True iff Node is a generable Scope (ie K_Repository, K_Ben_Idl_File,
   --  K_Module, K_Interface or K_ValueType).

   function Name (Node : Node_Id) return String;
   --  The name of a K_Named node

   function Original_Operation_Type (Node : Node_Id) return Node_Id;
   --  The type that was initially declared for an operation.
   --  The type of a non-void operation that has inout or out arguments is
   --  changed to void by the expander; this returns the original, non-void
   --  type.

   function Original_Parent_Scope (Node : Node_Id) return Node_Id;
   --  The scope wherein a K_Named node was initially declared. This property
   --  never changes once it is set by the parser.
   --
   --  If Node is a Forward_Interface or Forward_ValueType that has a
   --  corresponding actual declaration, then the Name and
   --  Original_Parent_Scope returned are those of the actual declaration.

   function Parent_Scope (Node : Node_Id) return Node_Id;
   --  The scope wherein a K_Named node was declared. This property may be set
   --  explicitly by the expander using Set_Parent_Scope. Otherwise, it is
   --  equal to the Original_Parent_Scope of the node.

   procedure Set_Parent_Scope (Node : Node_Id; To : Node_Id);
   --  Explicitly change the parent scope of Node to To.
   --  Intended for use only by the expander.

   function Idl_Repository_Id (Node : Node_Id) return String;
   --  Return a Repository ID in OMG IDL format for K_Named Node
   --  (as defined in "10.6 RepositoryIds").

   function Version (Node : Node_Id) return String;
   --  Return the version part of Node's repository id.

   function All_Ancestors
     (Node : Node_Id;
      Exclude : Node_List := Nil_List) return Node_List;
   --  Return the list of all ancestors (direct or indirect) of K_Interface
   --  Node.
   --  If Exclude is not Nil_List, all nodes in Exclude are ignored during the
   --  exploration.  It is up to the caller to Free the returned Node_List
   --  after use.

   function Primary_Parent (Node : Node_Id) return Node_Id;
   --  Return the first non abstract parent interface for an interface node,
   --  and the first non abstract parent valuetype for a valuetype node.
   --  Returns No_Node if such a parent does not exist

   function Supports_Non_Abstract_Interface (Node : Node_Id) return Boolean;
   --  For a valuetype, returns true if it supports at least one
   --  non-abstract interface

   function Has_Interface_Component
     (Node   : Node_Id;
      I_Node : Node_Id) return Boolean;
   --  For a node that is a type, True if the type is I_Node, a typedef
   --  thereof, or a composite type that has one such component.

   function Has_Local_Component (Node : Node_Id) return Boolean;
   --  For a node that is a type, True if the type is a local interface, a
   --  forward declaration of a local interface, or a composite or constructed
   --  type that has one such component, or that has a component for which
   --  Has_Local_Component is True.

   function Integer_Value   (Node : Node_Id) return Idl_Integer;
   function Character_Value (Node : Node_Id) return Character;
   function Float_Value     (Node : Node_Id) return Idl_Float;
   function String_Value    (Node : Node_Id) return String;
   function WString_Value   (Node : Node_Id) return Wide_String;
   function Boolean_Value   (Node : Node_Id) return Boolean;
   function Enum_Value      (Node : Node_Id) return Node_Id;
   --  Return the value of a constant expression node as an
   --  {integer,char,string,boolean,enumerator}.

   procedure Set_String_Value (Node : Node_Id; Val : String);
   --  Set the value of a string node

   function Root_Type (Typ : Node_Id) return Node_Id;
   --  Return the ultimate type derivation ancestor of Typ (unwinding all
   --  typedefs and type references).

end Idl_Fe.Tree.Synthetic;
