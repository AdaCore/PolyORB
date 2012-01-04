------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      F R O N T E N D . N U T I L S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Lexer;     use Lexer;
with Locations; use Locations;
with Types;     use Types;
with Values;

with Frontend.Nodes; use Frontend.Nodes;

package Frontend.Nutils is

   procedure Check_Identifier (Ref, Def : Node_Id);
   --  Return true when L and R have the same IDL names

   function First_Homonym (N : Node_Id) return Node_Id;
   procedure Set_First_Homonym (N : Node_Id; V : Node_Id);

   procedure Append_To (L : List_Id; E : Node_Id);
   --  Append node E to list L.

   procedure Insert_After_Node (E : Node_Id; N : Node_Id);
   --  Insert node E after node N

   procedure Insert_Before_Node
     (E : Node_Id; N : Node_Id; L : List_Id; Success : out Boolean);
   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id);
   --  Insert node E before node N in list L. The form with Success returns
   --  True in Success if N was inserted, and False if E is not in list L. The
   --  other one requires that E is in L.

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   --  This function returns a fully qualified name of an Identifier.
   function Fully_Qualified_Name
     (E : Node_Id;
      Separator : String := "::")
     return Name_Id;

   function Length (L : List_Id) return Natural;

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function Is_A_Forward_Of (X, Y : Node_Id) return Boolean;
   function Is_A_Scope (E : Node_Id) return Boolean;
   function Is_Type (E : Node_Id) return Boolean;
   function Is_Noninterface_Type (E : Node_Id) return Boolean;
   function Is_Attribute_Or_Operation (E : Node_Id) return Boolean;
   function Is_Interface_Redefinable_Node (E : Node_Id) return Boolean;
   function Is_A_Non_Module (E : Node_Id) return Boolean;
   function Is_A_Local_Type (E : Node_Id) return Boolean;
   function Is_Multidimensional_Array (D : Node_Id) return Boolean;

   function Is_Parent
     (Parent : Node_Id;
      Child  : Node_Id;
      First  : Boolean := False)
     return Boolean;
   --  This function returns True if the "Parent" node is a parent
   --  interface of the "Child" node. If First is true, the test is
   --  performed only at the first position in the interface spec of
   --  the child node.

   function Is_Redefined
     (Entity       : Node_Id;
      In_Interface : Node_Id)
     return Boolean;
   --  This function returns True if there is already an entity having
   --  the same name as "Entity" in "In_Interface". It returns False
   --  otherwise.

   function Get_Original_Type_Declarator (E : Node_Id) return Node_Id;
   --  This function returns the Original type declarator
   --  corresponding to the IDL node E. The original type declarator
   --  is a complex declarator, or a simple declarator for which the
   --  type specifier is *not* a scoped name.

   function Get_Original_Type_Declaration (E : Node_Id) return Node_Id;
   --  This function returns the type declaration node corresponding
   --  to the original type declarator of E.

   function Get_Original_Type_Specifier (E : Node_Id) return Node_Id;
   --  If the original type declarator of E is a simple declarator,
   --  this function return its type specifier. Otherwise it returns
   --  the original type declarator of E.

   function Has_Local_Component (E : Node_Id) return Boolean;
   --  Return True if the node E is a local interface, is defined
   --  basing on a local interface op else if it contains local
   --  interface as subcomponent (structure/.exception member or union
   --  element).

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   function New_List (Loc : Location) return List_Id;
   function New_Copy (N : Node_Id)   return Node_Id;

   procedure Bind_Identifier_To_Entity  (N : Node_Id; E : Node_Id);
   procedure Bind_Declarator_To_Entity  (D : Node_Id; E : Node_Id);
   procedure Bind_Declarators_To_Entity (D : List_Id; E : Node_Id);

   function Operator (E : Node_Id) return Operator_Type;
   procedure Set_Operator (E : Node_Id; O : Operator_Type);

   function Parameter_Mode (T : Token_Type) return Mode_Id;
   function Parameter_Mode (M : Mode_Id) return Token_Type;

   function Get_Pragma_Type (T : Token_Type) return Pragma_Type;
   function Get_Pragma_Type (P : Pragma_Type) return Token_Type;

   function Make_Scoped_Name
     (Loc        : Location;
      Identifier : Node_Id;
      Parent     : Node_Id;
      Reference  : Node_Id)
     return Node_Id;
   --  Return a scoped name

   function Make_Identifier
     (Loc          : Location;
      IDL_Name     : Name_Id;
      Node         : Node_Id;
      Scope_Entity : Node_Id)
     return Node_Id;
   --  return identifier

   function Make_Constant_Declaration
     (Loc        : Location;
      Type_Spec  : Node_Id;
      Identifier : Node_Id;
      Expression : Node_Id)
     return Node_Id;
   --  Return constant declaration

   function Expr_Value (N : Node_Id) return Value_Id;
   --  Returns the value of an expression, constant, or label node. This is
   --  just Frontend.Nodes.Value, except in the case of a K_Scoped_Name, in
   --  which case we have to get the Reference first. Possible alternative
   --  design: attach the Value attribute to scoped name nodes as well, and
   --  copy it over from the constant during analysis. We choose not to do that
   --  because scoped names can refer to other things as well -- things that
   --  have no "value".

   function Expr_Value (N : Node_Id) return Values.Value_Type;
   --  Same as previous Expr_Value, except fetches the Values.Value of the
   --  Valid_Id.

end Frontend.Nutils;
