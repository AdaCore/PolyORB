------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                      F R O N T E N D . N U T I L S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                           Copyright (c) 2005                             --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Lexer;     use Lexer;
with Locations; use Locations;
with Types;     use Types;

with Frontend.Nodes; use Frontend.Nodes;

package Frontend.Nutils is

   procedure Check_Identifier (Ref, Def : Node_Id);
   --  Return true when L and R have the same IDL names

   function First_Homonym (N : Node_Id) return Node_Id;
   procedure Set_First_Homonym (N : Node_Id; V : Node_Id);

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   --  Append node N to list L.

   procedure Insert_After_Node (E : Node_Id; N : Node_Id);
   --  Insert node E after node N

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id);
   --  Insert node E before node N in list L

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
   function Is_A_Type (E : Node_Id) return Boolean;
   function Is_Attribute_Or_Operation (E : Node_Id) return Boolean;
   function Is_Interface_Redefinable_Node (E : Node_Id) return Boolean;
   function Is_A_Non_Module (E : Node_Id) return Boolean;
   function Is_A_Local_Type (E : Node_Id) return Boolean;
   function Is_Multidimensional_Array (D : Node_Id) return Boolean;

   --  This function returns True if the "Parent" node is a parent interface of
   --  the "Child" node. If First is true, the test is performed only at the
   --  first position in the interface spec of the child node.
   function Is_Parent
     (Parent : Node_Id;
      Child  : Node_Id;
      First  : Boolean := False)
     return Boolean;

   function Is_Redefined
     (Entity       : Node_Id;
      In_Interface : Node_Id)
     return Boolean;
   --  This function returns True if there is already an entity having
   --  the same name as "Entity" in "In_Interface". It returns False
   --  otherwise.

   function Get_Original_Type (Param_Type : Node_Id) return Node_Id;
   --  This subprogram returns the original type of the given
   --  parameter. The node given as a parameter is a node of the IDL
   --  tree and the returned node is also a node from the IDL tree. If
   --  the given parameter is an array, the function return the
   --  corresponding complex declarator.

   function Get_Original_Type_Declaration
     (Param_Type : Node_Id)
     return Node_Id;
   --  This function returns the type declaration node corresponding
   --  to the original type 'Param_Type'

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   function New_List (Kind : Node_Kind; Loc : Location) return List_Id;

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

end Frontend.Nutils;
