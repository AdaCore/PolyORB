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

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

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
