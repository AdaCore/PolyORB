with Locations; use Locations;
with Nodes;     use Nodes;
with Types;     use Types;

package Nutils is

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   --  Append node N to list L.

   procedure Insert_After_Node (E : Node_Id; N : Node_Id);
   --  Insert node E after node N

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function Is_A_Forward_Of (X, Y : Node_Id) return Boolean;
   function Is_A_Scope (E : Node_Id) return Boolean;
   function Is_A_Type (E : Node_Id) return Boolean;
   function Is_Attribute_Or_Operation (E : Node_Id) return Boolean;
   function Is_Interface_Redefinable_Node (E : Node_Id) return Boolean;

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   function New_List (Kind : Node_Kind; Loc : Location) return List_Id;

   function New_Copy (N : Node_Id)   return Node_Id;

   procedure Bind_Identifier_To_Entity  (N : Node_Id; E : Node_Id);
   procedure Bind_Declarator_To_Entity  (D : Node_Id; E : Node_Id);
   procedure Bind_Declarators_To_Entity (D : List_Id; E : Node_Id);

   function Make_Scoped_Name
     (Loc        : Location;
      Identifier : Node_Id;
      Parent     : Node_Id)
     return Node_Id;
   --  Return a scoped name

   function Make_Identifier
     (Loc        : Location;
      Name       : Name_Id;
      IDL_Name   : Name_Id)
     return Node_Id;
   --  return identifier

   function Make_Constant_Declaration
     (Loc        : Location;
      Type_Spec  : Node_Id;
      Identifier : Node_Id;
      Expression : Node_Id)
     return Node_Id;
   --  Return constant declaration

end Nutils;
