with Locations; use Locations;
with Nodes;     use Nodes;
with Types;     use Types;

package Nutils is

   procedure Append_Entity_To_List (E : Entity_Id; L : List_Id);
   --  Append node N to list L.

   procedure Remove_Entity_From_List (E : Entity_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function Is_A_Forward_Of (X, Y : Entity_Id) return Boolean;
   function Is_A_Scope (E : Entity_Id) return Boolean;
   function Is_A_Type (E : Entity_Id) return Boolean;
   function Is_Attribute_Or_Operation (E : Entity_Id) return Boolean;
   function Is_Interface_Redefinable_Entity (E : Entity_Id) return Boolean;

   function New_Entity (Kind : Node_Kind; Loc : Location) return Entity_Id;
   function New_Node   (Kind : Node_Kind; Loc : Location) return Node_Id;
   function New_List   (Kind : Node_Kind; Loc : Location) return List_Id;

   function New_Copy (E : Entity_Id) return Entity_Id;
   function New_Copy (N : Node_Id)   return Node_Id;

   procedure Associate (E : Entity_Id; N : Node_Id);

end Nutils;
