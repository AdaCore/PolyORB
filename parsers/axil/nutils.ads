with Locations;    use Locations;
with Nodes;        use Nodes;
with Types;        use Types;

package Nutils is
   procedure Append_List_To_List (S : List_Id; D : in out List_Id);
   pragma Inline (Append_List_To_List);
   --  Append list S to list D, if D does not exist, then D := S

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   --  Append node N to list L.

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   --  Create a new node

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id;
   --  Create a new list

   function New_Copy (N : Node_Id) return Node_Id;

   function Make_Current_Identifier return Node_Id;
   --  Make an identifier from current token

   function Make_Identifier (Loc          : Location;
                             Name         : Name_Id;
                             Display_Name : Name_Id) return Node_Id;
   --  Make an identifier

end Nutils;
