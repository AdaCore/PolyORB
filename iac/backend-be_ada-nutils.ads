with Types; use Types;
with Locations; use Locations;
with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;
package Backend.BE_Ada.Nutils is

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   function New_List (Kind : Node_Kind; Loc  : Location) return List_Id;

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   --  Append node N to list L.

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty
   function Map_Id_Name_Idl2Ada (N : Name_Id) return Name_Id;

end Backend.BE_Ada.Nutils;
