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

   function Mk_Node_Ada_Argument
     (I : Node_Id; T : Node_Id; M : Mode_Id) return Node_Id;
   function Mk_Node_Ada_Argument_List (L : List_Id) return List_Id;

   function Mk_Node_Ada_Function_Spec
     (Arg_List : List_Id; Return_Type : Node_Id) return Node_Id;
   function Mk_Node_Ada_Function
     (Function_Spec : Node_Id; Decl : List_Id;
                               Funct_Body : List_Id) return Node_Id;
   function Mk_Node_Ada_Procedure_Spec
     (Arg_List : List_Id) return Node_Id;
   function Mk_Node_Ada_Procedure
     (Proc_Spec : Node_Id; Decl : List_Id;
                           Proc_Body : List_Id) return Node_Id;




end Backend.BE_Ada.Nutils;
