with Lexer;     use Lexer;
with Types;     use Types;

with Backend.BE_A.Nodes; use Backend.BE_A.Nodes;
with Frontend.Nodes;

package Backend.BE_A.Nutils is

   package FEN renames Frontend.Nodes;

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);

   procedure Push_Entity (E : Node_Id);
   procedure Pop_Entity;
   function  Current_Entity return Node_Id;
   function  Current_Package return Node_Id;

   procedure Declare_CORBA_Type (K : FEN.Node_Kind; S : String := "");
   --  Declare CORBA type as predefined Ada type.

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return Node_Id;
   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return List_Id;

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Parameter_Mode (E : Node_Id) return Mode_Type;
   procedure Set_Parameter_Mode (E : Node_Id; M : Mode_Type);

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   procedure Set_Impl_Body (N : Node_Id := No_Node);
   procedure Set_Impl_Spec (N : Node_Id := No_Node);

   procedure Set_Main_Body (N : Node_Id := No_Node);
   procedure Set_Main_Spec (N : Node_Id := No_Node);

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := 0)
      return                Node_Id;

   function Make_Subprogram_Specification
     (Defining_Identifier : Node_Id;
      Parameter_Profile   : List_Id;
      Return_Type         : Node_Id := No_Node)
     return Node_Id;

   function To_Ada_Name (N : Name_Id) return Name_Id;

   function Make_Defining_Identifier
     (Entity : Node_Id)
      return Node_Id;

   function Make_Defining_Identifier
     (Name : Name_Id)
     return  Node_Id;

   function Make_Designator
     (Entity : Node_Id)
     return Node_Id;

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id)
     return Node_Id;

   function Make_Enumeration_Type_Definition
     (Enumeration_Literals : List_Id)
     return Node_Id;

   function Make_Derived_Type_Definition
     (Is_Abstract_Type      : Boolean := False;
      Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id)
     return Node_Id;

   function Make_Record_Type_Definition
     (Is_Abstract_Type  : Boolean := False;
      Is_Tagged_Type    : Boolean := False;
      Is_Limited_Type   : Boolean := False;
      Record_Definition : Node_Id)
     return Node_Id;

   function Make_Record_Definition
     (Component_List : List_Id)
     return Node_Id;

end Backend.BE_A.Nutils;
