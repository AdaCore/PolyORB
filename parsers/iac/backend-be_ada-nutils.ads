with GNAT.Table;

with Lexer;     use Lexer;
with Locations; use Locations;
with Types;     use Types;

with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;

package Backend.BE_Ada.Nutils is

   type Inheritance_Stack_Entry is record
      Node : Node_Id;
   end record;

   No_Inheritance_Depth : constant Int := -1;
   package Inheritance_Stack is
      new GNAT.Table
        (Inheritance_Stack_Entry, Int, No_Inheritance_Depth + 1, 10, 10);

   procedure Append_Node_To_List (E : Node_Id; L : in out List_Id);

   procedure Push_Package (E : Node_Id);
   procedure Pop_Package;
   function  Current_Package return Node_Id;

   function Package_Name      (E : Node_Id) return String;
   function Full_Package_Name (E : Node_Id) return String;

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   function New_List (Kind : Node_Kind; Loc  : Location) return List_Id;

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Parameter_Mode (E : Node_Id) return Mode_Type;
   procedure Set_Parameter_Mode (E : Node_Id; M : Mode_Type);

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function Make_Ada_Parameter
     (N : Node_Id; T : Node_Id; M : Mode_Id := 0) return Node_Id;
   --  Return a node representing : <N> : <M> <T> where N is a
   --  parameter name, M a parameter mode and T a parameter type. When
   --  M = 0 then use default Ada mode that is "in" mode.

   function Make_Ada_Identifier (N : Name_Id) return Node_Id;
   function Make_Ada_Identifier (S : String) return Node_Id;

   function Make_Subprogram_Spec
     (S : Node_Id;
      P : List_Id;
      T : Node_Id := No_Node)
     return Node_Id;
   --  Return a node representing :
   --  when T not null
   --     function <S> (<P>) return <T>;
   --  when T null
   --     procedure <S> (<P>);

   function Make_Enumeration_Type (L : List_Id) return Node_Id;

   function Make_Derived_Type_Declaration
     (Identifier_Node : Node_Id; Type_Spec_Node : Node_Id) return Node_Id;

   function Make_Type_Declaration
     (Type_Identifier : Node_Id; Type_Spec : Node_Id) return Node_Id;

   function To_Ada_Name (N : Name_Id) return Name_Id;
   function To_Ada_Name (N : String) return String;

end Backend.BE_Ada.Nutils;
