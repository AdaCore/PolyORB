with GNAT.Table;
with Types; use Types;
with Locations; use Locations;
with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;
package Backend.BE_Ada.Nutils is

   type Inheritance_Stack_Entry is record
      Node : Node_Id;
   end record;

   No_Inheritance_Depth : constant Int := -1;
   package Inheritance_Stack is
      new GNAT.Table
     (Inheritance_Stack_Entry, Int, No_Inheritance_Depth + 1, 10, 10);

   type Base_Type_Mapping is array (K_Float .. K_String) of Name_Id;
   Base_Type_Mapping_Table : Base_Type_Mapping;


   procedure Push_Package (E : Node_Id);
   procedure Pop_Package;
   function  Current_Package return Node_Id;
   --  Return current package
   function Package_Name (E : Node_Id) return String;
   function Full_Package_Name (E : Node_Id) return String;

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   function New_List (Kind : Node_Kind; Loc  : Location) return List_Id;

   procedure Append_Node_To_List (E : Node_Id; L : in out List_Id);
   --  Append node N to list L.
   --  Create the list if the list is null;

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function Map_Identifier_Name_2Ada (N : Name_Id) return Name_Id;

   function Map_Identifier_Name_2Ada (N : String) return String;

   function Mk_Node_Ada_Argument
     (I : Node_Id; T : Node_Id; M : Mode_Id) return Node_Id;
   function Mk_Node_Ada_Argument_List (L : List_Id) return List_Id;

   function Mk_Node_Ada_Function_Spec
     (Function_Id : Node_Id;
      Arg_List : List_Id; Return_Type : Node_Id) return Node_Id;

   function Mk_Node_Ada_Function
     (Function_Spec : Node_Id; Decl : List_Id;
                               Funct_Body : List_Id) return Node_Id;
   function Mk_Node_Ada_Identifier (Name : Name_Id) return Node_Id;
   function Mk_Node_Ada_Identifier (Name : String) return Node_Id;
   function Mk_Node_Ada_Procedure_Spec
     (Procedure_Id : Node_Id;
      Arg_List : List_Id) return Node_Id;
   function Mk_Node_Ada_Procedure
     (Proc_Spec : Node_Id; Decl : List_Id;
                           Proc_Body : List_Id) return Node_Id;




end Backend.BE_Ada.Nutils;
