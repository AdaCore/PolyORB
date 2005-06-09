with Types; use Types;

with Frontend.Nodes;

package Backend.BE_Ada.IDL_To_Ada is

   package FEN renames Frontend.Nodes;

   function Base_Type_TC
     (K : FEN.Node_Kind)
     return Node_Id;

   procedure Bind_FE_To_Impl
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Helper
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Skel
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_Stub
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_TC
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_From_Any
     (F : Node_Id;
      B : Node_Id);

   procedure Bind_FE_To_To_Any
     (F : Node_Id;
      B : Node_Id);

   function Is_Base_Type
     (N : Node_Id)
     return Boolean;

   function Is_N_Parent_Of_M
     (N : Node_Id;
      M : Node_Id)
     return Boolean;

   procedure Link_BE_To_FE
     (BE : Node_Id;
      FE : Node_Id);

   function Map_Accessor_Declaration
     (Accessor  : Character;
      Attribute : Node_Id)
     return Node_Id;

   function Map_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
     return Node_Id;

   function Map_Defining_Identifier
     (Entity : Node_Id)
     return Node_Id;

   function Map_Designator
     (Entity : Node_Id)
     return Node_Id;

   function Map_Fully_Qualified_Identifier
     (Entity : Node_Id)
     return Node_Id;

   function Map_IDL_Unit
     (Entity : Node_Id)
     return Node_Id;

   function Map_Members_Definition
     (Members : List_Id)
     return List_Id;

   function Map_Range_Constraints
     (Array_Sizes : List_Id)
     return List_Id;

   function Map_Repository_Declaration
     (Entity : Node_Id)
     return Node_Id;

   function Map_Variant_List
     (Alternatives   : List_Id;
      Literal_Parent : Node_Id := No_Node)
     return List_Id;

end Backend.BE_Ada.IDL_To_Ada;
