package body Nodes_Access is

   use Nodes_Table;

   function Make_Forward return Node_Id is
      Node  : constant Node_Access := new Node_Type;
      Index : constant Node_Id     := Allocate;
   begin
      Node.Kind := K_Forward;
      Table (Index) := Node;
      return Index;
   end Make_Forward;

   function Make_Named return Node_Id is
      Node  : constant Node_Access := new Node_Type;
      Index : constant Node_Id     := Allocate;
   begin
      Node.Kind := K_Named;
      Table (Index) := Node;
      return Index;
   end Make_Named;

   function Make_Repository return Node_Id is
      Node  : constant Node_Access := new Node_Type;
      Index : constant Node_Id     := Allocate;
   begin
      Node.Kind := K_Repository;
      Table (Index) := Node;
      return Index;
   end Make_Repository;

   function Make_Scope return Node_Id is
      Node  : constant Node_Access := new Node_Type;
      Index : constant Node_Id     := Allocate;
   begin
      Node.Kind := K_Scope;
      Table (Index) := Node;
      return Index;
   end Make_Scope;

   function Contents (N : Node_Id) return Node_List is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Repository);
      return Node.Contents;
   end Contents;

   procedure Set_Contents (N : Node_Id; V : Node_List)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Repository);
      Node.Contents := V;
   end Set_Contents;

   function Definition (N : Node_Id) return Identifier_Definition_Acc is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      return Node.Definition;
   end Definition;

   procedure Set_Definition (N : Node_Id; V : Identifier_Definition_Acc)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      Node.Definition := V;
   end Set_Definition;

   function Enclosing_Scope (N : Node_Id) return Node_Id is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      return Node.Enclosing_Scope;
   end Enclosing_Scope;

   procedure Set_Enclosing_Scope (N : Node_Id; V : Node_Id)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      Node.Enclosing_Scope := V;
   end Set_Enclosing_Scope;

   function Identifier_List (N : Node_Id) return Identifier_Definition_List is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      return Node.Identifier_List;
   end Identifier_List;

   procedure Set_Identifier_List (N : Node_Id; V : Identifier_Definition_List)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      Node.Identifier_List := V;
   end Set_Identifier_List;

   function Identifier_Table (N : Node_Id) return Storage is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      return Node.Identifier_Table;
   end Identifier_Table;

   procedure Set_Identifier_Table (N : Node_Id; V : Storage)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      Node.Identifier_Table := V;
   end Set_Identifier_Table;

   function Kind (N : Node_Id) return Node_Kind is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      return Node.Kind;
   end Kind;

   procedure Set_Kind (N : Node_Id; V : Node_Kind)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      Node.Kind := V;
   end Set_Kind;

   function Loc (N : Node_Id) return Location is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      return Node.Loc;
   end Loc;

   procedure Set_Loc (N : Node_Id; V : Location)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      Node.Loc := V;
   end Set_Loc;

   function Original_Node (N : Node_Id) return Node_Id is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      return Node.Original_Node;
   end Original_Node;

   procedure Set_Original_Node (N : Node_Id; V : Node_Id)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      Node.Original_Node := V;
   end Set_Original_Node;

   function Parent (N : Node_Id) return Node_Id is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      return Node.Parent;
   end Parent;

   procedure Set_Parent (N : Node_Id; V : Node_Id)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Named
             or else Node.Kind = Repository
             or else Node.Kind = Scope);
      Node.Parent := V;
   end Set_Parent;

   function Unimplemented_Forwards (N : Node_Id) return Node_List is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Repository);
      return Node.Unimplemented_Forwards;
   end Unimplemented_Forwards;

   procedure Set_Unimplemented_Forwards (N : Node_Id; V : Node_List)
   is
      Node : constant Node_Access := Retrieve_Node (N);
   begin
      pragma Assert (Node.Kind = Forward
             or else Node.Kind = Repository);
      Node.Unimplemented_Forwards := V;
   end Set_Unimplemented_Forwards;

end Nodes_Access;
