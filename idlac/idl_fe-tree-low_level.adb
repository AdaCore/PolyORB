package body Idl_Fe.Tree.Low_Level is

   procedure Replace_Node
     (Old_Node : in out Node_Id;
      New_Node : in out Node_Id)
   is
      Temp_Node : Node_Access
        := Nodes_Table.Table (Old_Node);
      Temp_Id : Node_Id := Old_Node;
   begin
      Nodes_Table.Table (Old_Node) := Nodes_Table.Table (New_Node);
      Nodes_Table.Table (New_Node) := Temp_Node;
      Old_Node := New_Node;
      New_Node := Temp_Id;
      Set_Original_Node (New_Node, Old_Node);
   end Replace_Node;
   
   function Copy_Node
     (Old_Node : in Node_Id)
     return Node_Id
   is
      Node  : constant Node_Access := new Node_Type;
      Index : constant Node_Id     := Nodes_Table.Allocate;
   begin
      Node.all := Nodes_Table.Table (Old_Node).all;
      Nodes_Table.Table (Index) := Node;
      return Index;
   end Copy_Node;
   
end Idl_Fe.Tree.Low_Level;
