package Idl_Fe.Tree.Low_Level is

   ----------------------------------------------------
   -- Low-level manipulations on the tree structure. --
   ----------------------------------------------------

   procedure Replace_Node
     (Old_Node : in out Node_Id;
      New_Node : in out Node_Id);
   --  Replaces Old_Node with New_Node in the hashtable.
   --  Sets the Origianal_Node attribute of New_Node to
   --  Old_Node.

   function Copy_Node
     (Old_Node : in Node_Id)
     return Node_Id;
   --  Create a (shallow) copy of Node.

end Idl_Fe.Tree.Low_Level;
