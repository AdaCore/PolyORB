with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Expansion is

   procedure Expand_Repository (Node : in Node_Id);

private

   --  generic function that calls the most specific one
   --  according to the type of the node
   procedure Expand_Node (Node : in Node_Id);


   procedure Expand_Module (Node : in Node_Id);
   procedure Expand_Ben_Idl_File (Node : in Node_Id);
   procedure Expand_Interface (Node : in Node_Id);
   procedure Expand_Attribute (Node : in Node_Id);



   --  useful function to expand a whole list of nodes
   procedure Expand_Node_List (List : in Node_List);

   --  append a node to the contents node_list of parent
   procedure Append_Node_To_Contents (Parent : Node_Id;
                                      Child : Node_Id);

end Ada_Be.Expansion;
