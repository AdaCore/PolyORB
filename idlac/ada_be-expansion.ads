with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Expansion is

   procedure Expand_Repository (Node : in out N_Repository_Acc);

private

   --  generic function that calls the most specific one
   --  according to the type of the node
   procedure Expand_Node (Node : in out N_Root_Acc);


   procedure Expand_Repository (Node : in out N_Root_Acc);
   procedure Expand_Module (Node : in out N_Root_Acc);

   --  useful function to expand a whole list of nodes
   procedure Expand_Node_List (List : in out Node_List);


end Ada_Be.Expansion;
