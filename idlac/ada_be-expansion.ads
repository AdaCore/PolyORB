with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Expansion is

   procedure Expand_Repository (Node : in out N_Repository_Acc);

private

   --  generic function that calls the most specific one
   --  according to the type of the node
   --  Named_Nodes_In_Scope accumulates all the named nodes
   --  that could create an identifier conflict in ada
   procedure Expand_Node (Node : in out N_Root_Acc;
                          Named_Nodes_In_Scope : in Node_List);


   procedure Expand_Repository (Node : in out N_Root_Acc);


   --  function Expand_Module (Node : in N_Root_Acc)
   --                        return N_Root_Acc;

end Ada_Be.Expansion;
