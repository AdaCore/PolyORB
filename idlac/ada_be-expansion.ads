with Idl_Fe.Tree;

package Ada_Be.Expansion is

   function Expand_Repository (Node : in Idl_Fe.Tree.N_Root_Acc)
                               return Idl_Fe.Tree.N_Root_Acc;

private

   --  generic function that calls the most specific one
   --  according to the type of the node
   --  Named_Nodes_In_Scope accumulates all the named nodes
   --  that could create an identifier conflict in ada
   function Expand_Node (Node: in Idl_Fe.Types.N_Root_Acc;
                         Named_Nodes_In_Scope : in Node_List)
                         return Idl_Fe.Types.N_Root_Acc;


   function Expand_Module (Node : in Idl_Fe.Tree.N_Root_Acc)
                           return Idl_Fe.Tree.N_Root_Acc;


end Ada_Be.Expansion;
