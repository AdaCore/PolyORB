with Idl_Fe.Tree.Accessors;


package body Ada_Be.Expansion is

   ------------------------
   --  Expand_Repository --
   ------------------------

   function Expand_Repository (Node : in Idl_Fe.Tree.N_Root_Acc)
                               return Idl_Fe.Tree.N_Root_Acc is
      Named_Nodes_In_Scope : Node_List;
      Result : Idl_Fe.Types.N_Repository_Acc;
   begin
      pragma Assert (Idl_Fe.Types.Get_Kind (Node)
                     = Idl_Fe.Types.K_Repository);

      --  create the new node
      Result := new Idl_Fe.Types.N_Repository_Acc;
      Result.Old := Node;
      Result.Loc :=
      Idl_Fe.Tree.Accessors.Contents (
      --  create N_Idl_File nodes to put together declarations
      --  that were in the same file

      --  rename these N_Idl_File nodes

      --  expand these N_Idl_File nodes
      return Node;
   end Expand_Repository;


   -----------------
   --  Expand_Node --
   ------------------

   function Expand_Node (Node: in Idl_Fe.Types.N_Root_Acc;
                         Named_Nodes_In_Scope : in Node_List)
                         return Idl_Fe.Types.N_Root_Acc is
   begin
      case (Idl_Fe.Types.Get_Kind (Node))
         when K_Repository =>
           return Expand_Repository (Node));
         when others =>
            return Node;
      end case;
   end Expand_Node;


end Ada_Be.Expansion;
