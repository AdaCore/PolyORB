--  This package contains new nodes for the parse tree,
--  that are created only by the expansion

with Idl_Fe.Types;

package Ada_Be.Expansion.Types is

   --  when a node is expanded into several nodes,
   -- a node of this type is created
   type N_Node_List is new Idl_Fe.Types.N_Root with record
      Elements : Idl_Fe.Types.Node_List;
   end record;
   type N_Node_List_Acc is access all N_Node_List;


   --  a tree root (N_Repository) can be the result
   --  of several IDL files. During the expansion,
   --  the root (N_Repository) is rebuilt to contain
   --  only nodes of this type, that represents an IDL file
   type N_Idl_File is new Idl_Fe.Types.N_Named
     with null record;

end Ada_Be.Expansion.Types;
