--  This package contains new nodes for the parse tree,
--  that are created only by the expansion

with Idl_Fe.Types;

package Ada_Be.Expansion.Types is

   type String_Ptr is access all String;

   type Be_Node_Kind is
      (Bek_Node_List,
       Bek_Idl_File);

   --  Back-End node
   type Ben_Root is abstract new Idl_Fe.Tree.N_Unknown with null record;
   function Get_Be_Kind (Node : Ben_Root) return Be_Node_Kind is abstract;
   --  procedure Display (Node : Ben_Root) is abstract;

   --  when a node is expanded into several nodes,
   --  a node of this type is created
   type Ben_Node_List is new Ben_Root with record
      Elements : Idl_Fe.Types.Node_List;
   end record;
   type Ben_Node_List_Acc is access Ben_Node_List;
   function Get_Be_Kind (Node : Ben_Node_List) return Be_Node_Kind;
   --  procedure Display (Node : Ben_Node_List);


   --  a tree root (N_Repository) can be the result
   --  of several IDL files. During the expansion,
   --  the root (N_Repository) is rebuilt to contain
   --  only nodes of this type, that represents an IDL file
   type Ben_Idl_File is new Ben_Root with record
      Name : String_Ptr;
      Elements : Idl_Fe.Types.Node_List;
   end record;
   type Ben_Idl_File_Acc is access Ben_Idl_File;
   function Get_Be_Kind (Node : Ben_Idl_File) return Be_Node_Kind;
   function New_Ben_Idl_File (Filename : String_Ptr) return Ben_Idl_File_Acc;
   --  procedure Display (Node : Ben_Idl_File);

end Ada_Be.Expansion.Types;
