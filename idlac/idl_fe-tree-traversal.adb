with Idl_Fe.Types;
with Ada_Be.Identifiers; use Ada_Be.Identifiers;

--  This is the high-level interface to the tree structure.

package body Idl_Fe.Tree.Traversal is

   function Head
     (NL : Node_List)
     return N_Root_Acc
   is
      It : Node_Iterator;
   begin
      Init (It, NL);
      return Get_Node (It);
   end Head;

   function Contents
     (Node : N_Root_Acc)
     return Node_List
   is
   begin
      case Get_Kind (Node.all) is
         when K_Repository =>
            return N_Repository_Acc (Node).Contents;
         when K_Module =>
            return N_Module_Acc (Node).Contents;
         when K_Interface =>
            return N_Interface_Acc (Node).Contents;
         when K_ValueType =>
            return N_ValueType_Acc (Node).Contents;
         when others =>
            --  This kind of node does not have Contents.
            raise Program_Error;
      end case;
   end Contents;
   
   function Parents
     (Node : N_Root_Acc)
     return Node_List is
   begin
      return N_Interface_Acc (Node).Parents;
   end Parents;
   
   function Members
     (Node : N_Root_Acc)
     return Node_List is
   begin
      case Get_Kind (Node.all) is
	 when K_Exception =>
	    return N_Exception_Acc (Node).Members;
	 when K_Struct =>
	    return N_Struct_Acc (Node).Members;
	 when others =>
	    
	    raise Program_Error;
	    
      end case;
   end Members;
   
   function Decl
     (Node : N_Root_Acc)
     return Node_List is
   begin
      return N_Member_Acc (Node).Decl;
   end Decl;

   function Ada_Full_Name
     (Node : N_Root_Acc)
     return String is
   begin
      return Get_Ada_Full_Name (N_Named_Acc (Node).all);
   end Ada_Full_Name;
   
   function Ada_Name
     (Node : N_Root_Acc)
     return String
   is
      Full_Name : constant String
	:= Ada_Full_Name (Node);
      Last_Dot : Integer := Full_Name'First - 1;
   begin
      for I in Full_Name'Range loop
	 if Full_Name (I) = '.' then
	    Last_Dot := Integer (I);
	 end if;
      end loop;
      return Full_Name (Last_Dot + 1 .. Full_Name'Last);
   end Ada_Name;
   
   function M_Type
     (Node : N_Root_Acc)
     return N_Root_Acc is
   begin
      return N_Member_Acc (Node).M_Type;
   end M_Type;

end Idl_Fe.Tree.Traversal;
