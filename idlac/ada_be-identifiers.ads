with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Identifiers is

   function Ada_Name
     (Node : Node_Id)
     return String;
   --  The Ada name (unqualified) of K_Named node.

   function Ada_Full_Name
     (Node : Node_Id)
     return String;
   --  The Ada full name of K_Named Node.

   function Parent_Scope_Name
     (Node : Node_Id)
     return String;
   --  The Ada full name of the scope where K_Named
   --  Node is defined.

end Ada_Be.Identifiers;
