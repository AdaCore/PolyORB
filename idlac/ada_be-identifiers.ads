with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Identifiers is

   function Ada_Name
     (Node : Node_Id)
     return String;
   --  Return the Ada name (unqualified) of N_Named node.

   function Ada_Full_Name
     (Node : Node_Id)
     return String;
   --  Return the Ada full name of N_Named Node.

end Ada_Be.Identifiers;
