with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Identifiers is

   --  returns a unique and valid ada identifier for this node
   --  with package prefix
   function Get_Ada_Full_Name (Node : in N_Named'Class)
     return String;

   --  returns a unique and valid ada identifier for this node
   --  *without* package prefix
   function Get_Ada_Name (Node : in N_Named'Class)
     return String;

end Ada_Be.Identifiers;
