--  This package contains one function per node of the parse tree
with Idl_Fe.Types; use Idl_Fe.Types;

package Ada_Be.Idl2Ada is

   procedure Generate
     (Node      : in Node_Id;
      Implement : Boolean := False);
   --  Generate the Ada mapping of the IDL tree
   --  rooted at Node.
   --  If Implement is true, also produce a template
   --  for the Impl package of each interface, to
   --  be completed by the user.

end Ada_Be.Idl2Ada;
