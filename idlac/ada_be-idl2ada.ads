--  This package contains one function per node of the parse tree
with Ada_Be.Types;
--  with Types;

package Ada_Be.Idl2ada is

   function Generate_Ada (Node : in N_Root_Acc)
                          return Ada_Be.Types.IDL_Scope;


end Ada_Be.Idl2ada;
