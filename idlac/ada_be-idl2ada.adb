--  with Tree;

package body Ada_Be.Idl2ada is

   function Generate_Ada (Node : in N_Root_Acc)
                          return Ada_Be.Types.IDL_Scope is
      Result : Ada_Be.Types.IDL_Scope (new String '("toto"));
   begin
      return Result;
   end Generate_Ada;


end Ada_Be.Idl2ada;
