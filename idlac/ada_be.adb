with Ada_Be.Idl2ada;
with Ada_Be.Types;

package body Ada_Be is

   --------------------
   --  Generate_Code --
   --------------------
   procedure Generate_Code (Root : in N_Root_Acc) is
      Temp : Ada_Be.Types.IDL_Scope
        := Ada_Be.Idl2ada.Generate_Ada (Root);
   begin
      Ada_Be.Types.Dump_Code (Temp);
      --  Ada_Be.Types.Free (Temp);
   end Generate_Code;

end Ada_Be;
