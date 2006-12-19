-------------------------------------------------
--  This file has been generated automatically
--  by IAC (Idl to Ada Compiler)
-------------------------------------------------
pragma Style_Checks
  ("NM32766");
with Cyclic_M.Test_Cyclic.Skel;
pragma Unreferenced (Cyclic_M.Test_Cyclic.Skel);

package body Cyclic_M.Test_Cyclic.Impl is

   ------------------
   -- echoShortSeq -- 
   ------------------

   function echoShortSeq
     (Self : access Object;
      arg : in Cyclic_M.Test_Cyclic.ShortSeq)
     return Cyclic_M.Test_Cyclic.ShortSeq
   is
     pragma Unreferenced (Self);
   begin
      return arg;
   end echoShortSeq;

end Cyclic_M.Test_Cyclic.Impl;
