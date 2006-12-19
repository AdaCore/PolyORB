-------------------------------------------------
--  This file has been generated automatically
--  by IAC (Idl to Ada Compiler)
-------------------------------------------------
pragma Style_Checks
  ("NM32766");
with Cyclic_M.Cyclic_I.Skel;
pragma Unreferenced (Cyclic_M.Cyclic_I.Skel);

package body Cyclic_M.Cyclic_I.Impl is

   -----------------------
   -- echoForwardStruct -- 
   -----------------------

   function echoForwardStruct
     (Self : access Object;
      arg : in Cyclic_M.Cyclic_I.ForwardStruct)
     return Cyclic_M.Cyclic_I.ForwardStruct
   is
     pragma Unreferenced (Self);
   begin
      return arg;
   end echoForwardStruct;

end Cyclic_M.Cyclic_I.Impl;
