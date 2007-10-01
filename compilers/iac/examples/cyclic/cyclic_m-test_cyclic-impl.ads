-------------------------------------------------
--  This file has been generated automatically
--  by IAC (Idl to Ada Compiler)
-------------------------------------------------
pragma Style_Checks
  ("NM32766");
with PortableServer;

package Cyclic_M.Test_Cyclic.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is
     access all Object'Class;

   function echoShortSeq
     (Self : access Object;
      arg : in Cyclic_M.Test_Cyclic.ShortSeq)
     return Cyclic_M.Test_Cyclic.ShortSeq;

private
   type Object is
     new PortableServer.Servant_Base with  record
         --  Insert components to hold the state of the implementation 
         --  object.
         null;
      end record;

end Cyclic_M.Test_Cyclic.Impl;
