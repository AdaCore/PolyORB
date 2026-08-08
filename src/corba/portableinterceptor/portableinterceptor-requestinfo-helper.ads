pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package PortableInterceptor.RequestInfo.Helper is

   TC_RequestInfo : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableInterceptor.RequestInfo.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableInterceptor.RequestInfo.Local_Ref;

   
   package Internals is

      procedure Initialize_RequestInfo;

   end Internals;

end PortableInterceptor.RequestInfo.Helper;
