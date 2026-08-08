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

with CORBA.Object;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);

package PortableInterceptor.Interceptor is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/Interceptor:1.0";

   name_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/Interceptor/name:1.0";

   function get_name
     (Self : Local_Ref)
     return CORBA.String;

end PortableInterceptor.Interceptor;
