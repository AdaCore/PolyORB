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

with PortableInterceptor.Interceptor;
with PolyORB.Std;
with PortableInterceptor.IORInfo;

package PortableInterceptor.IORInterceptor is

   type Local_Ref is
     new PortableInterceptor.Interceptor.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/IORInterceptor:1.0";

   procedure establish_components
     (Self : Local_Ref;
      info : PortableInterceptor.IORInfo.Local_Ref);

   establish_components_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/IORInterceptor/establish_components:1.0";

end PortableInterceptor.IORInterceptor;
