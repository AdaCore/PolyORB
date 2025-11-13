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
with PortableInterceptor.ORBInitInfo;

package PortableInterceptor.ORBInitializer is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitializer:1.0";

   procedure pre_init
     (Self : Local_Ref;
      info : PortableInterceptor.ORBInitInfo.Local_Ref);

   pre_init_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitializer/pre_init:1.0";

   procedure post_init
     (Self : Local_Ref;
      info : PortableInterceptor.ORBInitInfo.Local_Ref);

   post_init_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/ORBInitializer/post_init:1.0";

end PortableInterceptor.ORBInitializer;
