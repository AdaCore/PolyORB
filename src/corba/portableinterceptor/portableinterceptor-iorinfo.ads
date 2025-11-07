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
with CORBA.Policy;
with IOP;

package PortableInterceptor.IORInfo is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/IORInfo:1.0";

   function get_effective_policy
     (Self : Local_Ref;
      IDL_type : CORBA.PolicyType)
     return CORBA.Policy.Ref;

   get_effective_policy_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/IORInfo/get_effective_policy:1.0";

   procedure add_ior_component
     (Self : Local_Ref;
      a_component : IOP.TaggedComponent);

   add_ior_component_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/IORInfo/add_ior_component:1.0";

   procedure add_ior_component_to_profile
     (Self : Local_Ref;
      a_component : IOP.TaggedComponent;
      profile_id : IOP.ProfileId);

   add_ior_component_to_profile_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/PortableInterceptor/IORInfo/add_ior_component_to_profile:1.0";

end PortableInterceptor.IORInfo;
