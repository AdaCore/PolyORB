pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/PortableInterceptor.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PortableInterceptor.IORInfo.Impl;

package body PortableInterceptor.IORInfo is

   --------------------------
   -- get_effective_policy --
   --------------------------

   function get_effective_policy
     (Self : Local_Ref;
      IDL_type : CORBA.PolicyType)
     return CORBA.Policy.Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.IORInfo.Impl.get_effective_policy
        (PortableInterceptor.IORInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         IDL_type);
   end get_effective_policy;

   -----------------------
   -- add_ior_component --
   -----------------------

   procedure add_ior_component
     (Self : Local_Ref;
      a_component : IOP.TaggedComponent)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.IORInfo.Impl.add_ior_component
        (PortableInterceptor.IORInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         a_component);
   end add_ior_component;

   ----------------------------------
   -- add_ior_component_to_profile --
   ----------------------------------

   procedure add_ior_component_to_profile
     (Self : Local_Ref;
      a_component : IOP.TaggedComponent;
      profile_id : IOP.ProfileId)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.IORInfo.Impl.add_ior_component_to_profile
        (PortableInterceptor.IORInfo.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         a_component,
         profile_id);
   end add_ior_component_to_profile;

end PortableInterceptor.IORInfo;
