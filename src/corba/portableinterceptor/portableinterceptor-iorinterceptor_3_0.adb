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

with CORBA.Object;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableInterceptor.IORInterceptor_3_0.Impl;

package body PortableInterceptor.IORInterceptor_3_0 is

   ----------------------------
   -- components_established --
   ----------------------------

   procedure components_established
     (Self : Local_Ref;
      info : PortableInterceptor.IORInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.IORInterceptor_3_0.Impl.components_established
        (PortableInterceptor.IORInterceptor_3_0.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         info);
   end components_established;

   -----------------------------------
   -- adapter_manager_state_changed --
   -----------------------------------

   procedure adapter_manager_state_changed
     (Self : Local_Ref;
      id : PortableInterceptor.AdapterManagerId;
      state : PortableInterceptor.AdapterState)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.IORInterceptor_3_0.Impl.adapter_manager_state_changed
        (PortableInterceptor.IORInterceptor_3_0.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id,
         state);
   end adapter_manager_state_changed;

end PortableInterceptor.IORInterceptor_3_0;
