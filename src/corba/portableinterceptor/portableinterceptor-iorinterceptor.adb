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
with PortableInterceptor.IORInterceptor.Impl;

package body PortableInterceptor.IORInterceptor is

   --------------------------
   -- establish_components --
   --------------------------

   procedure establish_components
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
      PortableInterceptor.IORInterceptor.Impl.establish_components
        (PortableInterceptor.IORInterceptor.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         info);
   end establish_components;

end PortableInterceptor.IORInterceptor;
