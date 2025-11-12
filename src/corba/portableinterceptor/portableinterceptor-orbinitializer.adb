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

with CORBA;
pragma Elaborate_All (CORBA);
with PortableInterceptor.ORBInitializer.Impl;

package body PortableInterceptor.ORBInitializer is

   --------------
   -- pre_init --
   --------------

   procedure pre_init
     (Self : Local_Ref;
      info : PortableInterceptor.ORBInitInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ORBInitializer.Impl.pre_init
        (PortableInterceptor.ORBInitializer.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         info);
   end pre_init;

   ---------------
   -- post_init --
   ---------------

   procedure post_init
     (Self : Local_Ref;
      info : PortableInterceptor.ORBInitInfo.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.ORBInitializer.Impl.post_init
        (PortableInterceptor.ORBInitializer.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         info);
   end post_init;

end PortableInterceptor.ORBInitializer;
