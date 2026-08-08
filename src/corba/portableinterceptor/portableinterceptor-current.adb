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
with PortableInterceptor.Current.Impl;

package body PortableInterceptor.Current is

   --------------
   -- get_slot --
   --------------

   function get_slot
     (Self : Local_Ref;
      id : PortableInterceptor.SlotId)
     return CORBA.Any
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return PortableInterceptor.Current.Impl.get_slot
        (PortableInterceptor.Current.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id);
   end get_slot;

   --------------
   -- set_slot --
   --------------

   procedure set_slot
     (Self : Local_Ref;
      id : PortableInterceptor.SlotId;
      data : CORBA.Any)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PortableInterceptor.Current.Impl.set_slot
        (PortableInterceptor.Current.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         id,
         data);
   end set_slot;

end PortableInterceptor.Current;
