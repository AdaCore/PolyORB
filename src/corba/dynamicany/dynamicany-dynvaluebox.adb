pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/DynamicAny.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA.Object;
with DynamicAny.DynValueBox.Impl;

package body DynamicAny.DynValueBox is

   ---------------------
   -- get_boxed_value --
   ---------------------

   function get_boxed_value
     (Self : Local_Ref)
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
      return DynamicAny.DynValueBox.Impl.get_boxed_value
        (DynamicAny.DynValueBox.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_boxed_value;

   ---------------------
   -- set_boxed_value --
   ---------------------

   procedure set_boxed_value
     (Self : Local_Ref;
      boxed : CORBA.Any)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynValueBox.Impl.set_boxed_value
        (DynamicAny.DynValueBox.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         boxed);
   end set_boxed_value;

   --------------------------------
   -- get_boxed_value_as_dyn_any --
   --------------------------------

   function get_boxed_value_as_dyn_any
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynValueBox.Impl.get_boxed_value_as_dyn_any
        (DynamicAny.DynValueBox.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_boxed_value_as_dyn_any;

   --------------------------------
   -- set_boxed_value_as_dyn_any --
   --------------------------------

   procedure set_boxed_value_as_dyn_any
     (Self : Local_Ref;
      boxed : DynamicAny.DynAny.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynValueBox.Impl.set_boxed_value_as_dyn_any
        (DynamicAny.DynValueBox.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         boxed);
   end set_boxed_value_as_dyn_any;

end DynamicAny.DynValueBox;
