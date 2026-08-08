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

with PolyORB.Exceptions;
with DynamicAny.DynAnyFactory.Impl;

package body DynamicAny.DynAnyFactory is

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynAnyFactory.InconsistentTypeCode_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   --------------------
   -- create_dyn_any --
   --------------------

   function create_dyn_any
     (Self : Local_Ref;
      value : CORBA.Any)
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
      return DynamicAny.DynAnyFactory.Impl.create_dyn_any
        (DynamicAny.DynAnyFactory.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end create_dyn_any;

   ---------------------------------------
   -- create_dyn_any_without_truncation --
   ---------------------------------------

   function create_dyn_any_without_truncation
     (Self : Local_Ref;
      value : CORBA.Any)
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
      return DynamicAny.DynAnyFactory.Impl.create_dyn_any_without_truncation
        (DynamicAny.DynAnyFactory.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end create_dyn_any_without_truncation;

end DynamicAny.DynAnyFactory;
