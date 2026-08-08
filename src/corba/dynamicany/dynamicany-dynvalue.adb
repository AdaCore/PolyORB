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
with DynamicAny.DynValue.Impl;

package body DynamicAny.DynValue is

   -------------------------
   -- current_member_name --
   -------------------------

   function current_member_name
     (Self : Local_Ref)
     return DynamicAny.FieldName
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynValue.Impl.current_member_name
        (DynamicAny.DynValue.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end current_member_name;

   -------------------------
   -- current_member_kind --
   -------------------------

   function current_member_kind
     (Self : Local_Ref)
     return CORBA.TCKind
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynValue.Impl.current_member_kind
        (DynamicAny.DynValue.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end current_member_kind;

   -----------------
   -- get_members --
   -----------------

   function get_members
     (Self : Local_Ref)
     return DynamicAny.NameValuePairSeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynValue.Impl.get_members
        (DynamicAny.DynValue.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_members;

   -----------------
   -- set_members --
   -----------------

   procedure set_members
     (Self : Local_Ref;
      value : DynamicAny.NameValuePairSeq)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynValue.Impl.set_members
        (DynamicAny.DynValue.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end set_members;

   ----------------------------
   -- get_members_as_dyn_any --
   ----------------------------

   function get_members_as_dyn_any
     (Self : Local_Ref)
     return DynamicAny.NameDynAnyPairSeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynValue.Impl.get_members_as_dyn_any
        (DynamicAny.DynValue.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_members_as_dyn_any;

   ----------------------------
   -- set_members_as_dyn_any --
   ----------------------------

   procedure set_members_as_dyn_any
     (Self : Local_Ref;
      value : DynamicAny.NameDynAnyPairSeq)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynValue.Impl.set_members_as_dyn_any
        (DynamicAny.DynValue.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end set_members_as_dyn_any;

end DynamicAny.DynValue;
