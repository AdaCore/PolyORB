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
with DynamicAny.DynValueCommon.Impl;

package body DynamicAny.DynValueCommon is

   -------------
   -- is_null --
   -------------

   function is_null
     (Self : Local_Ref)
     return CORBA.Boolean
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynValueCommon.Impl.is_null
        (DynamicAny.DynValueCommon.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end is_null;

   -----------------
   -- set_to_null --
   -----------------

   procedure set_to_null
     (Self : Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynValueCommon.Impl.set_to_null
        (DynamicAny.DynValueCommon.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end set_to_null;

   ------------------
   -- set_to_value --
   ------------------

   procedure set_to_value
     (Self : Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynValueCommon.Impl.set_to_value
        (DynamicAny.DynValueCommon.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end set_to_value;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynValueCommon.InvalidValue_Members)
     renames DynamicAny.DynAny.Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynValueCommon.TypeMismatch_Members)
     renames DynamicAny.DynAny.Get_Members;

end DynamicAny.DynValueCommon;
