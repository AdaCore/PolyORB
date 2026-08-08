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
with DynamicAny.DynEnum.Impl;

package body DynamicAny.DynEnum is

   -------------------
   -- get_as_string --
   -------------------

   function get_as_string
     (Self : Local_Ref)
     return CORBA.String
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynEnum.Impl.get_as_string
        (DynamicAny.DynEnum.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_as_string;

   -------------------
   -- set_as_string --
   -------------------

   procedure set_as_string
     (Self : Local_Ref;
      value : CORBA.String)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynEnum.Impl.set_as_string
        (DynamicAny.DynEnum.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end set_as_string;

   ------------------
   -- get_as_ulong --
   ------------------

   function get_as_ulong
     (Self : Local_Ref)
     return CORBA.Unsigned_Long
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynEnum.Impl.get_as_ulong
        (DynamicAny.DynEnum.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_as_ulong;

   ------------------
   -- set_as_ulong --
   ------------------

   procedure set_as_ulong
     (Self : Local_Ref;
      value : CORBA.Unsigned_Long)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynEnum.Impl.set_as_ulong
        (DynamicAny.DynEnum.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end set_as_ulong;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynEnum.InvalidValue_Members)
     renames DynamicAny.DynAny.Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynEnum.TypeMismatch_Members)
     renames DynamicAny.DynAny.Get_Members;

end DynamicAny.DynEnum;
