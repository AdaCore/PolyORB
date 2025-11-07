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
with CORBA;
pragma Elaborate_All (CORBA);
with DynamicAny.DynArray.Impl;

package body DynamicAny.DynArray is

   ------------------
   -- get_elements --
   ------------------

   function get_elements
     (Self : Local_Ref)
     return DynamicAny.AnySeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynArray.Impl.get_elements
        (DynamicAny.DynArray.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_elements;

   ------------------
   -- set_elements --
   ------------------

   procedure set_elements
     (Self : Local_Ref;
      value : DynamicAny.AnySeq)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynArray.Impl.set_elements
        (DynamicAny.DynArray.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end set_elements;

   -----------------------------
   -- get_elements_as_dyn_any --
   -----------------------------

   function get_elements_as_dyn_any
     (Self : Local_Ref)
     return DynamicAny.DynAnySeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return DynamicAny.DynArray.Impl.get_elements_as_dyn_any
        (DynamicAny.DynArray.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_elements_as_dyn_any;

   -----------------------------
   -- set_elements_as_dyn_any --
   -----------------------------

   procedure set_elements_as_dyn_any
     (Self : Local_Ref;
      value : DynamicAny.DynAnySeq)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynArray.Impl.set_elements_as_dyn_any
        (DynamicAny.DynArray.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end set_elements_as_dyn_any;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynArray.InvalidValue_Members)
     renames DynamicAny.DynAny.Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynArray.TypeMismatch_Members)
     renames DynamicAny.DynAny.Get_Members;

end DynamicAny.DynArray;
