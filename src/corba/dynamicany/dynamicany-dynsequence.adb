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
with DynamicAny.DynSequence.Impl;

package body DynamicAny.DynSequence is

   ----------------
   -- get_length --
   ----------------

   function get_length
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
      return DynamicAny.DynSequence.Impl.get_length
        (DynamicAny.DynSequence.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_length;

   ----------------
   -- set_length --
   ----------------

   procedure set_length
     (Self : Local_Ref;
      len : CORBA.Unsigned_Long)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynSequence.Impl.set_length
        (DynamicAny.DynSequence.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         len);
   end set_length;

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
      return DynamicAny.DynSequence.Impl.get_elements
        (DynamicAny.DynSequence.Impl.Object_Ptr
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
      DynamicAny.DynSequence.Impl.set_elements
        (DynamicAny.DynSequence.Impl.Object_Ptr
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
      return DynamicAny.DynSequence.Impl.get_elements_as_dyn_any
        (DynamicAny.DynSequence.Impl.Object_Ptr
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
      DynamicAny.DynSequence.Impl.set_elements_as_dyn_any
        (DynamicAny.DynSequence.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         value);
   end set_elements_as_dyn_any;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynSequence.InvalidValue_Members)
     renames DynamicAny.DynAny.Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynSequence.TypeMismatch_Members)
     renames DynamicAny.DynAny.Get_Members;

end DynamicAny.DynSequence;
