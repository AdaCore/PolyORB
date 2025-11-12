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
with DynamicAny.DynUnion.Impl;

package body DynamicAny.DynUnion is

   -----------------------
   -- get_discriminator --
   -----------------------

   function get_discriminator
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
      return DynamicAny.DynUnion.Impl.get_discriminator
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_discriminator;

   -----------------------
   -- set_discriminator --
   -----------------------

   procedure set_discriminator
     (Self : Local_Ref;
      d : DynamicAny.DynAny.Local_Ref)
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      DynamicAny.DynUnion.Impl.set_discriminator
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         d);
   end set_discriminator;

   ---------------------------
   -- set_to_default_member --
   ---------------------------

   procedure set_to_default_member
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
      DynamicAny.DynUnion.Impl.set_to_default_member
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end set_to_default_member;

   -----------------------------
   -- set_to_no_active_member --
   -----------------------------

   procedure set_to_no_active_member
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
      DynamicAny.DynUnion.Impl.set_to_no_active_member
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end set_to_no_active_member;

   --------------------------
   -- has_no_active_member --
   --------------------------

   function has_no_active_member
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
      return DynamicAny.DynUnion.Impl.has_no_active_member
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end has_no_active_member;

   ------------------------
   -- discriminator_kind --
   ------------------------

   function discriminator_kind
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
      return DynamicAny.DynUnion.Impl.discriminator_kind
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end discriminator_kind;

   ------------
   -- member --
   ------------

   function member
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
      return DynamicAny.DynUnion.Impl.member
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end member;

   -----------------
   -- member_name --
   -----------------

   function member_name
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
      return DynamicAny.DynUnion.Impl.member_name
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end member_name;

   -----------------
   -- member_kind --
   -----------------

   function member_kind
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
      return DynamicAny.DynUnion.Impl.member_kind
        (DynamicAny.DynUnion.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end member_kind;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynUnion.InvalidValue_Members)
     renames DynamicAny.DynAny.Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynUnion.TypeMismatch_Members)
     renames DynamicAny.DynAny.Get_Members;

end DynamicAny.DynUnion;
