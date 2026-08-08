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
with DynamicAny.DynFixed.Impl;

package body DynamicAny.DynFixed is

   ---------------
   -- get_value --
   ---------------

   function get_value
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
      return DynamicAny.DynFixed.Impl.get_value
        (DynamicAny.DynFixed.Impl.Object_Ptr
           (Entity_Of
              (Self)));
   end get_value;

   ---------------
   -- set_value --
   ---------------

   function set_value
     (Self : Local_Ref;
      val : CORBA.String)
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
      return DynamicAny.DynFixed.Impl.set_value
        (DynamicAny.DynFixed.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         val);
   end set_value;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynFixed.InvalidValue_Members)
     renames DynamicAny.DynAny.Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynFixed.TypeMismatch_Members)
     renames DynamicAny.DynAny.Get_Members;

end DynamicAny.DynFixed;
