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

with PolyORB.Std;
with PolyORB.Any;
with DynamicAny.DynAny.Helper;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body DynamicAny.DynFixed.Helper is

   
   package body Internals is

      DynFixed_Initialized : PolyORB.Std.Boolean :=
        False;

      -------------------------
      -- Initialize_DynFixed --
      -------------------------

      procedure Initialize_DynFixed is
         Name_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("DynFixed");
         Id_Ü : constant CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:omg.org/DynamicAny/DynFixed:1.0");
      begin
         if not DynFixed_Initialized
         then
            DynFixed_Initialized :=
              True;
            DynamicAny.DynFixed.Helper.TC_DynFixed :=
              CORBA.TypeCode.Internals.To_CORBA_Object
                 (PolyORB.Any.TypeCode.Tcf_Object);
            CORBA.Internals.Add_Parameter
              (TC_DynFixed,
               CORBA.To_Any
                 (Name_Ü));
            CORBA.Internals.Add_Parameter
              (TC_DynFixed,
               CORBA.To_Any
                 (Id_Ü));
            CORBA.TypeCode.Internals.Disable_Ref_Counting
              (TC_DynFixed);
            CORBA.TypeCode.Internals.Freeze
              (TC_DynFixed);
         end if;
      end Initialize_DynFixed;

   end Internals;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynFixed.Local_Ref
   is
      Result : DynamicAny.DynFixed.Local_Ref;
   begin
      Set
        (Result,
         CORBA.Object.Object_Of
           (The_Ref));
      return Result;
   end Unchecked_To_Local_Ref;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynFixed.Local_Ref
   is
   begin
      if (CORBA.Object.Is_Nil
        (The_Ref)
         or else CORBA.Object.Is_A
           (The_Ref,
            Repository_Id))
      then
         return Unchecked_To_Local_Ref
           (The_Ref);
      end if;
      CORBA.Raise_Bad_Param
        (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynFixed.InvalidValue_Members
     renames DynamicAny.DynAny.Helper.From_Any;

   function To_Any
     (Item : DynamicAny.DynFixed.InvalidValue_Members)
     return CORBA.Any
     renames DynamicAny.DynAny.Helper.To_Any;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynFixed.TypeMismatch_Members
     renames DynamicAny.DynAny.Helper.From_Any;

   function To_Any
     (Item : DynamicAny.DynFixed.TypeMismatch_Members)
     return CORBA.Any
     renames DynamicAny.DynAny.Helper.To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      DynamicAny.DynFixed.Helper.Internals.Initialize_DynFixed;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
           (Name => +"DynamicAny.DynFixed.Helper",
            Conflicts => PolyORB.Utils.Strings.Lists.Empty,
            Depends => +"any",
            Provides => PolyORB.Utils.Strings.Lists.Empty,
            Implicit => False,
            Init => Deferred_Initialization'Access,
            Shutdown => null));
   end;
end DynamicAny.DynFixed.Helper;
