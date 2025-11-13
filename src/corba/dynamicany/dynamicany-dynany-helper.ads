pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Misc/DynamicAny.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package DynamicAny.DynAny.Helper is

   TC_DynAny : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAny.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAny.Local_Ref;

   TC_InvalidValue : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynAny.InvalidValue_Members;

   function To_Any
     (Item : DynamicAny.DynAny.InvalidValue_Members)
     return CORBA.Any;

   procedure Raise_InvalidValue
     (Members : DynamicAny.DynAny.InvalidValue_Members);

   pragma No_Return (Raise_InvalidValue);

   TC_TypeMismatch : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynAny.TypeMismatch_Members;

   function To_Any
     (Item : DynamicAny.DynAny.TypeMismatch_Members)
     return CORBA.Any;

   procedure Raise_TypeMismatch
     (Members : DynamicAny.DynAny.TypeMismatch_Members);

   pragma No_Return (Raise_TypeMismatch);

   
   package Internals is

      procedure Initialize_DynAny;

      procedure Initialize_InvalidValue;

      procedure Initialize_TypeMismatch;

   end Internals;

end DynamicAny.DynAny.Helper;
