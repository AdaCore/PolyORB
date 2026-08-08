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

package DynamicAny.DynUnion.Helper is

   TC_DynUnion : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynUnion.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynUnion.Local_Ref;

   --  InvalidValue : inherited from DynamicAny.DynAny

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynUnion.InvalidValue_Members;

   function To_Any
     (Item : DynamicAny.DynUnion.InvalidValue_Members)
     return CORBA.Any;

   --  TypeMismatch : inherited from DynamicAny.DynAny

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynUnion.TypeMismatch_Members;

   function To_Any
     (Item : DynamicAny.DynUnion.TypeMismatch_Members)
     return CORBA.Any;

   
   package Internals is

      procedure Initialize_DynUnion;

   end Internals;

end DynamicAny.DynUnion.Helper;
