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

package DynamicAny.DynStruct.Helper is

   TC_DynStruct : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynStruct.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynStruct.Local_Ref;

   --  InvalidValue : inherited from DynamicAny.DynAny

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynStruct.InvalidValue_Members;

   function To_Any
     (Item : DynamicAny.DynStruct.InvalidValue_Members)
     return CORBA.Any;

   --  TypeMismatch : inherited from DynamicAny.DynAny

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynStruct.TypeMismatch_Members;

   function To_Any
     (Item : DynamicAny.DynStruct.TypeMismatch_Members)
     return CORBA.Any;

   
   package Internals is

      procedure Initialize_DynStruct;

   end Internals;

end DynamicAny.DynStruct.Helper;
