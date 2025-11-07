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

package DynamicAny.DynAnyFactory.Helper is

   TC_DynAnyFactory : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAnyFactory.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return DynamicAny.DynAnyFactory.Local_Ref;

   TC_InconsistentTypeCode : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return DynamicAny.DynAnyFactory.InconsistentTypeCode_Members;

   function To_Any
     (Item : DynamicAny.DynAnyFactory.InconsistentTypeCode_Members)
     return CORBA.Any;

   procedure Raise_InconsistentTypeCode
     (Members : DynamicAny.DynAnyFactory.InconsistentTypeCode_Members);

   pragma No_Return (Raise_InconsistentTypeCode);

   
   package Internals is

      procedure Initialize_DynAnyFactory;

      procedure Initialize_InconsistentTypeCode;

   end Internals;

end DynamicAny.DynAnyFactory.Helper;
