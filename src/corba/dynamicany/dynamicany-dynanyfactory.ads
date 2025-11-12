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

with CORBA.Object;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with Ada.Exceptions;
with DynamicAny.DynAny;

package DynamicAny.DynAnyFactory is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAnyFactory:1.0";

   InconsistentTypeCode : exception;

   type InconsistentTypeCode_Members is
     new CORBA.Idl_Exception_Members with null record;

   InconsistentTypeCode_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAnyFactory/InconsistentTypeCode:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynAnyFactory.InconsistentTypeCode_Members);

   function create_dyn_any
     (Self : Local_Ref;
      value : CORBA.Any)
     return DynamicAny.DynAny.Local_Ref;

   create_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAnyFactory/create_dyn_any:1.0";

   function create_dyn_any_without_truncation
     (Self : Local_Ref;
      value : CORBA.Any)
     return DynamicAny.DynAny.Local_Ref;

   create_dyn_any_without_truncation_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAnyFactory/create_dyn_any_without_truncation:1.0";

end DynamicAny.DynAnyFactory;
