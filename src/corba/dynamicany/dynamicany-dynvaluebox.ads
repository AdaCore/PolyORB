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

with DynamicAny.DynValueCommon;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with DynamicAny.DynAny;

package DynamicAny.DynValueBox is

   type Local_Ref is
     new DynamicAny.DynValueCommon.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValueBox:1.0";

   function get_boxed_value
     (Self : Local_Ref)
     return CORBA.Any;

   get_boxed_value_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValueBox/get_boxed_value:1.0";

   procedure set_boxed_value
     (Self : Local_Ref;
      boxed : CORBA.Any);

   set_boxed_value_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValueBox/set_boxed_value:1.0";

   function get_boxed_value_as_dyn_any
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref;

   get_boxed_value_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValueBox/get_boxed_value_as_dyn_any:1.0";

   procedure set_boxed_value_as_dyn_any
     (Self : Local_Ref;
      boxed : DynamicAny.DynAny.Local_Ref);

   set_boxed_value_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValueBox/set_boxed_value_as_dyn_any:1.0";

end DynamicAny.DynValueBox;
