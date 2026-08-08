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

package DynamicAny.DynValue is

   type Local_Ref is
     new DynamicAny.DynValueCommon.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValue:1.0";

   function current_member_name
     (Self : Local_Ref)
     return DynamicAny.FieldName;

   current_member_name_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValue/current_member_name:1.0";

   function current_member_kind
     (Self : Local_Ref)
     return CORBA.TCKind;

   current_member_kind_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValue/current_member_kind:1.0";

   function get_members
     (Self : Local_Ref)
     return DynamicAny.NameValuePairSeq;

   get_members_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValue/get_members:1.0";

   procedure set_members
     (Self : Local_Ref;
      value : DynamicAny.NameValuePairSeq);

   set_members_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValue/set_members:1.0";

   function get_members_as_dyn_any
     (Self : Local_Ref)
     return DynamicAny.NameDynAnyPairSeq;

   get_members_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValue/get_members_as_dyn_any:1.0";

   procedure set_members_as_dyn_any
     (Self : Local_Ref;
      value : DynamicAny.NameDynAnyPairSeq);

   set_members_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynValue/set_members_as_dyn_any:1.0";

end DynamicAny.DynValue;
