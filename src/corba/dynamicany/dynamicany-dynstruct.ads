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

with DynamicAny.DynAny;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with Ada.Exceptions;

package DynamicAny.DynStruct is

   type Local_Ref is
     new DynamicAny.DynAny.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynStruct:1.0";

   function current_member_name
     (Self : Local_Ref)
     return DynamicAny.FieldName;

   current_member_name_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynStruct/current_member_name:1.0";

   function current_member_kind
     (Self : Local_Ref)
     return CORBA.TCKind;

   current_member_kind_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynStruct/current_member_kind:1.0";

   function get_members
     (Self : Local_Ref)
     return DynamicAny.NameValuePairSeq;

   get_members_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynStruct/get_members:1.0";

   procedure set_members
     (Self : Local_Ref;
      value : DynamicAny.NameValuePairSeq);

   set_members_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynStruct/set_members:1.0";

   function get_members_as_dyn_any
     (Self : Local_Ref)
     return DynamicAny.NameDynAnyPairSeq;

   get_members_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynStruct/get_members_as_dyn_any:1.0";

   procedure set_members_as_dyn_any
     (Self : Local_Ref;
      value : DynamicAny.NameDynAnyPairSeq);

   set_members_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynStruct/set_members_as_dyn_any:1.0";

   --  InvalidValue : inherited from DynamicAny.DynAny

   InvalidValue : exception
     renames DynamicAny.DynAny.InvalidValue;

   subtype InvalidValue_Members is
     DynamicAny.DynAny.InvalidValue_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynStruct.InvalidValue_Members);

   --  TypeMismatch : inherited from DynamicAny.DynAny

   TypeMismatch : exception
     renames DynamicAny.DynAny.TypeMismatch;

   subtype TypeMismatch_Members is
     DynamicAny.DynAny.TypeMismatch_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynStruct.TypeMismatch_Members);

end DynamicAny.DynStruct;
