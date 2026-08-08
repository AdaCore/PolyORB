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

package DynamicAny.DynUnion is

   type Local_Ref is
     new DynamicAny.DynAny.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion:1.0";

   function get_discriminator
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref;

   get_discriminator_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/get_discriminator:1.0";

   procedure set_discriminator
     (Self : Local_Ref;
      d : DynamicAny.DynAny.Local_Ref);

   set_discriminator_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/set_discriminator:1.0";

   procedure set_to_default_member
     (Self : Local_Ref);

   set_to_default_member_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/set_to_default_member:1.0";

   procedure set_to_no_active_member
     (Self : Local_Ref);

   set_to_no_active_member_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/set_to_no_active_member:1.0";

   function has_no_active_member
     (Self : Local_Ref)
     return CORBA.Boolean;

   has_no_active_member_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/has_no_active_member:1.0";

   function discriminator_kind
     (Self : Local_Ref)
     return CORBA.TCKind;

   discriminator_kind_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/discriminator_kind:1.0";

   function member
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref;

   member_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/member:1.0";

   function member_name
     (Self : Local_Ref)
     return DynamicAny.FieldName;

   member_name_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/member_name:1.0";

   function member_kind
     (Self : Local_Ref)
     return CORBA.TCKind;

   member_kind_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynUnion/member_kind:1.0";

   --  InvalidValue : inherited from DynamicAny.DynAny

   InvalidValue : exception
     renames DynamicAny.DynAny.InvalidValue;

   subtype InvalidValue_Members is
     DynamicAny.DynAny.InvalidValue_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynUnion.InvalidValue_Members);

   --  TypeMismatch : inherited from DynamicAny.DynAny

   TypeMismatch : exception
     renames DynamicAny.DynAny.TypeMismatch;

   subtype TypeMismatch_Members is
     DynamicAny.DynAny.TypeMismatch_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynUnion.TypeMismatch_Members);

end DynamicAny.DynUnion;
