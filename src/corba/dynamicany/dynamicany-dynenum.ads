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

package DynamicAny.DynEnum is

   type Local_Ref is
     new DynamicAny.DynAny.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynEnum:1.0";

   function get_as_string
     (Self : Local_Ref)
     return CORBA.String;

   get_as_string_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynEnum/get_as_string:1.0";

   procedure set_as_string
     (Self : Local_Ref;
      value : CORBA.String);

   set_as_string_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynEnum/set_as_string:1.0";

   function get_as_ulong
     (Self : Local_Ref)
     return CORBA.Unsigned_Long;

   get_as_ulong_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynEnum/get_as_ulong:1.0";

   procedure set_as_ulong
     (Self : Local_Ref;
      value : CORBA.Unsigned_Long);

   set_as_ulong_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynEnum/set_as_ulong:1.0";

   --  InvalidValue : inherited from DynamicAny.DynAny

   InvalidValue : exception
     renames DynamicAny.DynAny.InvalidValue;

   subtype InvalidValue_Members is
     DynamicAny.DynAny.InvalidValue_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynEnum.InvalidValue_Members);

   --  TypeMismatch : inherited from DynamicAny.DynAny

   TypeMismatch : exception
     renames DynamicAny.DynAny.TypeMismatch;

   subtype TypeMismatch_Members is
     DynamicAny.DynAny.TypeMismatch_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynEnum.TypeMismatch_Members);

end DynamicAny.DynEnum;
