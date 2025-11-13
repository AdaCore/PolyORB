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

package DynamicAny.DynAny is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny:1.0";

   InvalidValue : exception;

   type InvalidValue_Members is
     new CORBA.Idl_Exception_Members with null record;

   InvalidValue_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/InvalidValue:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynAny.InvalidValue_Members);

   TypeMismatch : exception;

   type TypeMismatch_Members is
     new CORBA.Idl_Exception_Members with null record;

   TypeMismatch_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/TypeMismatch:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynAny.TypeMismatch_Members);

   function IDL_type
     (Self : Local_Ref)
     return CORBA.TypeCode.Object;

   IDL_type_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/type:1.0";

   procedure assign
     (Self : Local_Ref;
      dyn_any : DynamicAny.DynAny.Local_Ref'Class);

   assign_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/assign:1.0";

   procedure from_any
     (Self : Local_Ref;
      value : CORBA.Any);

   from_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/from_any:1.0";

   function to_any
     (Self : Local_Ref)
     return CORBA.Any;

   to_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/to_any:1.0";

   function equal
     (Self : Local_Ref;
      dyn_any : DynamicAny.DynAny.Local_Ref'Class)
     return CORBA.Boolean;

   equal_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/equal:1.0";

   procedure destroy
     (Self : Local_Ref);

   destroy_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/destroy:1.0";

   function copy
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref'Class;

   copy_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/copy:1.0";

   procedure insert_boolean
     (Self : Local_Ref;
      value : CORBA.Boolean);

   insert_boolean_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_boolean:1.0";

   procedure insert_octet
     (Self : Local_Ref;
      value : CORBA.Octet);

   insert_octet_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_octet:1.0";

   procedure insert_char
     (Self : Local_Ref;
      value : CORBA.Char);

   insert_char_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_char:1.0";

   procedure insert_short
     (Self : Local_Ref;
      value : CORBA.Short);

   insert_short_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_short:1.0";

   procedure insert_ushort
     (Self : Local_Ref;
      value : CORBA.Unsigned_Short);

   insert_ushort_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_ushort:1.0";

   procedure insert_long
     (Self : Local_Ref;
      value : CORBA.Long);

   insert_long_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_long:1.0";

   procedure insert_ulong
     (Self : Local_Ref;
      value : CORBA.Unsigned_Long);

   insert_ulong_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_ulong:1.0";

   procedure insert_float
     (Self : Local_Ref;
      value : CORBA.Float);

   insert_float_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_float:1.0";

   procedure insert_double
     (Self : Local_Ref;
      value : CORBA.Double);

   insert_double_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_double:1.0";

   procedure insert_string
     (Self : Local_Ref;
      value : CORBA.String);

   insert_string_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_string:1.0";

   procedure insert_typecode
     (Self : Local_Ref;
      value : CORBA.TypeCode.Object);

   insert_typecode_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_typecode:1.0";

   procedure insert_longlong
     (Self : Local_Ref;
      value : CORBA.Long_Long);

   insert_longlong_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_longlong:1.0";

   procedure insert_ulonglong
     (Self : Local_Ref;
      value : CORBA.Unsigned_Long_Long);

   insert_ulonglong_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_ulonglong:1.0";

   procedure insert_longdouble
     (Self : Local_Ref;
      value : CORBA.Long_Double);

   insert_longdouble_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_longdouble:1.0";

   procedure insert_wchar
     (Self : Local_Ref;
      value : CORBA.Wchar);

   insert_wchar_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_wchar:1.0";

   procedure insert_wstring
     (Self : Local_Ref;
      value : CORBA.Wide_String);

   insert_wstring_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_wstring:1.0";

   procedure insert_any
     (Self : Local_Ref;
      value : CORBA.Any);

   insert_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_any:1.0";

   procedure insert_dyn_any
     (Self : Local_Ref;
      value : DynamicAny.DynAny.Local_Ref'Class);

   insert_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/insert_dyn_any:1.0";

   function get_boolean
     (Self : Local_Ref)
     return CORBA.Boolean;

   get_boolean_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_boolean:1.0";

   function get_octet
     (Self : Local_Ref)
     return CORBA.Octet;

   get_octet_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_octet:1.0";

   function get_char
     (Self : Local_Ref)
     return CORBA.Char;

   get_char_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_char:1.0";

   function get_short
     (Self : Local_Ref)
     return CORBA.Short;

   get_short_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_short:1.0";

   function get_ushort
     (Self : Local_Ref)
     return CORBA.Unsigned_Short;

   get_ushort_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_ushort:1.0";

   function get_long
     (Self : Local_Ref)
     return CORBA.Long;

   get_long_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_long:1.0";

   function get_ulong
     (Self : Local_Ref)
     return CORBA.Unsigned_Long;

   get_ulong_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_ulong:1.0";

   function get_float
     (Self : Local_Ref)
     return CORBA.Float;

   get_float_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_float:1.0";

   function get_double
     (Self : Local_Ref)
     return CORBA.Double;

   get_double_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_double:1.0";

   function get_string
     (Self : Local_Ref)
     return CORBA.String;

   get_string_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_string:1.0";

   function get_typecode
     (Self : Local_Ref)
     return CORBA.TypeCode.Object;

   get_typecode_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_typecode:1.0";

   function get_longlong
     (Self : Local_Ref)
     return CORBA.Long_Long;

   get_longlong_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_longlong:1.0";

   function get_ulonglong
     (Self : Local_Ref)
     return CORBA.Unsigned_Long_Long;

   get_ulonglong_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_ulonglong:1.0";

   function get_longdouble
     (Self : Local_Ref)
     return CORBA.Long_Double;

   get_longdouble_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_longdouble:1.0";

   function get_wchar
     (Self : Local_Ref)
     return CORBA.Wchar;

   get_wchar_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_wchar:1.0";

   function get_wstring
     (Self : Local_Ref)
     return CORBA.Wide_String;

   get_wstring_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_wstring:1.0";

   function get_any
     (Self : Local_Ref)
     return CORBA.Any;

   get_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_any:1.0";

   function get_dyn_any
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref'Class;

   get_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/get_dyn_any:1.0";

   function seek
     (Self : Local_Ref;
      index : CORBA.Long)
     return CORBA.Boolean;

   seek_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/seek:1.0";

   procedure rewind
     (Self : Local_Ref);

   rewind_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/rewind:1.0";

   function next
     (Self : Local_Ref)
     return CORBA.Boolean;

   next_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/next:1.0";

   function component_count
     (Self : Local_Ref)
     return CORBA.Unsigned_Long;

   component_count_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/component_count:1.0";

   function current_component
     (Self : Local_Ref)
     return DynamicAny.DynAny.Local_Ref'Class;

   current_component_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynAny/current_component:1.0";

   package Convert_Forward is
     new DynamicAny.DynAny_Forward.Convert
        (Local_Ref);

end DynamicAny.DynAny;
