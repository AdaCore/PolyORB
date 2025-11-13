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

package DynamicAny.DynSequence is

   type Local_Ref is
     new DynamicAny.DynAny.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynSequence:1.0";

   function get_length
     (Self : Local_Ref)
     return CORBA.Unsigned_Long;

   get_length_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynSequence/get_length:1.0";

   procedure set_length
     (Self : Local_Ref;
      len : CORBA.Unsigned_Long);

   set_length_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynSequence/set_length:1.0";

   function get_elements
     (Self : Local_Ref)
     return DynamicAny.AnySeq;

   get_elements_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynSequence/get_elements:1.0";

   procedure set_elements
     (Self : Local_Ref;
      value : DynamicAny.AnySeq);

   set_elements_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynSequence/set_elements:1.0";

   function get_elements_as_dyn_any
     (Self : Local_Ref)
     return DynamicAny.DynAnySeq;

   get_elements_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynSequence/get_elements_as_dyn_any:1.0";

   procedure set_elements_as_dyn_any
     (Self : Local_Ref;
      value : DynamicAny.DynAnySeq);

   set_elements_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynSequence/set_elements_as_dyn_any:1.0";

   --  InvalidValue : inherited from DynamicAny.DynAny

   InvalidValue : exception
     renames DynamicAny.DynAny.InvalidValue;

   subtype InvalidValue_Members is
     DynamicAny.DynAny.InvalidValue_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynSequence.InvalidValue_Members);

   --  TypeMismatch : inherited from DynamicAny.DynAny

   TypeMismatch : exception
     renames DynamicAny.DynAny.TypeMismatch;

   subtype TypeMismatch_Members is
     DynamicAny.DynAny.TypeMismatch_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynSequence.TypeMismatch_Members);

end DynamicAny.DynSequence;
