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
with Ada.Exceptions;

package DynamicAny.DynArray is

   type Local_Ref is
     new DynamicAny.DynAny.Local_Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynArray:1.0";

   function get_elements
     (Self : Local_Ref)
     return DynamicAny.AnySeq;

   get_elements_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynArray/get_elements:1.0";

   procedure set_elements
     (Self : Local_Ref;
      value : DynamicAny.AnySeq);

   set_elements_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynArray/set_elements:1.0";

   function get_elements_as_dyn_any
     (Self : Local_Ref)
     return DynamicAny.DynAnySeq;

   get_elements_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynArray/get_elements_as_dyn_any:1.0";

   procedure set_elements_as_dyn_any
     (Self : Local_Ref;
      value : DynamicAny.DynAnySeq);

   set_elements_as_dyn_any_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/DynamicAny/DynArray/set_elements_as_dyn_any:1.0";

   --  InvalidValue : inherited from DynamicAny.DynAny

   InvalidValue : exception
     renames DynamicAny.DynAny.InvalidValue;

   subtype InvalidValue_Members is
     DynamicAny.DynAny.InvalidValue_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynArray.InvalidValue_Members);

   --  TypeMismatch : inherited from DynamicAny.DynAny

   TypeMismatch : exception
     renames DynamicAny.DynAny.TypeMismatch;

   subtype TypeMismatch_Members is
     DynamicAny.DynAny.TypeMismatch_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out DynamicAny.DynArray.TypeMismatch_Members);

end DynamicAny.DynArray;
