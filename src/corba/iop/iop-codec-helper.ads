pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/IOP.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package IOP.Codec.Helper is

   TC_Codec : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return IOP.Codec.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return IOP.Codec.Local_Ref;

   TC_InvalidTypeForEncoding : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.Codec.InvalidTypeForEncoding_Members;

   function To_Any
     (Item : IOP.Codec.InvalidTypeForEncoding_Members)
     return CORBA.Any;

   procedure Raise_InvalidTypeForEncoding
     (Members : IOP.Codec.InvalidTypeForEncoding_Members);

   pragma No_Return (Raise_InvalidTypeForEncoding);

   TC_FormatMismatch : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.Codec.FormatMismatch_Members;

   function To_Any
     (Item : IOP.Codec.FormatMismatch_Members)
     return CORBA.Any;

   procedure Raise_FormatMismatch
     (Members : IOP.Codec.FormatMismatch_Members);

   pragma No_Return (Raise_FormatMismatch);

   TC_TypeMismatch : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.Codec.TypeMismatch_Members;

   function To_Any
     (Item : IOP.Codec.TypeMismatch_Members)
     return CORBA.Any;

   procedure Raise_TypeMismatch
     (Members : IOP.Codec.TypeMismatch_Members);

   pragma No_Return (Raise_TypeMismatch);

   
   package Internals is

      procedure Initialize_Codec;

      procedure Initialize_InvalidTypeForEncoding;

      procedure Initialize_FormatMismatch;

      procedure Initialize_TypeMismatch;

   end Internals;

end IOP.Codec.Helper;
