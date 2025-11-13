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

with CORBA.Object;
with PolyORB.Std;
with CORBA;
pragma Elaborate_All (CORBA);
with Ada.Exceptions;
with CORBA.IDL_Sequences;

package IOP.Codec is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Codec:1.0";

   InvalidTypeForEncoding : exception;

   type InvalidTypeForEncoding_Members is
     new CORBA.Idl_Exception_Members with null record;

   InvalidTypeForEncoding_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Codec/InvalidTypeForEncoding:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out IOP.Codec.InvalidTypeForEncoding_Members);

   FormatMismatch : exception;

   type FormatMismatch_Members is
     new CORBA.Idl_Exception_Members with null record;

   FormatMismatch_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Codec/FormatMismatch:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out IOP.Codec.FormatMismatch_Members);

   TypeMismatch : exception;

   type TypeMismatch_Members is
     new CORBA.Idl_Exception_Members with null record;

   TypeMismatch_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Codec/TypeMismatch:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out IOP.Codec.TypeMismatch_Members);

   function encode
     (Self : Local_Ref;
      data : CORBA.Any)
     return CORBA.IDL_Sequences.OctetSeq;

   encode_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Codec/encode:1.0";

   function decode
     (Self : Local_Ref;
      data : CORBA.IDL_Sequences.OctetSeq)
     return CORBA.Any;

   decode_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Codec/decode:1.0";

   function encode_value
     (Self : Local_Ref;
      data : CORBA.Any)
     return CORBA.IDL_Sequences.OctetSeq;

   encode_value_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Codec/encode_value:1.0";

   function decode_value
     (Self : Local_Ref;
      data : CORBA.IDL_Sequences.OctetSeq;
      tc : CORBA.TypeCode.Object)
     return CORBA.Any;

   decode_value_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/Codec/decode_value:1.0";

end IOP.Codec;
