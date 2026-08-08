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
with IOP.Codec;

package IOP.CodecFactory is

   type Local_Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/CodecFactory:1.0";

   UnknownEncoding : exception;

   type UnknownEncoding_Members is
     new CORBA.Idl_Exception_Members with null record;

   UnknownEncoding_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/CodecFactory/UnknownEncoding:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out IOP.CodecFactory.UnknownEncoding_Members);

   function create_codec
     (Self : Local_Ref;
      enc : IOP.Encoding)
     return IOP.Codec.Local_Ref;

   create_codec_Repository_Id : constant PolyORB.Std.String :=
     "IDL:omg.org/IOP/CodecFactory/create_codec:1.0";

end IOP.CodecFactory;
