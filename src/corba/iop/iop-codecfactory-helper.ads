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

package IOP.CodecFactory.Helper is

   TC_CodecFactory : CORBA.TypeCode.Object;

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return IOP.CodecFactory.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return IOP.CodecFactory.Local_Ref;

   TC_UnknownEncoding : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.CodecFactory.UnknownEncoding_Members;

   function To_Any
     (Item : IOP.CodecFactory.UnknownEncoding_Members)
     return CORBA.Any;

   procedure Raise_UnknownEncoding
     (Members : IOP.CodecFactory.UnknownEncoding_Members);

   pragma No_Return (Raise_UnknownEncoding);

   
   package Internals is

      procedure Initialize_CodecFactory;

      procedure Initialize_UnknownEncoding;

   end Internals;

end IOP.CodecFactory.Helper;
