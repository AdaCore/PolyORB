pragma Style_Checks ("NM32766");
pragma Warnings (Off, "use of an anonymous access type allocator");
pragma Warnings (Off, "unnecessary with of ancestor");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/IOP.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with PolyORB.Exceptions;
with IOP.CodecFactory.Impl;

package body IOP.CodecFactory is

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out IOP.CodecFactory.UnknownEncoding_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   ------------------
   -- create_codec --
   ------------------

   function create_codec
     (Self : Local_Ref;
      enc : IOP.Encoding)
     return IOP.Codec.Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return IOP.CodecFactory.Impl.create_codec
        (IOP.CodecFactory.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         enc);
   end create_codec;

end IOP.CodecFactory;
