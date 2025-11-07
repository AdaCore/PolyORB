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
with IOP.Codec.Impl;

package body IOP.Codec is

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out IOP.Codec.InvalidTypeForEncoding_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out IOP.Codec.FormatMismatch_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out IOP.Codec.TypeMismatch_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members
        (From,
         To);
   end Get_Members;

   ------------
   -- encode --
   ------------

   function encode
     (Self : Local_Ref;
      data : CORBA.Any)
     return CORBA.IDL_Sequences.OctetSeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return IOP.Codec.Impl.encode
        (IOP.Codec.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         data);
   end encode;

   ------------
   -- decode --
   ------------

   function decode
     (Self : Local_Ref;
      data : CORBA.IDL_Sequences.OctetSeq)
     return CORBA.Any
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return IOP.Codec.Impl.decode
        (IOP.Codec.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         data);
   end decode;

   ------------------
   -- encode_value --
   ------------------

   function encode_value
     (Self : Local_Ref;
      data : CORBA.Any)
     return CORBA.IDL_Sequences.OctetSeq
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return IOP.Codec.Impl.encode_value
        (IOP.Codec.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         data);
   end encode_value;

   ------------------
   -- decode_value --
   ------------------

   function decode_value
     (Self : Local_Ref;
      data : CORBA.IDL_Sequences.OctetSeq;
      tc : CORBA.TypeCode.Object)
     return CORBA.Any
   is
   begin
      if CORBA.Object.Is_Nil
        (CORBA.Object.Ref
           (Self))
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      return IOP.Codec.Impl.decode_value
        (IOP.Codec.Impl.Object_Ptr
           (Entity_Of
              (Self)),
         data,
         tc);
   end decode_value;

end IOP.Codec;
