----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.FixedDef.Skel;

package body CORBA.Repository_Root.FixedDef.Impl is

   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Type : CORBA.TypeCode.Object;
                   IDL_Digits : CORBA.Unsigned_Short;
                   Scale : CORBA.Short) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind,
                         IDL_Type);
      Self.Idl_Digits := IDL_Digits;
      Self.Scale := Scale;
   end Init;

   function get_digits
     (Self : access Object)
     return CORBA.Unsigned_Short
   is
      Result : CORBA.Unsigned_Short;
   begin

      --  Insert implementation of get_digits

      return Result;
   end get_digits;


   procedure set_digits
     (Self : access Object;
      To : in CORBA.Unsigned_Short) is
   begin

      --  Insert implementation of set_digits

      null;
   end set_digits;


   function get_scale
     (Self : access Object)
     return CORBA.Short
   is
      Result : CORBA.Short;
   begin

      --  Insert implementation of get_scale

      return Result;
   end get_scale;


   procedure set_scale
     (Self : access Object;
      To : in CORBA.Short) is
   begin

      --  Insert implementation of set_scale

      null;
   end set_scale;

end CORBA.Repository_Root.FixedDef.Impl;
