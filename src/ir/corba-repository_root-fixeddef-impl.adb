----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.ORB.Typecode;

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.FixedDef.Skel;

package body CORBA.Repository_Root.FixedDef.Impl is

   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Digits : CORBA.Unsigned_Short;
                   Scale : CORBA.Short) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind);
      Self.Idl_Digits := IDL_Digits;
      Self.Scale := Scale;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.TypeCode.Create_Fixed_Tc (Self.IDL_Digits,
                                                 Self.Scale);
   end get_type;

   function get_digits
     (Self : access Object)
     return CORBA.Unsigned_Short
   is
   begin
      return Self.IDL_Digits;
   end get_digits;


   procedure set_digits
     (Self : access Object;
      To : in CORBA.Unsigned_Short) is
   begin
      Self.IDL_Digits := To;
   end set_digits;


   function get_scale
     (Self : access Object)
     return CORBA.Short
   is
   begin
      return Self.Scale;
   end get_scale;


   procedure set_scale
     (Self : access Object;
      To : in CORBA.Short) is
   begin
      Self.Scale := To;
   end set_scale;

end CORBA.Repository_Root.FixedDef.Impl;
