----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.PrimitiveDef.Skel;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;

package body CORBA.Repository_Root.PrimitiveDef.Impl is

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Type : CORBA.TypeCode.Object;
                   Kind : CORBA.Repository_Root.PrimitiveKind) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind,
                         IDL_Type);
      Self.Kind := Kind;
   end Init;


   function get_kind
     (Self : access Object)
     return CORBA.Repository_Root.PrimitiveKind
   is
      Result : CORBA.Repository_Root.PrimitiveKind;
   begin

      --  Insert implementation of get_kind

      return Result;
   end get_kind;

end CORBA.Repository_Root.PrimitiveDef.Impl;
