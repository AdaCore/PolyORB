----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;

with CORBA.Repository_Root.PrimitiveDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.PrimitiveDef.Skel);

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
                         Def_Kind);
      Self.Kind := Kind;
      Self.IDL_Type := IDL_Type;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return Self.IDL_Type;
   end get_type;


   function get_kind
     (Self : access Object)
     return CORBA.Repository_Root.PrimitiveKind
   is
   begin
      return Self.Kind;
   end get_kind;

end CORBA.Repository_Root.PrimitiveDef.Impl;
