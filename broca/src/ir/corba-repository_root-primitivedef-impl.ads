----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;

pragma Elaborate_All (CORBA.Repository_Root.IDLType.Impl);

package CORBA.Repository_Root.PrimitiveDef.Impl is

   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Type : CORBA.TypeCode.Object;
                   Kind : CORBA.Repository_Root.PrimitiveKind);

   function get_kind
     (Self : access Object)
     return CORBA.Repository_Root.PrimitiveKind;

private

   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with record
        Kind : CORBA.Repository_Root.PrimitiveKind;
   end record;

end CORBA.Repository_Root.PrimitiveDef.Impl;
