----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.IDLType.Impl;
pragma Elaborate_All (CORBA.Repository_Root.IDLType.Impl);

package CORBA.Repository_Root.WstringDef.Impl is

   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Bound : CORBA.Unsigned_Long);

   --  overload the get_type from IDLType
   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_bound
     (Self : access Object)
     return CORBA.Unsigned_Long;

   procedure set_bound
     (Self : access Object;
      To : in CORBA.Unsigned_Long);

private

   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with record
        Bound : CORBA.Unsigned_Long;
     end record;

end CORBA.Repository_Root.WstringDef.Impl;
