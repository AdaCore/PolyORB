----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.IDLType.Impl;
pragma Elaborate_All (CORBA.Repository_Root.IDLType.Impl);

package CORBA.Repository_Root.FixedDef.Impl is

   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Digits : CORBA.Unsigned_Short;
                   Scale : CORBA.Short);

   --  overload the get_type from IDLType
   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_digits
     (Self : access Object)
     return CORBA.Unsigned_Short;

   procedure set_digits
     (Self : access Object;
      To : in CORBA.Unsigned_Short);

   function get_scale
     (Self : access Object)
     return CORBA.Short;

   procedure set_scale
     (Self : access Object;
      To : in CORBA.Short);

private
   type Object is
     new CORBA.Repository_Root.IDLType.Impl.Object with record
        IDL_Digits : CORBA.Unsigned_Short;
        Scale : CORBA.Short;
     end record;

end CORBA.Repository_Root.FixedDef.Impl;
