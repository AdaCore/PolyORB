----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;

package CORBA.Repository_Root.IDLType.Impl is

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  should only be called if the cast is safe!
   function To_IDLType
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr)
      return Object_Ptr;

   --  method used to initialize recursively the object fields.
--   procedure Init (Self : access Object;
--                   Real_Object :
--                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
--                   Def_Kind : CORBA.Repository_Root.DefinitionKind);

   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

private

   type Object is new CORBA.Repository_Root.IRObject.Impl.Object
     with null record;
   --  the Type attribute is computed dynamically in each child,
   --  because it can be changed by setting specific attributes after
   --  the beginning.

end CORBA.Repository_Root.IDLType.Impl;
