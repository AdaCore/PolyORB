----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;
pragma Elaborate_All (CORBA.Repository_Root.IRObject.Impl);

package CORBA.Repository_Root.IDLType.Impl is

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  To transform a forward_ref in impl.object_ptr.
   function To_Object (Fw_Ref : IDLType_Forward.Ref)
                       return Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return IDLType_Forward.Ref;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind);

   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

private

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with null record;
     --  the Type attribute is computed dynamically in each child,
     --  because it can be changed by setting specific attributes after
     --  the beginning.

end CORBA.Repository_Root.IDLType.Impl;
