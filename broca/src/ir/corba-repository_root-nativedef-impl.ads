----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.TypedefDef.Impl;
pragma Elaborate_All (CORBA.Repository_Root.TypedefDef.Impl);

package CORBA.Repository_Root.NativeDef.Impl is

   type Object is
     new CORBA.Repository_Root.TypedefDef.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : NativeDef_Forward.Ref)
                       return Object_Ptr;

    --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return NativeDef_Forward.Ref;

private

   type Object is
     new CORBA.Repository_Root.TypedefDef.Impl.Object with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end CORBA.Repository_Root.NativeDef.Impl;
