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

private

   type Object is
     new CORBA.Repository_Root.TypedefDef.Impl.Object with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end CORBA.Repository_Root.NativeDef.Impl;
