----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with PortableServer;

package VTsupport.SupportsAverage.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function get_average
     (Self : access Object)
     return CORBA.Double;

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end VTsupport.SupportsAverage.Impl;
