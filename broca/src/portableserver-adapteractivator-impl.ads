with Broca.Refs;
with CORBA;
with PortableServer.AdapterActivator;

package PortableServer.AdapterActivator.Impl is
   type Object is abstract new PortableServer.Servant_Base with private;

   --  FIXME
   --  This corresponds to _this in c++.
   type Object_Access is access all Object'Class;
   function To_Ref (Self : Object_Access)
                    return PortableServer.AdapterActivator.Ref;

   procedure Unknown_Adapter
     (Self   : in out Object;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String;
      Returns : out Boolean) is abstract;
private
   type Object is abstract new PortableServer.Servant_Base with
     null record;
end PortableServer.AdapterActivator.Impl;
