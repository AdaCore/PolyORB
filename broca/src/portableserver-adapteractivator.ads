with CORBA;
with CORBA.Object;
with PortableServer;

package PortableServer.AdapterActivator is
   type Ref is new CORBA.Object.Ref with private;

   function Unknown_Adapter
     (Self   : Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
     return Boolean;

private
   type Ref is new CORBA.Object.Ref with null record;
end PortableServer.AdapterActivator;
