
package PortableServer.AdapterActivator is

   type Ref is new CORBA.Object.Ref with null record;

   function Unknown_Adapter
     (Self   : Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
     return Boolean;

end PortableServer.AdapterActivator;
