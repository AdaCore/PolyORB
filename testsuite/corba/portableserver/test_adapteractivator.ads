with CORBA;
with PortableServer.AdapterActivator;

package Test_AdapterActivator is

   type NullAdapter_Ref is new PortableServer.AdapterActivator.Ref
     with null record;

   function Unknown_Adapter
     (Self   : NullAdapter_Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
     return Boolean;

   type SimpleAdapter_Ref is new PortableServer.AdapterActivator.Ref
     with null record;

   function Unknown_Adapter
     (Self   : SimpleAdapter_Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
     return Boolean;

   procedure Run_Test_AdapterActivator;

end Test_AdapterActivator;
