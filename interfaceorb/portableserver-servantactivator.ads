
package PortableServer.ServantActivator is

   type Ref is new PortableServer.ServantManager.Ref
     with null record;

   function Incarnate
     (Self    : in Ref;
      Oid     : in ObjectId;
      Adapter : in PortableServer.POA.Ref) return Servant;

   procedure Etheralize
     (Self    : in Ref;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref;
      Serv    : in PortableServer.Servant;
      Cleanup_In_Progress : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean);

end PortableServer.ServantActivator;
