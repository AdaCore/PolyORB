with CORBA;
with PortableServer.ServantManager.Impl;
with PortableServer.POA;

package PortableServer.ServantActivator.Impl is

   type Object is abstract new PortableServer.ServantManager.Impl.Object with
     private;

   procedure Incarnate
     (Self    : in out Object;
      Oid     : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Returns : out Servant) is abstract;

   procedure Etherealize
     (Self    : in out Object;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref;
      Serv    : in PortableServer.Servant;
      Cleanup_In_Progress : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean) is abstract;

private
   type Object is abstract new PortableServer.ServantManager.Impl.Object with
     null record;
end PortableServer.ServantActivator.Impl;
