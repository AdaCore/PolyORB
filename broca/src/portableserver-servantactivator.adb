with CORBA;
with PortableServer.ServantManager;
with PortableServer.POA;
with Broca.Poa;
with PortableServer.ServantActivator.Impl;

package body PortableServer.ServantActivator is

   function Incarnate
     (Self    : in Ref;
      Oid     : in ObjectId;
      Adapter : in PortableServer.POA.Ref) return Servant
   is
      Res : Servant;
   begin
      PortableServer.ServantActivator.Impl.Incarnate
        (Impl.Object'Class
         (Broca.Poa.To_Internal_Skeleton (Self).P_Servant.all),
         Oid, Adapter, Res);
      return Res;
   end Incarnate;

   procedure Etherealize
     (Self    : in Ref;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref;
      Serv    : in PortableServer.Servant;
      Cleanup_In_Progress : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean) is
   begin
      PortableServer.ServantActivator.Impl.Etherealize
        (Impl.Object'Class
         (Broca.Poa.To_Internal_Skeleton (Self).P_Servant.all),
         Oid, Adapter, Serv, Cleanup_In_Progress, Remaining_Activations);
   end Etherealize;
end PortableServer.ServantActivator;
