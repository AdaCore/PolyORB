with PortableServer.AdapterActivator.Impl;
with Broca.Poa;

package body PortableServer.AdapterActivator is
   function Unknown_Adapter
     (Self   : Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
      return Boolean
   is
      Res : Boolean;
   begin
      Impl.Unknown_Adapter
        (Impl.Object'Class
         (Broca.Poa.To_Internal_Skeleton (Self).P_Servant.all),
         Parent, Name, Res);
      return Res;
   end Unknown_Adapter;
end PortableServer.AdapterActivator;
