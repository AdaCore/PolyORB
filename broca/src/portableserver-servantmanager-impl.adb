with Broca.Refs;
with Broca.Poa;

package body PortableServer.ServantManager.Impl is
   function To_Ref (Self : Object_Access)
                    return PortableServer.ServantManager.Ref is
      Res : PortableServer.ServantManager.Ref;
   begin
      Set (Res,
           Broca.Refs.Ref_Acc
           (Broca.Poa.Create_Internal_Skeleton (Servant (Self))));
      return Res;
   end To_Ref;
end PortableServer.ServantManager.Impl;
