with Broca.Refs;
with Broca.POA;
with PortableServer.AdapterActivator;

package body PortableServer.AdapterActivator.Impl is
   function To_Ref (Self : Object_Access)
                    return PortableServer.AdapterActivator.Ref is
      Res : PortableServer.AdapterActivator.Ref;
   begin
      Set (Res,
           Broca.Refs.Ref_Ptr
           (Broca.POA.Create_Internal_Skeleton (Servant (Self))));
      return Res;
   end To_Ref;
end PortableServer.AdapterActivator.Impl;
