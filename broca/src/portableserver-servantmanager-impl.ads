package PortableServer.ServantManager.Impl is
   type Object is abstract new PortableServer.Servant_Base with private;

   --  FIXME
   type Object_Access is access all Object'Class;
   function To_Ref (Self : Object_Access)
                    return PortableServer.ServantManager.Ref;

private
   type Object is abstract new PortableServer.Servant_Base with null record;
end PortableServer.ServantManager.Impl;
