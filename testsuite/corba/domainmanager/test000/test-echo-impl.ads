
with PortableServer;

package Test.Echo.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

private

   type Object is new PortableServer.Servant_Base with null record;

end Test.Echo.Impl;
