----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with PortableServer;

with CosEventChannelAdmin.ProxyPullSupplier;

package CosEventComm.PullConsumer.Impl is

   --  This implementation is supposed to be application
   --  dependent. This is an example used to test the event service.

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Disconnect_Pull_Consumer
     (Self : access Object);
   --  Call by proxy to disconnect

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Connect_Proxy_Pull_Supplier
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPullSupplier.Ref);
   --  Call by application to connect object with proxy

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   function Pull (Self : access Object) return CORBA.Any;
   --  Call by application to consume an event

   procedure Try_Pull
     (Self    : access Object;
      Done    : out CORBA.Boolean;
      Returns : out CORBA.Any);
   --  Call by application to try to consume an event

private

   type Pull_Consumer_Record;
   type Pull_Consumer_Access is access Pull_Consumer_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Pull_Consumer_Access;
      end record;

end CosEventComm.PullConsumer.Impl;
