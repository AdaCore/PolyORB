----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;

with PortableServer;

with CosEventChannelAdmin.ProxyPushSupplier;

package CosEventComm.PushConsumer.Impl is

   --  This implementation is supposed to be application
   --  dependent. This is an example used to test the event service.

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any);
   --  Call by proxy to push an event

   procedure Disconnect_Push_Consumer
     (Self : access Object);
   --  Call by proxy to disconnect

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Connect_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPushSupplier.Ref);
   --  Call by application to connect object with proxy

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   function Pull
     (Self : access Object) return CORBA.Any;
   --  Call by application to consume an event

   procedure Try_Pull
     (Self : access Object;
      Done : out CORBA.Boolean;
      Data : out CORBA.Any);
   --  Call by application to try to consume an event

private

   type Push_Consumer_Record;
   type Push_Consumer_Access is access Push_Consumer_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Push_Consumer_Access;
      end record;

end CosEventComm.PushConsumer.Impl;
