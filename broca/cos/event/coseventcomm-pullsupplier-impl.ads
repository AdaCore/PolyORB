----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;

with PortableServer;

with CosEventChannelAdmin.ProxyPullConsumer;

package CosEventComm.PullSupplier.Impl is

   --  This implementation is supposed to be application
   --  dependent. This is an example used to test the event service.

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Disconnect_Pull_Supplier
     (Self : access Object);
   --  Call by proxy to disconnect

   function Pull
     (Self : access Object)
     return CORBA.Any;
   --  Call by proxy to pull an event

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out CORBA.Boolean;
      Returns   : out CORBA.Any);
   --  Call by proxy to try yo pull an event

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Connect_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPullConsumer.Ref);
   --  Call by application to connect object with proxy

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any);
   --  Call by application to produce an event

private

   type Pull_Supplier_Record;
   type Pull_Supplier_Access is access Pull_Supplier_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Pull_Supplier_Access;
      end record;

end CosEventComm.PullSupplier.Impl;
