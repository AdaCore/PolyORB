----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;

with PortableServer;

with CosEventChannelAdmin.ProxyPushConsumer;

package CosEventComm.PushSupplier.Impl is

   --  This implementation is supposed to be application
   --  dependent. This is an example used to test the event service.

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure disconnect_push_supplier
     (Self : access Object);

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Connect_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPushConsumer.Ref);

   function Create return Object_Ptr;

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any);

private

   type Push_Supplier_Record;
   type Push_Supplier_Access is access Push_Supplier_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Push_Supplier_Access;
      end record;

end CosEventComm.PushSupplier.Impl;
