----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with PortableServer;

with CosEventChannelAdmin.SupplierAdmin.Impl;

package CosEventChannelAdmin.ProxyPushConsumer.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Connect_Push_Supplier
     (Self          : access Object;
      Push_Supplier : in CosEventComm.PushSupplier.Ref);

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any);

   procedure Disconnect_Push_Consumer
     (Self : access Object);

   ------------------------
   -- AdaBroker specific --
   ------------------------

   function Create
     (Admin : CosEventChannelAdmin.SupplierAdmin.Impl.Object_Ptr)
     return Object_Ptr;

private

   type Proxy_Push_Consumer_Record;
   type Proxy_Push_Consumer_Access is access all Proxy_Push_Consumer_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Proxy_Push_Consumer_Access;
      end record;

end CosEventChannelAdmin.ProxyPushConsumer.Impl;
