----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with PortableServer;

with CosEventChannelAdmin.ConsumerAdmin.Impl;

package CosEventChannelAdmin.ProxyPushSupplier.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Connect_Push_Consumer
     (Self          : access Object;
      Push_Consumer : in CosEventComm.PushConsumer.Ref);

   procedure Disconnect_Push_Supplier
     (Self : access Object);

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any);

   function Create
     (Admin : CosEventChannelAdmin.ConsumerAdmin.Impl.Object_Ptr)
     return Object_Ptr;

private

   type Proxy_Push_Supplier_Record;
   type Proxy_Push_Supplier_Access is access Proxy_Push_Supplier_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Proxy_Push_Supplier_Access;
      end record;

end CosEventChannelAdmin.ProxyPushSupplier.Impl;
