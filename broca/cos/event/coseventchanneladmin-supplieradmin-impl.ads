----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPushConsumer;
with CosEventChannelAdmin.EventChannel.Impl;
with PortableServer;

package CosEventChannelAdmin.SupplierAdmin.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function Obtain_Push_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPushConsumer.Ref;

   function Obtain_Pull_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullConsumer.Ref;

   function Create
     (Channel : CosEventChannelAdmin.EventChannel.Impl.Object_Ptr)
     return Object_Ptr;

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any);

private

   type Supplier_Admin_Record;
   type Supplier_Admin_Access is access all Supplier_Admin_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Supplier_Admin_Access;
      end record;

end CosEventChannelAdmin.SupplierAdmin.Impl;
