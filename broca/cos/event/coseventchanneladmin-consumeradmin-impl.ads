----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventChannelAdmin.ProxyPullSupplier;
with CosEventChannelAdmin.ProxyPushSupplier;

with CosEventChannelAdmin.EventChannel.Impl;

with PortableServer;

package CosEventChannelAdmin.ConsumerAdmin.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function Obtain_Push_Supplier
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPushSupplier.Ref;

   function Obtain_Pull_Supplier
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullSupplier.Ref;

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any);

   function Create
     (Channel : CosEventChannelAdmin.EventChannel.Impl.Object_Ptr)
     return Object_Ptr;

private

   type Consumer_Admin_Record;
   type Consumer_Admin_Access is access all Consumer_Admin_Record;

   type Object is new PortableServer.Servant_Base with
      record
         X : Consumer_Admin_Access;
      end record;

end CosEventChannelAdmin.ConsumerAdmin.Impl;
