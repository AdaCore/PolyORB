----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventChannelAdmin.SupplierAdmin;
with CosEventChannelAdmin.ConsumerAdmin;

with CosEventChannelAdmin.EventChannel;

with PortableServer;

package CosEventChannelAdmin.EventChannel.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function For_Consumers
     (Self : access Object)
     return CosEventChannelAdmin.ConsumerAdmin.Ref;

   function For_Suppliers
     (Self : access Object)
     return CosEventChannelAdmin.SupplierAdmin.Ref;

   procedure Destroy
     (Self : access Object);

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any);

   function Create return Object_Ptr;

private

   type Event_Channel_Record;
   type Event_Channel_Access is access Event_Channel_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Event_Channel_Access;
      end record;

end CosEventChannelAdmin.EventChannel.Impl;
