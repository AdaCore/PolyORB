----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with PortableServer;

with CosEventChannelAdmin.ConsumerAdmin.Impl;

package CosEventChannelAdmin.ProxyPullSupplier.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Connect_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : in CosEventComm.PullConsumer.Ref);

   function Pull
     (Self : access Object)
     return CORBA.Any;

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out CORBA.Boolean;
      Returns   : out CORBA.Any);

   procedure Disconnect_Pull_Supplier
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

   type Proxy_Pull_Supplier_Record;
   type Proxy_Pull_Supplier_Access is access all Proxy_Pull_Supplier_Record;

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Proxy_Pull_Supplier_Access;
      end record;

end CosEventChannelAdmin.ProxyPullSupplier.Impl;
