----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with PortableServer;

with CosEventChannelAdmin.SupplierAdmin.Impl;

package CosEventChannelAdmin.ProxyPullConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   -----------------------
   -- ProxyPullConsumer --
   -----------------------

   procedure Connect_Pull_Supplier
     (Self          : access Object;
      Pull_Supplier : in CosEventComm.PullSupplier.Ref);

   ------------------
   -- PullConsumer --
   ------------------

   procedure Disconnect_Pull_Consumer
     (Self : access Object);

   ------------------------
   -- AdaBroker specific --
   ------------------------

   function Create
     (Admin : CosEventChannelAdmin.SupplierAdmin.Impl.Object_Ptr)
     return Object_Ptr;

private

   type Proxy_Pull_Consumer_Record;
   type Proxy_Pull_Consumer_Access is access all Proxy_Pull_Consumer_Record;

   type Object is new PortableServer.Servant_Base with
      record
         X : Proxy_Pull_Consumer_Access;
      end record;

end CosEventChannelAdmin.ProxyPullConsumer.Impl;
