----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm;              use CosEventComm;

with CosEventComm.PushSupplier;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPushConsumer.Skel;
with CosEventChannelAdmin.SupplierAdmin.Impl;

with Broca.Basic_Startup; use Broca.Basic_Startup;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer;

with CORBA.Object;

package body CosEventChannelAdmin.ProxyPushConsumer.Impl is

   type Proxy_Push_Consumer_Record is
      record
         This   : Object_Ptr;
         Peer   : PushSupplier.Ref;
         Admin  : SupplierAdmin.Impl.Object_Ptr;
         Mutex  : Mutex_Access;
      end record;

   ---------------------------
   -- Connect_Push_Supplier --
   ---------------------------

   procedure Connect_Push_Supplier
     (Self          : access Object;
      Push_Supplier : in CosEventComm.PushSupplier.Ref) is
   begin
      Enter (Self.X.Mutex);

      --  Check a peer is not already connected
      if not PushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self.X.Mutex);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Push_Supplier;
      Leave (Self.X.Mutex);

   exception when Disconnected =>
      Destroy (Self.X.Mutex);
      raise Disconnected;

   end Connect_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create (Admin : SupplierAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : CORBA.Object.Ref;

   begin
      Consumer         := new Object;
      Consumer.X       := new Proxy_Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin;
      Create (Consumer.X.Mutex);
      Initiate_Servant (PortableServer.Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Push_Consumer --
   ------------------------------

   procedure Disconnect_Push_Consumer
     (Self : access Object)
   is
      Nil_Ref : PushSupplier.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.Mutex);
   end Disconnect_Push_Consumer;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      Enter (Self.X.Mutex);
      SupplierAdmin.Impl.Post (Self.X.Admin, Data);
      Leave (Self.X.Mutex);
   end Push;

end CosEventChannelAdmin.ProxyPushConsumer.Impl;
