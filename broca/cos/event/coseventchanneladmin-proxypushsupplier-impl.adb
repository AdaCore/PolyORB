----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm;  use CosEventComm;

with CosEventComm.PushConsumer;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ConsumerAdmin;

with CosEventChannelAdmin.ProxyPushSupplier.Skel;

with Broca.Basic_Startup; use Broca.Basic_Startup;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer;

with CORBA.Object;

package body CosEventChannelAdmin.ProxyPushSupplier.Impl is

    type Proxy_Push_Supplier_Record is
       record
          This   : Object_Ptr;
          Peer   : PushConsumer.Ref;
          Admin  : ConsumerAdmin.Impl.Object_Ptr;
          Mutex  : Mutex_Access;
       end record;

   ---------------------------
   -- Connect_Push_Consumer --
   ---------------------------

   procedure Connect_Push_Consumer
     (Self          : access Object;
      Push_Consumer : in CosEventComm.PushConsumer.Ref) is
   begin
      Enter (Self.X.Mutex);

      --  Check a peer is not already connected
      if not PushConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.Mutex);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Push_Consumer;
      Leave (Self.X.Mutex);

   exception
      when Disconnected =>
         Destroy (Self.X.Mutex);
         raise Disconnected;
   end Connect_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create (Admin : ConsumerAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Supplier : ProxyPushSupplier.Impl.Object_Ptr;
      My_Ref   : CORBA.Object.Ref;

   begin
      Supplier         := new Object;
      Supplier.X       := new Proxy_Push_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;
      Create (Supplier.X.Mutex);
      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Push_Supplier --
   ------------------------------

   procedure Disconnect_Push_Supplier
     (Self : access Object)
   is
      Nil_Ref : PushConsumer.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.Mutex);
   end Disconnect_Push_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      Enter (Self.X.Mutex);
      PushConsumer.Push (Self.X.Peer, Data);
      Leave (Self.X.Mutex);
   end Post;

end CosEventChannelAdmin.ProxyPushSupplier.Impl;
