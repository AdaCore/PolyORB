----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm; use CosEventComm;

with CosEventComm.PullSupplier;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.SupplierAdmin.Impl;

with CosEventChannelAdmin.ProxyPullConsumer.Skel;

with CosEventChannelAdmin.SupplierAdmin.Impl;

with Broca.Basic_Startup; use Broca.Basic_Startup;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer;

with CORBA.Object;

package body CosEventChannelAdmin.ProxyPullConsumer.Impl is

   task type Proxy_Pull_Consumer_Engin is
      entry Connect
        (Consumer : in Object_Ptr;
         Supplier : in PullSupplier.Ref);
      entry Stop;
   end Proxy_Pull_Consumer_Engin;

   type Proxy_Pull_Consumer_Engin_Access is access Proxy_Pull_Consumer_Engin;

   type Proxy_Pull_Consumer_Record is
      record
         This   : Object_Ptr;
         Peer   : PullSupplier.Ref;
         Admin  : SupplierAdmin.Impl.Object_Ptr;
         Engin  : Proxy_Pull_Consumer_Engin_Access;
         Mutex  : Mutex_Access;
      end record;

   -------------------------------
   -- Proxy_Pull_Consumer_Engin --
   -------------------------------

   task body Proxy_Pull_Consumer_Engin
   is
      This  : Object_Ptr;
      Peer  : PullSupplier.Ref;
      Event : CORBA.Any;

   begin
      select
         accept Connect
           (Consumer : Object_Ptr;
            Supplier : PullSupplier.Ref)
         do
            This := Consumer;
            Peer := Supplier;
         end Connect;
      or
         terminate;
      end select;
      loop
         Event := PullSupplier.Pull (Peer);
         Enter (This.X.Mutex);
         SupplierAdmin.Impl.Post (This.X.Admin, Event);
         Leave (This.X.Mutex);
      end loop;
   end Proxy_Pull_Consumer_Engin;

   ---------------------------
   -- Connect_Pull_Supplier --
   ---------------------------

   procedure Connect_Pull_Supplier
     (Self          : access Object;
      Pull_Supplier : in CosEventComm.PullSupplier.Ref) is
   begin
      Enter (Self.X.Mutex);

      --  Check a peer is not already connected
      if not PullSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self.X.Mutex);
         raise AlreadyConnected;
      end if;

      --  Start engin
      Self.X.Engin.Connect (Self.X.This, Pull_Supplier);

      Leave (Self.X.Mutex);
   end Connect_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create (Admin : SupplierAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : CORBA.Object.Ref;

   begin
      Consumer         := new Object;
      Consumer.X       := new Proxy_Pull_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin;
      Create (Consumer.X.Mutex);
      Initiate_Servant (PortableServer.Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Consumer --
   ------------------------------

   procedure Disconnect_Pull_Consumer
     (Self : access Object)
   is
      Nil_Ref : PullSupplier.Ref;

   begin
      Self.X.Peer := Nil_Ref;
   end Disconnect_Pull_Consumer;

end CosEventChannelAdmin.ProxyPullConsumer.Impl;
