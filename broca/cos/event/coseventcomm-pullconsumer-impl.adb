----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm.PullConsumer.Skel;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPullSupplier;

with Broca.Basic_Startup; use  Broca.Basic_Startup;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Impl;

package body CosEventComm.PullConsumer.Impl is

   type Pull_Consumer_Record is
      record
         This  : Object_Ptr;
         Peer  : ProxyPullSupplier.Ref;
         Mutex : Mutex_Access;
      end record;

   ---------------------------------
   -- Connect_Proxy_Pull_Supplier --
   ---------------------------------

   procedure Connect_Proxy_Pull_Supplier
     (Self  : access Object;
      Proxy : in ProxyPullSupplier.Ref)
   is
      My_Ref : PullConsumer.Ref;

   begin
      Enter (Self.X.Mutex);
      if not ProxyPullSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self.X.Mutex);
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      PullConsumer.Set (My_Ref, CORBA.Impl.Object_Ptr (Self.X.This));
      ProxyPullSupplier.Connect_Pull_Consumer (Proxy, My_Ref);
      Leave (Self.X.Mutex);
   end Connect_Proxy_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : CORBA.Object.Ref;

   begin
      Consumer        := new Object;
      Consumer.X      := new Pull_Consumer_Record;
      Consumer.X.This := Consumer;
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
      Nil_Ref : ProxyPullSupplier.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.Mutex);
   end Disconnect_Pull_Consumer;

   ----------
   -- Pull --
   ----------

   function Pull (Self : access Object) return CORBA.Any
   is
      Peer : ProxyPullSupplier.Ref;

   begin
      Enter (Self.X.Mutex);
      Peer := Self.X.Peer;
      Leave (Self.X.Mutex);
      if ProxyPullSupplier.Is_Nil (Peer) then
         raise Disconnected;
      end if;
      return ProxyPullSupplier.Pull (Peer);
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self    : access Object;
      Done    : out CORBA.Boolean;
      Returns : out CORBA.Any)
   is
      Peer : ProxyPullSupplier.Ref;

   begin
      Enter (Self.X.Mutex);
      Peer := Self.X.Peer;
      Leave (Self.X.Mutex);
      if ProxyPullSupplier.Is_Nil (Peer) then
         raise Disconnected;
      end if;
      ProxyPullSupplier.Try_Pull (Peer, Done, Returns);
   end Try_Pull;

end CosEventComm.PullConsumer.Impl;
