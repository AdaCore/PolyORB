----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm; use CosEventComm;

with CosEventComm.PullConsumer;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPullSupplier.Helper;
with CosEventChannelAdmin.ProxyPullSupplier.Skel;

with CosEventChannelAdmin.ConsumerAdmin.Impl;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer; use PortableServer;

with CORBA.Object;

package body CosEventChannelAdmin.ProxyPullSupplier.Impl is

   type Proxy_Pull_Supplier_Record is
      record
         This    : Object_Ptr;
         Peer    : PullConsumer.Ref;
         Admin   : ConsumerAdmin.Impl.Object_Ptr;
         Event   : CORBA.Any;
         Empty   : Boolean;
         Watcher : Watcher_Access;
         Mutex   : Mutex_Access;
      end record;

   ---------------------------
   -- Connect_Pull_Consumer --
   ---------------------------

   procedure Connect_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : in PullConsumer.Ref)
   is
   begin
      Enter (Self.X.Mutex);

      --  Check a peer is not already connected
      if not PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.Mutex);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Pull_Consumer;
      Leave (Self.X.Mutex);
   exception when Disconnected =>
      Destroy (Self.X.Mutex);
      raise Disconnected;

   end Connect_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create (Admin : ConsumerAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : ProxyPullSupplier.Ref;

   begin
      Supplier         := new Object;
      Supplier.X       := new Proxy_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;
      Supplier.X.Empty := True;
      Create (Supplier.X.Mutex);
      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier
     (Self : access Object)
   is
      Nil_Ref : PullConsumer.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.Mutex);
   end Disconnect_Pull_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      Enter (Self.X.Mutex);
      Self.X.Event := Data;
      Self.X.Empty := False;
      Update (Self.X.Watcher);
      Leave (Self.X.Mutex);
   end Post;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Version : Version_Id;
      Event   : CORBA.Any;

   begin
      loop
         Enter (Self.X.Mutex);
         if not Self.X.Empty then
            Event := Self.X.Event;
            Self.X.Empty := True;
            Leave (Self.X.Mutex);
            exit;
         end if;
         Lookup (Self.X.Watcher, Version);
         Leave (Self.X.Mutex);
         Differ (Self.X.Watcher, Version);
      end loop;

      return Event;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out CORBA.Boolean;
      Returns   : out CORBA.Any)
   is
   begin
      Enter (Self.X.Mutex);
      if Self.X.Empty then
         Has_Event := False;

      else
         Has_Event    := True;
         Returns      := Self.X.Event;
         Self.X.Empty := True;
      end if;
      Leave (Self.X.Mutex);
   end Try_Pull;

end CosEventChannelAdmin.ProxyPullSupplier.Impl;
