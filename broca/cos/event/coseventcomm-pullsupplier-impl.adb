----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with CORBA.Impl;

with CosEventComm.PullSupplier.Skel;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPullConsumer;

with Broca.Basic_Startup; use  Broca.Basic_Startup;
with Broca.Soft_Links;    use  Broca.Soft_Links;

package body CosEventComm.PullSupplier.Impl is

   type Pull_Supplier_Record is
      record
         This    : Object_Ptr;
         Peer    : ProxyPullConsumer.Ref;
         Mutex   : Mutex_Access;
         Empty   : Boolean;
         Event   : CORBA.Any;
         Watcher : Watcher_Access;
      end record;

   ---------------------------------
   -- Connect_Proxy_Pull_Consumer --
   ---------------------------------

   procedure Connect_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPullConsumer.Ref)
   is
      My_Ref : PullSupplier.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Proxy;
      PullSupplier.Set (My_Ref, CORBA.Impl.Object_Ptr (Self.X.This));
      ProxyPullConsumer.Connect_Pull_Supplier (Proxy, My_Ref);
      Leave (Self.X.Mutex);
   end Connect_Proxy_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : CORBA.Object.Ref;

   begin
      Supplier         := new Object;
      Supplier.X       := new Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Empty := True;
      Create (Supplier.X.Mutex);
      Create (Supplier.X.Watcher);
      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier
     (Self : access Object)
   is
      Nil_Ref : ProxyPullConsumer.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.Mutex);
   end Disconnect_Pull_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Event   : CORBA.Any;
      Version : Version_Id;

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

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any) is

   begin
      Enter (Self.X.Mutex);
      Self.X.Empty := False;
      Self.X.Event := Data;
      Update (Self.X.Watcher);
      Leave (Self.X.Mutex);
   end Push;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out CORBA.Boolean;
      Returns   : out CORBA.Any) is
   begin
      Enter (Self.X.Mutex);
      if Self.X.Empty then
         Has_Event := False;

      else
         Has_Event := True;
         Returns := Self.X.Event;
         Self.X.Empty := True;
      end if;
      Leave (Self.X.Mutex);
   end Try_Pull;

end CosEventComm.PullSupplier.Impl;
