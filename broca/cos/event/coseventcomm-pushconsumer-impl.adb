----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPushSupplier;

with CosEventComm.PushConsumer.Helper;
with CosEventComm.PushConsumer.Skel;

with Broca.Server_Tools; use  Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Impl;

with PortableServer; use PortableServer;

package body CosEventComm.PushConsumer.Impl is

   type Push_Consumer_Record is
      record
         This    : Object_Ptr;
         Peer    : ProxyPushSupplier.Ref;
         Empty   : Boolean;
         Event   : CORBA.Any;
         Watcher : Watcher_Access;
         Mutex   : Mutex_Access;
      end record;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : PushConsumer.Ref;

   begin
      Consumer         := new Object;
      Consumer.X       := new Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Empty := True;
      Create (Consumer.X.Watcher);
      Create (Consumer.X.Mutex);
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

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

   ------------------------------
   -- Disconnect_Push_Consumer --
   ------------------------------

   procedure Disconnect_Push_Consumer
     (Self : access Object)
   is
      Nil_Ref : ProxyPushSupplier.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.Mutex);
   end Disconnect_Push_Consumer;

   ---------------------------------
   -- Connect_Proxy_Push_Supplier --
   ---------------------------------

   procedure Connect_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPushSupplier.Ref)
   is
      My_Ref : PushConsumer.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Proxy;
      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPushSupplier.Connect_Push_Consumer (Proxy, My_Ref);
      Leave (Self.X.Mutex);
   end Connect_Proxy_Push_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object) return CORBA.Any
   is
      Event   : CORBA.Any;
      Version : Version_Id;

   begin
      loop
         Enter (Self.X.Mutex);
         if not Self.X.Empty then
            Self.X.Empty := True;
            Event := Self.X.Event;
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
     (Self : access Object;
      Done : out CORBA.Boolean;
      Data : out CORBA.Any) is
   begin
      Enter (Self.X.Mutex);
      if Self.X.Empty then
         Done := False;

      else
         Done := True;
         Self.X.Empty := True;
         Data := Self.X.Event;
      end if;
      Leave (Self.X.Mutex);
   end Try_Pull;

end CosEventComm.PushConsumer.Impl;
