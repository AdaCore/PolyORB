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

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventComm.PushConsumer.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("pushconsumer");
   procedure O is new Broca.Debug.Output (Flag);

   type Push_Consumer_Record is
      record
         This    : Object_Ptr;
         Peer    : ProxyPushSupplier.Ref;
         Empty   : Boolean;
         Event   : CORBA.Any;
         Watcher : Watcher_Access;
      end record;

   ---------------------------------
   -- Connect_Proxy_Push_Supplier --
   ---------------------------------

   procedure Connect_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPushSupplier.Ref)
   is
      My_Ref : PushConsumer.Ref;

   begin
      pragma Debug (O ("connect proxy push consumer to push supplier"));

      Enter_Critical_Section;
      if not ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave_Critical_Section;

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPushSupplier.Connect_Push_Consumer (Proxy, My_Ref);
   end Connect_Proxy_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : PushConsumer.Ref;

   begin
      pragma Debug (O ("create push consumer"));

      Consumer         := new Object;
      Consumer.X       := new Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Empty := True;
      Create (Consumer.X.Watcher);
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Push_Consumer --
   ------------------------------

   procedure Disconnect_Push_Consumer
     (Self : access Object)
   is
      Peer    : ProxyPushSupplier.Ref;
      Nil_Ref : ProxyPushSupplier.Ref;

   begin
      pragma Debug (O ("disconnect push consumer"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Update (Self.X.Watcher);
      Leave_Critical_Section;

      if not ProxyPushSupplier.Is_Nil (Peer) then
         ProxyPushSupplier.Disconnect_Push_Supplier (Peer);
      end if;
   end Disconnect_Push_Consumer;

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
         pragma Debug (O ("attempt to pull new data from push consumer"));

         Enter_Critical_Section;
         if ProxyPushSupplier.Is_Nil (Self.X.Peer) then
            Leave_Critical_Section;
            raise Disconnected;
         end if;

         if not Self.X.Empty then
            Self.X.Empty := True;
            Event := Self.X.Event;
            Leave_Critical_Section;
            exit;
         end if;
         Lookup (Self.X.Watcher, Version);
         Leave_Critical_Section;
         Differ (Self.X.Watcher, Version);
      end loop;
      pragma Debug (O ("succeed to pull new data from push consumer"));

      return Event;
   end Pull;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      pragma Debug (O ("push new data to push consumer"));

      Enter_Critical_Section;
      Self.X.Empty := False;
      Self.X.Event := Data;
      Update (Self.X.Watcher);
      Leave_Critical_Section;
   end Push;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self : access Object;
      Done : out CORBA.Boolean;
      Data : out CORBA.Any) is
   begin
      pragma Debug (O ("try to pull new data from push consumer"));

      Enter_Critical_Section;
      if ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise Disconnected;
      end if;

      if Self.X.Empty then
         Done := False;

      else
         Done := True;
         Self.X.Empty := True;
         Data := Self.X.Event;
      end if;
      Leave_Critical_Section;
   end Try_Pull;

end CosEventComm.PushConsumer.Impl;
