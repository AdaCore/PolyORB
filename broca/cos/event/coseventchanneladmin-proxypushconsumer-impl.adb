----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm;              use CosEventComm;

with CosEventComm.PushSupplier;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPushConsumer.Helper;
with CosEventChannelAdmin.ProxyPushConsumer.Skel;

with CosEventChannelAdmin.SupplierAdmin.Impl;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer; use PortableServer;

with CORBA.Object;
with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.ProxyPushConsumer.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("proxypushconsumer");
   procedure O is new Broca.Debug.Output (Flag);

   type Proxy_Push_Consumer_Record is
      record
         This   : Object_Ptr;
         Peer   : PushSupplier.Ref;
         Admin  : SupplierAdmin.Impl.Object_Ptr;
      end record;

   ---------------------------
   -- Connect_Push_Supplier --
   ---------------------------

   procedure Connect_Push_Supplier
     (Self          : access Object;
      Push_Supplier : in CosEventComm.PushSupplier.Ref) is
   begin
      pragma Debug (O ("connect push supplier to proxy push consumer"));

      Enter_Critical_Section;
      if not PushSupplier.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Push_Supplier;
      Leave_Critical_Section;
   end Connect_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create (Admin : SupplierAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("create proxy push consumer"));

      Consumer         := new Object;
      Consumer.X       := new Proxy_Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin;
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Push_Consumer --
   ------------------------------

   procedure Disconnect_Push_Consumer
     (Self : access Object)
   is
      Peer    : PushSupplier.Ref;
      Nil_Ref : PushSupplier.Ref;

   begin
      pragma Debug (O ("disconnect proxy push consumer"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave_Critical_Section;

      if not PushSupplier.Is_Nil (Peer) then
         PushSupplier.Disconnect_Push_Supplier (Peer);
      end if;
   end Disconnect_Push_Consumer;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      pragma Debug
        (O ("push new data from proxy push consumer to supplier admin"));

      SupplierAdmin.Impl.Post (Self.X.Admin, Data);
   end Push;

end CosEventChannelAdmin.ProxyPushConsumer.Impl;
