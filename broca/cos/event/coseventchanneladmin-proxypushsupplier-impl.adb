----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm;  use CosEventComm;

with CosEventComm.PushConsumer;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ConsumerAdmin;

with CosEventChannelAdmin.ProxyPushSupplier.Helper;
with CosEventChannelAdmin.ProxyPushSupplier.Skel;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer; use PortableServer;

with CORBA.Object;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.ProxyPushSupplier.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("proxypushsupplier");
   procedure O is new Broca.Debug.Output (Flag);

    type Proxy_Push_Supplier_Record is
       record
          This   : Object_Ptr;
          Peer   : PushConsumer.Ref;
          Admin  : ConsumerAdmin.Impl.Object_Ptr;
       end record;

   ---------------------------
   -- Connect_Push_Consumer --
   ---------------------------

   procedure Connect_Push_Consumer
     (Self          : access Object;
      Push_Consumer : in CosEventComm.PushConsumer.Ref) is
   begin
      pragma Debug (O ("connect push consumer to proxy push supplier"));

      Enter_Critical_Section;
      if not PushConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Push_Consumer;
      Leave_Critical_Section;
   end Connect_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create (Admin : ConsumerAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Supplier : ProxyPushSupplier.Impl.Object_Ptr;
      My_Ref   : ProxyPushSupplier.Ref;

   begin
      pragma Debug (O ("create proxy push supplier"));

      Supplier         := new Object;
      Supplier.X       := new Proxy_Push_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;
      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Push_Supplier --
   ------------------------------

   procedure Disconnect_Push_Supplier
     (Self : access Object)
   is
      Peer    : PushConsumer.Ref;
      Nil_Ref : PushConsumer.Ref;

   begin
      pragma Debug (O ("disconnect proxy push supplier"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave_Critical_Section;

      if PushConsumer.Is_Nil (Peer) then
         PushConsumer.Disconnect_Push_Consumer (Peer);
      end if;
   end Disconnect_Push_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      pragma Debug
        (O ("post new data from proxy push supplier to push consumer"));

      begin
         PushConsumer.Push (Self.X.Peer, Data);
      exception when others =>
         null;
      end;
   end Post;

end CosEventChannelAdmin.ProxyPushSupplier.Impl;
