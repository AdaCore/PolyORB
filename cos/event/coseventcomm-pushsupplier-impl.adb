----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm.PushSupplier.Helper;
with CosEventComm.PushSupplier.Skel;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPushConsumer;

with Broca.Server_Tools; use  Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Impl;

with PortableServer; use PortableServer;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventComm.PushSupplier.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("pushsupplier");
   procedure O is new Broca.Debug.Output (Flag);

   type Push_Supplier_Record is
      record
         This  : Object_Ptr;
         Peer  : ProxyPushConsumer.Ref;
      end record;

   ---------------------------------
   -- Connect_Proxy_Push_Consumer --
   ---------------------------------

   procedure Connect_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPushConsumer.Ref)
   is
      My_Ref : PushSupplier.Ref;

   begin
      pragma Debug (O ("connect proxy push supplier to push consumer"));

      Enter_Critical_Section;
      if not ProxyPushConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave_Critical_Section;

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPushConsumer.Connect_Push_Supplier (Proxy, My_Ref);
   end Connect_Proxy_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : PushSupplier.Ref;

   begin
      pragma Debug (O ("create push supplier"));

      Supplier := new Object;
      Supplier.X := new Push_Supplier_Record;
      Supplier.X.This := Supplier;
      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Push_Supplier --
   ------------------------------

   procedure Disconnect_Push_Supplier
     (Self : access Object)
   is
      Peer    : ProxyPushConsumer.Ref;
      Nil_Ref : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("disconnect push supplier"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave_Critical_Section;

      if not ProxyPushConsumer.Is_Nil (Peer) then
         ProxyPushConsumer.Disconnect_Push_Consumer (Peer);
      end if;
   end Disconnect_Push_Supplier;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any)
   is
      Peer : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("push new data to push supplier"));

      Enter_Critical_Section;
      Peer := Self.X.Peer;
      Leave_Critical_Section;

      if ProxyPushConsumer.Is_Nil (Peer) then
         raise Disconnected;
      end if;

      ProxyPushConsumer.Push (Peer, Data);
   end Push;

end CosEventComm.PushSupplier.Impl;
