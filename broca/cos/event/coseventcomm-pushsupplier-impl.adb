----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm.PushSupplier.Skel;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPushConsumer;

with Broca.Basic_Startup; use  Broca.Basic_Startup;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Impl;

package body CosEventComm.PushSupplier.Impl is

   type Push_Supplier_Record is
      record
         This  : Object_Ptr;
         Peer  : ProxyPushConsumer.Ref;
         Mutex : Mutex_Access;
      end record;

   ------------------------------
   -- Disconnect_Push_Supplier --
   ------------------------------

   procedure Disconnect_Push_Supplier
     (Self : access Object)
   is
      Nil_Ref : ProxyPushConsumer.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.Mutex);
   end Disconnect_Push_Supplier;

   ---------------------------------
   -- Connect_Proxy_Push_Consumer --
   ---------------------------------

   procedure Connect_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPushConsumer.Ref)
   is
      My_Ref : PushSupplier.Ref;

   begin
      Enter (Self.X.Mutex);
      Self.X.Peer := Proxy;
      PushSupplier.Set (My_Ref, CORBA.Impl.Object_Ptr (Self.X.This));
      ProxyPushConsumer.Connect_Push_Supplier (Proxy, My_Ref);
      Leave (Self.X.Mutex);
   end Connect_Proxy_Push_Consumer;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      Enter (Self.X.Mutex);
      ProxyPushConsumer.Push (Self.X.Peer, Data);
      Leave (Self.X.Mutex);
   end Push;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : CORBA.Object.Ref;

   begin
      Supplier := new Object;
      Supplier.X := new Push_Supplier_Record;
      Supplier.X.This := Supplier;
      Create (Supplier.X.Mutex);
      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

end CosEventComm.PushSupplier.Impl;
