----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPullSupplier;
with CosEventChannelAdmin.ProxyPullSupplier.Helper;
with CosEventChannelAdmin.ProxyPullSupplier.Impl;

with CosEventChannelAdmin.ProxyPushSupplier;
with CosEventChannelAdmin.ProxyPushSupplier.Helper;
with CosEventChannelAdmin.ProxyPushSupplier.Impl;

with CosEventChannelAdmin.ConsumerAdmin.Skel;

with Broca.Basic_Startup; use Broca.Basic_Startup;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Sequences.Unbounded;

with CORBA.Impl;

package body CosEventChannelAdmin.ConsumerAdmin.Impl is

   package PushSuppliers is
      new CORBA.Sequences.Unbounded (ProxyPushSupplier.Impl.Object_Ptr);

   package PullSuppliers is
      new CORBA.Sequences.Unbounded (ProxyPullSupplier.Impl.Object_Ptr);

   type Consumer_Admin_Record is
      record
         This    : Object_Ptr;
         Channel : EventChannel.Impl.Object_Ptr;
         Pushs   : PushSuppliers.Sequence;
         Pulls   : PullSuppliers.Sequence;
         Mutex   : Mutex_Access;
      end record;

   ------------
   -- Create --
   ------------

   function Create (Channel : EventChannel.Impl.Object_Ptr)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : CORBA.Object.Ref;

   begin
      Consumer        := new Object;
      Consumer.X      := new Consumer_Admin_Record;
      Create (Consumer.X.Mutex);
      Consumer.X.This    := Consumer;
      Consumer.X.Channel := Channel;
      Initiate_Servant (PortableServer.Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   --------------------------
   -- Obtain_Pull_Supplier --
   --------------------------

   function Obtain_Pull_Supplier
     (Self : access Object)
     return ProxyPullSupplier.Ref
   is
      Supplier : ProxyPullSupplier.Impl.Object_Ptr;
      Its_Ref  : ProxyPullSupplier.Ref;

   begin
      Enter  (Self.X.Mutex);
      Supplier := ProxyPullSupplier.Impl.Create (Self.X.This);
      ProxyPullSupplier.Set (Its_Ref, CORBA.Impl.Object_Ptr (Supplier));
      PullSuppliers.Append (Self.X.Pulls, Supplier);
      Leave  (Self.X.Mutex);
      return Its_Ref;
   end Obtain_Pull_Supplier;

   --------------------------
   -- Obtain_Push_Supplier --
   --------------------------

   function Obtain_Push_Supplier
     (Self : access Object)
     return ProxyPushSupplier.Ref
   is
      Supplier : ProxyPushSupplier.Impl.Object_Ptr;
      Its_Ref  : ProxyPushSupplier.Ref;

   begin
      Enter  (Self.X.Mutex);
      Supplier := ProxyPushSupplier.Impl.Create (Self.X.This);
      ProxyPushSupplier.Set (Its_Ref, CORBA.Impl.Object_Ptr (Supplier));
      PushSuppliers.Append (Self.X.Pushs, Supplier);
      Leave  (Self.X.Mutex);
      return Its_Ref;
   end Obtain_Push_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      Enter (Self.X.Mutex);
      declare
         Pulls : PullSuppliers.Element_Array
           := PullSuppliers.To_Element_Array (Self.X.Pulls);
      begin
         for I in Pulls'Range loop
            ProxyPullSupplier.Impl.Post (Pulls (I), Data);
         end loop;
      end;
      declare
         Pushs : PushSuppliers.Element_Array
           := PushSuppliers.To_Element_Array (Self.X.Pushs);
      begin
         for I in Pushs'Range loop
            ProxyPushSupplier.Impl.Post (Pushs (I), Data);
         end loop;
      end;
      Leave (Self.X.Mutex);
   end Post;

end CosEventChannelAdmin.ConsumerAdmin.Impl;
