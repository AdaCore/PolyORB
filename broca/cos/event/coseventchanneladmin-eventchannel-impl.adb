----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventChannelAdmin.SupplierAdmin;
with CosEventChannelAdmin.SupplierAdmin.Impl;

with CosEventChannelAdmin.ConsumerAdmin;
with CosEventChannelAdmin.ConsumerAdmin.Impl;

with CosEventChannelAdmin.EventChannel.Helper;
with CosEventChannelAdmin.EventChannel.Skel;

with Broca.Server_Tools; use  Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer; use PortableServer;

with CORBA.Impl;

package body CosEventChannelAdmin.EventChannel.Impl is

   -------------
   -- Channel --
   -------------

   type Event_Channel_Record is
      record
         This     : Object_Ptr;
         Consumer : ConsumerAdmin.Impl.Object_Ptr;
         Supplier : SupplierAdmin.Impl.Object_Ptr;
         Mutex    : Mutex_Access;
      end record;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Channel : Object_Ptr;
      My_Ref  : EventChannel.Ref;

   begin
      Channel            := new Object;
      Channel.X          := new Event_Channel_Record;
      Channel.X.This     := Channel;
      Channel.X.Consumer := ConsumerAdmin.Impl.Create (Channel);
      Channel.X.Supplier := SupplierAdmin.Impl.Create (Channel);
      Create (Channel.X.Mutex);
      Initiate_Servant (Servant (Channel), My_Ref);
      return Channel;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object) is
   begin
      null;
   end Destroy;

   -------------------
   -- For_Consumers --
   -------------------

   function For_Consumers
     (Self : access Object)
     return ConsumerAdmin.Ref
   is
      R : ConsumerAdmin.Ref;

   begin
      Servant_To_Reference (Servant (Self.X.Consumer), R);
      return R;
   end For_Consumers;

   -------------------
   -- For_Suppliers --
   -------------------

   function For_Suppliers
     (Self : access Object)
     return CosEventChannelAdmin.SupplierAdmin.Ref
   is
      R : SupplierAdmin.Ref;

   begin
      Servant_To_Reference (Servant (Self.X.Supplier), R);
      return R;
   end For_Suppliers;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      ConsumerAdmin.Impl.Post (Self.X.Consumer, Data);
   end Post;

end CosEventChannelAdmin.EventChannel.Impl;
