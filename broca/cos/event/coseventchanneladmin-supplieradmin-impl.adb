----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.EventChannel.Impl;

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPullConsumer.Impl;

with CosEventChannelAdmin.ProxyPushConsumer;
with CosEventChannelAdmin.ProxyPushConsumer.Impl;

with CosEventChannelAdmin.SupplierAdmin.Helper;
with CosEventChannelAdmin.SupplierAdmin.Skel;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Impl;

with CORBA.Sequences.Unbounded;

with PortableServer; use PortableServer;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.SupplierAdmin.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("consumeradmin");
   procedure O is new Broca.Debug.Output (Flag);

   package PullConsumers is
      new CORBA.Sequences.Unbounded (ProxyPullConsumer.Impl.Object_Ptr);

   package PushConsumers is
      new CORBA.Sequences.Unbounded (ProxyPushConsumer.Impl.Object_Ptr);

   type Supplier_Admin_Record is
      record
         This    : Object_Ptr;
         Channel : EventChannel.Impl.Object_Ptr;
         Pushs   : PushConsumers.Sequence;
         Pulls   : PullConsumers.Sequence;
      end record;

   ------------
   -- Create --
   ------------

   function Create (Channel : EventChannel.Impl.Object_Ptr)
     return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : SupplierAdmin.Ref;

   begin
      pragma Debug (O ("create supplier admin"));

      Supplier        := new Object;
      Supplier.X      := new Supplier_Admin_Record;
      Supplier.X.This    := Supplier;
      Supplier.X.Channel := Channel;
      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   --------------------------
   -- Obtain_Pull_Consumer --
   --------------------------

   function Obtain_Pull_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullConsumer.Ref
   is
      Consumer : ProxyPullConsumer.Impl.Object_Ptr;
      Its_Ref  : ProxyPullConsumer.Ref;

   begin
      pragma Debug (O ("obtain proxy pull consumer from supplier admin"));

      Enter_Critical_Section;
      Consumer := ProxyPullConsumer.Impl.Create (Self.X.This);
      PullConsumers.Append (Self.X.Pulls, Consumer);
      Leave_Critical_Section;
      Servant_To_Reference (Servant (Consumer), Its_Ref);
      return Its_Ref;
   end Obtain_Pull_Consumer;

   --------------------------
   -- Obtain_Push_Consumer --
   --------------------------

   function Obtain_Push_Consumer
     (Self : access Object)
     return ProxyPushConsumer.Ref
   is
      Consumer : ProxyPushConsumer.Impl.Object_Ptr;
      Its_Ref  : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("obtain proxy push consumer from supplier admin"));

      Enter_Critical_Section;
      Consumer := ProxyPushConsumer.Impl.Create (Self.X.This);
      PushConsumers.Append (Self.X.Pushs, Consumer);
      Leave_Critical_Section;
      Servant_To_Reference (Servant (Consumer), Its_Ref);
      return Its_Ref;
   end Obtain_Push_Consumer;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      pragma Debug (O ("post new data from supplier admin to channel"));

      EventChannel.Impl.Post (Self.X.Channel, Data);
   end Post;

end CosEventChannelAdmin.SupplierAdmin.Impl;
