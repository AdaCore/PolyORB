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

with CosEventChannelAdmin.ConsumerAdmin.Helper;
with CosEventChannelAdmin.ConsumerAdmin.Skel;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Sequences.Unbounded;

with CORBA.Impl;

with PortableServer; use PortableServer;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.ConsumerAdmin.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("consumeradmin");
   procedure O is new Broca.Debug.Output (Flag);

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
      end record;

   ------------
   -- Create --
   ------------

   function Create (Channel : EventChannel.Impl.Object_Ptr)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : ConsumerAdmin.Ref;

   begin
      pragma Debug (O ("create consumer admin"));

      Consumer        := new Object;
      Consumer.X      := new Consumer_Admin_Record;
      Consumer.X.This    := Consumer;
      Consumer.X.Channel := Channel;
      Initiate_Servant (Servant (Consumer), My_Ref);
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
      pragma Debug (O ("obtain proxy pull supplier from consumer admin"));

      Enter_Critical_Section;
      Supplier := ProxyPullSupplier.Impl.Create (Self.X.This);
      PullSuppliers.Append (Self.X.Pulls, Supplier);
      Leave_Critical_Section;
      Servant_To_Reference (Servant (Supplier), Its_Ref);
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
      pragma Debug (O ("obtain proxy push supplier from consumer admin"));

      Enter_Critical_Section;
      Supplier := ProxyPushSupplier.Impl.Create (Self.X.This);
      PushSuppliers.Append (Self.X.Pushs, Supplier);
      Leave_Critical_Section;
      Servant_To_Reference (Servant (Supplier), Its_Ref);
      return Its_Ref;
   end Obtain_Push_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      Enter_Critical_Section;
      declare
         Pulls : PullSuppliers.Element_Array
           := PullSuppliers.To_Element_Array (Self.X.Pulls);
         Pushs : PushSuppliers.Element_Array
           := PushSuppliers.To_Element_Array (Self.X.Pushs);
      begin
         Leave_Critical_Section;
         pragma Debug (O ("post new data to proxy pull suppliers"));

         for I in Pulls'Range loop
            ProxyPullSupplier.Impl.Post (Pulls (I), Data);
         end loop;
         pragma Debug (O ("post new data to proxy push suppliers"));
         for I in Pushs'Range loop
            ProxyPushSupplier.Impl.Post (Pushs (I), Data);
         end loop;
      end;
   end Post;

end CosEventChannelAdmin.ConsumerAdmin.Impl;
