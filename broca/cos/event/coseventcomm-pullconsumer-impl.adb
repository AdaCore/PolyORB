----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm.PullConsumer.Helper;
with CosEventComm.PullConsumer.Skel;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPullSupplier;

with Broca.Server_Tools; use  Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Impl;

with PortableServer; use PortableServer;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventComm.PullConsumer.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("pullconsumer");
   procedure O is new Broca.Debug.Output (Flag);

   type Pull_Consumer_Record is
      record
         This  : Object_Ptr;
         Peer  : ProxyPullSupplier.Ref;
      end record;

   ---------------------------------
   -- Connect_Proxy_Pull_Supplier --
   ---------------------------------

   procedure Connect_Proxy_Pull_Supplier
     (Self  : access Object;
      Proxy : in ProxyPullSupplier.Ref)
   is
      My_Ref : PullConsumer.Ref;

   begin
      pragma Debug (O ("connect proxy pull consumer to pull supplier"));

      Enter_Critical_Section;
      if not ProxyPullSupplier.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave_Critical_Section;

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPullSupplier.Connect_Pull_Consumer (Proxy, My_Ref);
   end Connect_Proxy_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : PullConsumer.Ref;

   begin
      pragma Debug (O ("create pull consumer"));

      Consumer        := new Object;
      Consumer.X      := new Pull_Consumer_Record;
      Consumer.X.This := Consumer;
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Consumer --
   ------------------------------

   procedure Disconnect_Pull_Consumer
     (Self : access Object)
   is
      Peer    : ProxyPullSupplier.Ref;
      Nil_Ref : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("disconnect pull consumer"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave_Critical_Section;

      if not ProxyPullSupplier.Is_Nil (Peer) then
         ProxyPullSupplier.Disconnect_Pull_Supplier (Peer);
      end if;
   end Disconnect_Pull_Consumer;

   ----------
   -- Pull --
   ----------

   function Pull (Self : access Object) return CORBA.Any
   is
      Peer : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("pull new data from pull consumer"));

      Enter_Critical_Section;
      Peer := Self.X.Peer;
      Leave_Critical_Section;

      if ProxyPullSupplier.Is_Nil (Peer) then
         raise Disconnected;
      end if;

      return ProxyPullSupplier.Pull (Peer);
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self    : access Object;
      Done    : out CORBA.Boolean;
      Returns : out CORBA.Any)
   is
      Peer : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("try to pull new data from pull consumer"));

      Enter_Critical_Section;
      Peer := Self.X.Peer;
      Leave_Critical_Section;

      if ProxyPullSupplier.Is_Nil (Peer) then
         raise Disconnected;
      end if;

      ProxyPullSupplier.Try_Pull (Peer, Done, Returns);
   end Try_Pull;

end CosEventComm.PullConsumer.Impl;
