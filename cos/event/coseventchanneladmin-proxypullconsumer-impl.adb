----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm; use CosEventComm;

with CosEventComm.PullSupplier;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.SupplierAdmin.Impl;

with CosEventChannelAdmin.ProxyPullConsumer.Helper;
with CosEventChannelAdmin.ProxyPullConsumer.Skel;

with CosEventChannelAdmin.SupplierAdmin.Impl;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer; use PortableServer;

with CORBA.Object;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.ProxyPullConsumer.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("proxypullconsumer");
   procedure O is new Broca.Debug.Output (Flag);

   task type Proxy_Pull_Consumer_Engin is
      entry Connect (Consumer : in Object_Ptr);
   end Proxy_Pull_Consumer_Engin;

   type Proxy_Pull_Consumer_Engin_Access is access Proxy_Pull_Consumer_Engin;

   type Proxy_Pull_Consumer_Record is
      record
         This   : Object_Ptr;
         Peer   : PullSupplier.Ref;
         Admin  : SupplierAdmin.Impl.Object_Ptr;
         Engin  : Proxy_Pull_Consumer_Engin_Access;
      end record;

   -------------------------------
   -- Proxy_Pull_Consumer_Engin --
   -------------------------------

   task body Proxy_Pull_Consumer_Engin
   is
      This  : Object_Ptr;
      Peer  : PullSupplier.Ref;
      Event : CORBA.Any;

   begin
      loop
         select
            accept Connect
              (Consumer : Object_Ptr)
            do
               This := Consumer;
            end Connect;
         or
            terminate;
         end select;

         loop
            Enter_Critical_Section;
            Peer := This.X.Peer;
            Leave_Critical_Section;

            exit when PullSupplier.Is_Nil (Peer);

            pragma Debug
              (O ("pull new data from proxy pull consumer engin"));

            begin
               Event := PullSupplier.Pull (Peer);
            exception when others =>
               exit;
            end;

            pragma Debug
              (O ("post new data from proxy pull consumer to admin"));

            SupplierAdmin.Impl.Post (This.X.Admin, Event);
         end loop;
      end loop;
   end Proxy_Pull_Consumer_Engin;

   ---------------------------
   -- Connect_Pull_Supplier --
   ---------------------------

   procedure Connect_Pull_Supplier
     (Self          : access Object;
      Pull_Supplier : in CosEventComm.PullSupplier.Ref) is
   begin
      pragma Debug (O ("connect pull supplier to proxy pull consumer"));

      Enter_Critical_Section;
      if not PullSupplier.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Pull_Supplier;

      --  Start engin
      if Self.X.Engin = null then
         Self.X.Engin := new Proxy_Pull_Consumer_Engin;
      end if;
      Self.X.Engin.Connect (Self.X.This);
      Leave_Critical_Section;
   end Connect_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create (Admin : SupplierAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : ProxyPullConsumer.Ref;

   begin
      pragma Debug (O ("create proxy pull consumer"));

      Consumer         := new Object;
      Consumer.X       := new Proxy_Pull_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin;
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Consumer --
   ------------------------------

   procedure Disconnect_Pull_Consumer
     (Self : access Object)
   is
      Peer    : PullSupplier.Ref;
      Nil_Ref : PullSupplier.Ref;

   begin
      pragma Debug (O ("disconnect proxy pull consumer"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave_Critical_Section;

      if not PullSupplier.Is_Nil (Peer) then
         PullSupplier.Disconnect_Pull_Supplier (Peer);
      end if;
   end Disconnect_Pull_Consumer;

end CosEventChannelAdmin.ProxyPullConsumer.Impl;
