----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosEventComm; use CosEventComm;

with CosEventComm.PullConsumer;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPullSupplier.Helper;
with CosEventChannelAdmin.ProxyPullSupplier.Skel;

with CosEventChannelAdmin.ConsumerAdmin.Impl;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

with PortableServer; use PortableServer;

with CORBA.Object;

package body CosEventChannelAdmin.ProxyPullSupplier.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("proxypullsupplier");
   procedure O is new Broca.Debug.Output (Flag);

   type Proxy_Pull_Supplier_Record is
      record
         This    : Object_Ptr;
         Peer    : PullConsumer.Ref;
         Admin   : ConsumerAdmin.Impl.Object_Ptr;
         Event   : CORBA.Any;
         Empty   : Boolean;
         Watcher : Watcher_Access;
      end record;

   ---------------------------
   -- Connect_Pull_Consumer --
   ---------------------------

   procedure Connect_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : in PullConsumer.Ref)
   is
   begin
      pragma Debug (O ("connect pull consumer to proxy pull supplier"));

      Enter_Critical_Section;
      if not PullConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Pull_Consumer;
      Leave_Critical_Section;
   end Connect_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create (Admin : ConsumerAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("create proxy pull supplier"));

      Supplier         := new Object;
      Supplier.X       := new Proxy_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;
      Supplier.X.Empty := True;
      Create (Supplier.X.Watcher);
      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier
     (Self : access Object)
   is
      Peer    : PullConsumer.Ref;
      Nil_Ref : PullConsumer.Ref;

   begin
      pragma Debug (O ("disconnect proxy pull supplier"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Update (Self.X.Watcher);
      Leave_Critical_Section;

      if not PullConsumer.Is_Nil (Peer) then
         PullConsumer.Disconnect_Pull_Consumer (Peer);
      end if;
   end Disconnect_Pull_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      pragma Debug (O ("post new data to proxy pull supplier"));

      Enter_Critical_Section;
      Self.X.Event := Data;
      Self.X.Empty := False;
      Update (Self.X.Watcher);
      Leave_Critical_Section;
   end Post;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Version : Version_Id;
      Event   : CORBA.Any;

   begin
      loop
         pragma Debug
           (O ("attempt to pull new data from proxy pull supplier"));

         Enter_Critical_Section;
         if PullConsumer.Is_Nil (Self.X.Peer) then
            Leave_Critical_Section;
            raise Disconnected;
         end if;

         if not Self.X.Empty then
            Event := Self.X.Event;
            Self.X.Empty := True;
            Leave_Critical_Section;
            exit;
         end if;
         Lookup (Self.X.Watcher, Version);
         Leave_Critical_Section;
         Differ (Self.X.Watcher, Version);
      end loop;
      pragma Debug (O ("succeed to pull new data from proxy pull supplier"));

      return Event;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out CORBA.Boolean;
      Returns   : out CORBA.Any) is
   begin
      pragma Debug (O ("try to pull new data from proxy pull supplier"));
      Enter_Critical_Section;
      if PullConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise Disconnected;
      end if;

      if Self.X.Empty then
         Has_Event := False;

      else
         Has_Event    := True;
         Returns      := Self.X.Event;
         Self.X.Empty := True;
      end if;
      Leave_Critical_Section;
   end Try_Pull;

end CosEventChannelAdmin.ProxyPullSupplier.Impl;
