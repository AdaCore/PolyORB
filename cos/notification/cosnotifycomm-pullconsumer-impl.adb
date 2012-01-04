------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      C O S N O T I F Y C O M M . P U L L C O N S U M E R . I M P L       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CosEventChannelAdmin.Helper;
with CosEventComm.Helper;
with CosEventComm.PullConsumer.Helper;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosNotifyComm.PullConsumer.Skel;
pragma Warnings (Off, CosNotifyComm.PullConsumer.Skel);

package body CosNotifyComm.PullConsumer.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("pullconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Pull_Consumer_Record is record
      This : Object_Ptr;
      Peer : CosNotifyChannelAdmin.ProxyPullSupplier.Ref;
   end record;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

   T_Initialized : Boolean := False;
   Self_Mutex : Mutex_Access;

   procedure Ensure_Initialization is
   begin
      if not T_Initialized then
         Create (Self_Mutex);
         T_Initialized := True;
      end if;
   end Ensure_Initialization;

   ------------------------------
   -- Disconnect_Pull_Consumer --
   ------------------------------

   procedure Disconnect_Pull_Consumer
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.ProxyPullSupplier.Ref;
      Nil_Ref : CosNotifyChannelAdmin.ProxyPullSupplier.Ref;
   begin
      pragma Debug (O ("disconnect pull consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not CosNotifyChannelAdmin.ProxyPullSupplier.Is_Nil (Peer) then
         CosNotifyChannelAdmin.ProxyPullSupplier.disconnect_pull_supplier
         (Peer);
      end if;
   end Disconnect_Pull_Consumer;

   ------------------
   -- Offer_Change --
   ------------------

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Added, Removed);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("offer_change in pullconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Offer_Change;

   -------------------------------------
   -- Connect_Any_Proxy_Pull_Supplier --
   -------------------------------------

   procedure Connect_Any_Proxy_Pull_Supplier
      (Self  : access Object;
       Proxy : CosNotifyChannelAdmin.ProxyPullSupplier.Ref)
   is
      Cons_Ref : CosEventComm.PullConsumer.Ref;
      My_Ref   : PullConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("connect_any_proxy_pull_supplier in pullconsumer"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.ProxyPullSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

      Cons_Ref := CosEventComm.PullConsumer.Helper.To_Ref (My_Ref);
      CosNotifyChannelAdmin.ProxyPullSupplier.connect_any_pull_consumer
        (Proxy, Cons_Ref);
   end Connect_Any_Proxy_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : PullConsumer.Ref;
      Peer_Ref : CosNotifyChannelAdmin.ProxyPullSupplier.Ref;
   begin
      pragma Debug (O ("create pullconsumer"));

      Consumer        := new Object;
      Consumer.X      := new Pull_Consumer_Record;
      Consumer.X.This := Consumer;
      Consumer.X.Peer := Peer_Ref;
      Initiate_Servant (PortableServer.Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Peer : CosNotifyChannelAdmin.ProxyPullSupplier.Ref;
   begin
      pragma Debug (O ("pull new data from pull consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.ProxyPullSupplier.Is_Nil (Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      return CosNotifyChannelAdmin.ProxyPullSupplier.pull (Peer);
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self    : access Object;
      Done    : out    CORBA.Boolean;
      Returns : out    CORBA.Any)
   is
      Peer : CosNotifyChannelAdmin.ProxyPullSupplier.Ref;
   begin
      pragma Debug (O ("try to pull new data from pull consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.ProxyPullSupplier.Is_Nil (Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      CosNotifyChannelAdmin.ProxyPullSupplier.try_pull (Peer, Done, Returns);
   end Try_Pull;

end CosNotifyComm.PullConsumer.Impl;
