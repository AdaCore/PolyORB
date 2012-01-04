------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCOMM.SEQUENCEPULLCONSUMER.IMPL                  --
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

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosNotifyComm.SequencePullConsumer.Skel;
pragma Warnings (Off, CosNotifyComm.SequencePullConsumer.Skel);

package body CosNotifyComm.SequencePullConsumer.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("sequencepullconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Sequence_Pull_Consumer_Record is record
      This : Object_Ptr;
      Peer : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref;
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

   ---------------------------------------
   -- Disconnect_Sequence_Pull_Consumer --
   ---------------------------------------

   procedure Disconnect_Sequence_Pull_Consumer
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref;
      Nil_Ref : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("disconnect sequencepullconsumer"));

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not CosNotifyChannelAdmin.SequenceProxyPullSupplier.Is_Nil
      (Peer) then
         CosNotifyChannelAdmin.SequenceProxyPullSupplier.
         disconnect_sequence_pull_supplier (Peer);
      end if;
   end Disconnect_Sequence_Pull_Consumer;

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
      Ensure_Initialization;
      pragma Debug (O ("offer_change in sequencepullconsumer"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Offer_Change;

   ------------------------------------------
   -- Connect_Sequence_Proxy_Pull_Supplier --
   ------------------------------------------

   procedure Connect_Sequence_Proxy_Pull_Supplier
      (Self  : access Object;
       Proxy : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref)
   is
      My_Ref   : SequencePullConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("connect_sequence_proxy_pull_supplier in sequencepullconsumer"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.SequenceProxyPullSupplier.Is_Nil
      (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

      CosNotifyChannelAdmin.SequenceProxyPullSupplier.
      connect_sequence_pull_consumer (Proxy, My_Ref);
   end Connect_Sequence_Proxy_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : SequencePullConsumer.Ref;
      Peer_Ref : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref;
   begin
      pragma Debug (O ("create sequencepullconsumer"));

      Consumer        := new Object;
      Consumer.X      := new Sequence_Pull_Consumer_Record;
      Consumer.X.This := Consumer;
      Consumer.X.Peer := Peer_Ref;
      Initiate_Servant (PortableServer.Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self       : access Object;
      Max_Number : CORBA.Long)
     return CosNotification.EventBatch
   is
      Peer : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("pull sequence of structured events " &
                       "from sequencepullconsumer"));

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.SequenceProxyPullSupplier.Is_Nil (Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      return CosNotifyChannelAdmin.SequenceProxyPullSupplier.
             pull_structured_events (Peer, Max_Number);
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self       : access Object;
      Max_Number : CORBA.Long;
      Done       : out    CORBA.Boolean;
      Returns    : out    CosNotification.EventBatch)
   is
      Peer : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref;
   begin
      pragma Debug
      (O ("try to pull sequence of structured events " &
          "from sequencepullconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.SequenceProxyPullSupplier.Is_Nil (Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      CosNotifyChannelAdmin.SequenceProxyPullSupplier.
      try_pull_structured_events (Peer, Max_Number, Done, Returns);
   end Try_Pull;

end CosNotifyComm.SequencePullConsumer.Impl;
