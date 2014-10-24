------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCOMM.SEQUENCEPUSHCONSUMER.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2014, Free Software Foundation, Inc.          --
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
with PolyORB.Tasking.Condition_Variables;

with CosNotifyComm.SequencePushConsumer.Skel;
pragma Warnings (Off, CosNotifyComm.SequencePushConsumer.Skel);

package body CosNotifyComm.SequencePushConsumer.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("sequencepushconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Sequence_Push_Consumer_Record is record
      This      : Object_Ptr;
      Empty     : Boolean;
      Events    : CosNotification.EventBatch;
      Peer      : CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref;
      M         : Mutex_Access;
      CV        : Condition_Access;
   end record;

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
      pragma Debug (O ("offer_change in sequencepushconsumer"));
      null;
   end Offer_Change;

   ----------------------------
   -- Push_Structured_Events --
   ----------------------------

   procedure Push_Structured_Events
     (Self          : access Object;
      Notifications : CosNotification.EventBatch)
   is
   begin
      pragma Debug (O ("push sequence of structured events " &
                       "to sequencepushconsumer"));

      Enter (Self.X.M);
      Self.X.Empty  := False;
      Self.X.Events := Notifications;
      Leave (Self.X.M);
      Signal (Self.X.CV);
   end Push_Structured_Events;

   ---------------------- ----------------
   -- Disconnect_Sequence_Push_Consumer --
   ---------------------------------------

   procedure Disconnect_Sequence_Push_Consumer
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref;
      Nil_Ref : CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref;
   begin
      pragma Debug (O ("disconnect sequencepushconsumer"));

      Enter (Self.X.M);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);
      Broadcast (Self.X.CV);

      if not CosNotifyChannelAdmin.SequenceProxyPushSupplier.Is_Nil (Peer) then
         CosNotifyChannelAdmin.SequenceProxyPushSupplier.
            disconnect_sequence_push_supplier (Peer);
      end if;
   end Disconnect_Sequence_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : SequencePushConsumer.Ref;
      Peer_Ref : CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref;
   begin
      pragma Debug (O ("create sequencepushconsumer"));

      Consumer         := new Object;
      Consumer.X       := new Sequence_Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Empty := True;
      Consumer.X.Peer  := Peer_Ref;
      Create (Consumer.X.M);
      Create (Consumer.X.CV);
      Initiate_Servant (PortableServer.Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------------------
   -- Connect_Sequence_Proxy_Push_Supplier --
   ------------------------------------------

   procedure Connect_Sequence_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref)
   is
      My_Ref   : SequencePushConsumer.Ref;
   begin
      pragma Debug
      (O ("connect_sequence_proxy_push_supplier in sequencepushconsumer"));

      Enter (Self.X.M);
      if not CosNotifyChannelAdmin.SequenceProxyPushSupplier.Is_Nil
        (Self.X.Peer)
      then
         Leave (Self.X.M);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self.X.M);

      CosNotifyChannelAdmin.SequenceProxyPushSupplier.
         connect_sequence_push_consumer (Proxy, My_Ref);
   end Connect_Sequence_Proxy_Push_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull
    (Self  : access Object)
    return CosNotification.EventBatch
   is
      Notifications : CosNotification.EventBatch;
   begin
      pragma Debug
        (O ("attempt to pull sequence of structured events " &
            "from sequencepushconsumer"));

      Enter (Self.X.M);

      loop
         if CosNotifyChannelAdmin.SequenceProxyPushSupplier.Is_Nil
           (Self.X.Peer)
         then
            Leave (Self.X.M);
            CosEventComm.Helper.Raise_Disconnected
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         if not Self.X.Empty then
            Self.X.Empty := True;
            Notifications := Self.X.Events;
            exit;
         end if;
         Wait (Self.X.CV, Self.X.M);
      end loop;

      Leave (Self.X.M);

      pragma Debug
      (O ("succeded to pull sequence of structured events " &
          "from sequencepushconsumer"));

      return Notifications;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self : access Object;
      Done : out    CORBA.Boolean;
      Data : out    CosNotification.EventBatch) is
   begin
      pragma Debug
      (O ("try to pull sequence of structured events " &
          "from sequencepushconsumer"));

      Enter (Self.X.M);

      if CosNotifyChannelAdmin.SequenceProxyPushSupplier.Is_Nil
        (Self.X.Peer)
      then
         Leave (Self.X.M);
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Done := not Self.X.Empty;

      if Done then
         Self.X.Empty := True;
         Data := Self.X.Events;
      end if;

      Leave (Self.X.M);
   end Try_Pull;

end CosNotifyComm.SequencePushConsumer.Impl;
