------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCOMM.SEQUENCEPULLSUPPLIER.IMPL                  --
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
with PolyORB.Tasking.Condition_Variables;

with CosNotifyComm.SequencePullSupplier.Skel;
pragma Warnings (Off, CosNotifyComm.SequencePullSupplier.Skel);

package body CosNotifyComm.SequencePullSupplier.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("sequencepullsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Sequence_Pull_Supplier_Record is record
      This       : Object_Ptr;
      Peer       : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref;
      Empty      : Boolean;
      Events     : CosNotification.EventBatch;
      M          : Mutex_Access;
      CV         : Condition_Access;
   end record;

   ------------------------------------------
   -- Connect_Sequence_Proxy_Pull_Consumer --
   ------------------------------------------

   procedure Connect_Sequence_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref)
   is
      My_Ref  : SequencePullSupplier.Ref;
   begin
      pragma Debug
      (O ("connect_sequence_proxy_pull_consumer in sequencepullsupplier"));

      Enter (Self.X.M);
      if not CosNotifyChannelAdmin.SequenceProxyPullConsumer.Is_Nil
      (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;
      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self.X.M);

      CosNotifyChannelAdmin.SequenceProxyPullConsumer.
      connect_sequence_pull_supplier (Proxy, My_Ref);

   end Connect_Sequence_Proxy_Pull_Consumer;

   -------------------------
   -- Subscription_Change --
   -------------------------

   procedure Subscription_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Added, Removed);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("subscription_change in sequencepullsupplier"));
      null;
   end Subscription_Change;

   ---------------------------------------
   -- Disconnect_Sequence_Pull_Supplier --
   ---------------------------------------

   procedure Disconnect_Sequence_Pull_Supplier
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref;
      Nil_Ref : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref;
   begin
      pragma Debug (O ("disconnect sequencepullsupplier"));

      Enter (Self.X.M);
      Peer := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);
      Broadcast (Self.X.CV);

      if not CosNotifyChannelAdmin.SequenceProxyPullConsumer.Is_Nil (Peer) then
         CosNotifyChannelAdmin.SequenceProxyPullConsumer.
         disconnect_sequence_pull_consumer (Peer);
      end if;
   end Disconnect_Sequence_Pull_Supplier;

   ----------------------------
   -- Pull_Structured_Events --
   ----------------------------

   function Pull_Structured_Events
     (Self       : access Object;
      Max_Number : CORBA.Long)
     return CosNotification.EventBatch
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Max_Number);
      pragma Warnings (On);  --  WAG:3.14
      Events : CosNotification.EventBatch;
   begin
      pragma Debug
        (O ("attempt to pull new sequence of structured events " &
            "from pull supplier"));

      Enter (Self.X.M);

      loop
         if CosNotifyChannelAdmin.SequenceProxyPullConsumer.Is_Nil
              (Self.X.Peer)
         then
            Leave (Self.X.M);
            CosEventComm.Helper.Raise_Disconnected
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         if not Self.X.Empty then
            Events := Self.X.Events;
            Self.X.Empty := True;
            exit;
         end if;
         Wait (Self.X.CV, Self.X.M);
      end loop;
      Leave (Self.X.M);

      pragma Debug
        (O ("succeed to pull new sequence of structured events " &
            "from pull supplier"));

      return Events;
   end Pull_Structured_Events;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : CosNotification.EventBatch) is
   begin
      pragma Debug (O ("push new sequence of structured events " &
                       "to sequencepullsupplier"));

      Enter (Self.X.M);
      Self.X.Empty  := False;
      Self.X.Events := Data;
      Leave (Self.X.M);
      Signal (Self.X.CV);
   end Push;

   --------------------------------
   -- Try_Pull_Structured_Events --
   --------------------------------

   procedure Try_Pull_Structured_Events
     (Self       : access Object;
      Max_Number : CORBA.Long;
      Has_Event  : out CORBA.Boolean;
      Returns    : out CosNotification.EventBatch)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Max_Number);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug
      (O ("try to pull new sequence of structured events " &
          "from sequencepullsupplier"));

      Enter (Self.X.M);
      if CosNotifyChannelAdmin.SequenceProxyPullConsumer.Is_Nil
      (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Has_Event := not Self.X.Empty;

      if Has_Event then
         Returns := Self.X.Events;
         Self.X.Empty := True;
      end if;

      Leave (Self.X.M);
   end Try_Pull_Structured_Events;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : SequencePullSupplier.Ref;
      Peer_Ref : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref;
   begin
      pragma Debug (O ("create sequencepullsupplier"));

      Supplier         := new Object;
      Supplier.X       := new Sequence_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Empty := True;
      Supplier.X.Peer  := Peer_Ref;
      Create (Supplier.X.M);
      Create (Supplier.X.CV);

      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

end CosNotifyComm.SequencePullSupplier.Impl;
