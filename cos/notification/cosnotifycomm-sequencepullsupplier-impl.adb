------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCOMM.SEQUENCEPULLSUPPLIER.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
with PolyORB.Tasking.Semaphores;

with CosNotifyComm.SequencePullSupplier.Skel;
pragma Warnings (Off, CosNotifyComm.SequencePullSupplier.Skel);

package body CosNotifyComm.SequencePullSupplier.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Semaphores;

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
      Semaphore  : Semaphore_Access;
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

   ------------------------------------------
   -- Connect_Sequence_Proxy_Pull_Consumer --
   ------------------------------------------

   procedure Connect_Sequence_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref)
   is
      My_Ref  : SequencePullSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("connect_sequence_proxy_pull_consumer in sequencepullsupplier"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.SequenceProxyPullConsumer.Is_Nil
      (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;
      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

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
      Ensure_Initialization;
      pragma Debug (O ("subscription_change in sequencepullsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

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
      Ensure_Initialization;

      pragma Debug (O ("disconnect sequencepullsupplier"));

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);

      if not CosNotifyChannelAdmin.SequenceProxyPullConsumer.Is_Nil
      (Peer) then
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

      Ensure_Initialization;

      loop
         pragma Debug
         (O ("attempt to pull new sequence of structured events " &
             "from pull supplier"));
         P (Self.X.Semaphore);

         Enter (Self_Mutex);
         if CosNotifyChannelAdmin.SequenceProxyPullConsumer.Is_Nil
         (Self.X.Peer) then
            Leave (Self_Mutex);
            CosEventComm.Helper.Raise_Disconnected
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         if not Self.X.Empty then
            Events := Self.X.Events;
            Self.X.Empty := True;
            Leave (Self_Mutex);
            exit;
         end if;

         Leave (Self_Mutex);
      end loop;

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Self.X.Empty  := False;
      Self.X.Events := Data;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);
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
      Ensure_Initialization;
      pragma Debug
      (O ("try to pull new sequence of structured events " &
          "from sequencepullsupplier"));

      Enter (Self_Mutex);
      if CosNotifyChannelAdmin.SequenceProxyPullConsumer.Is_Nil
      (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Has_Event := not Self.X.Empty;

      if Has_Event then
         Returns := Self.X.Events;
         Self.X.Empty := True;
      end if;

      Leave (Self_Mutex);
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
      Create (Supplier.X.Semaphore);

      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

end CosNotifyComm.SequencePullSupplier.Impl;
