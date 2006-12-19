------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCOMM.SEQUENCEPUSHSUPPLIER.IMPL                  --
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

with CosNotifyComm.SequencePushSupplier.Skel;
pragma Warnings (Off, CosNotifyComm.SequencePushSupplier.Skel);

package body CosNotifyComm.SequencePushSupplier.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("sequencepushsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Sequence_Push_Supplier_Record is record
      This : Object_Ptr;
      Peer : CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref;
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
      pragma Debug (O ("subscription_change in sequencepushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Subscription_Change;

   ---------------------------------------
   -- Disconnect_Sequence_Push_Supplier --
   ---------------------------------------

   procedure Disconnect_Sequence_Push_Supplier
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref;
      Nil_Ref : CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("disconnect sequencepushsupplier"));

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not CosNotifyChannelAdmin.SequenceProxyPushConsumer.Is_Nil
      (Peer) then
         CosNotifyChannelAdmin.SequenceProxyPushConsumer.
         disconnect_sequence_push_consumer (Peer);
      end if;
   end Disconnect_Sequence_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : SequencePushSupplier.Ref;
      My_Peer  : CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref;
   begin
      pragma Debug (O ("create sequencepushsupplier"));

      Supplier        := new Object;
      Supplier.X      := new Sequence_Push_Supplier_Record;
      Supplier.X.This := Supplier;
      Supplier.X.Peer := My_Peer;
      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------------------
   -- Connect_Sequence_Proxy_Push_Consumer --
   ------------------------------------------

   procedure Connect_Sequence_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref)
   is
      My_Ref  : SequencePushSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("connect_sequence_proxy_push_consumer in sequencepushsupplier"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.SequenceProxyPushConsumer.Is_Nil
      (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

      CosNotifyChannelAdmin.SequenceProxyPushConsumer.
         connect_sequence_push_supplier (Proxy, My_Ref);

   end Connect_Sequence_Proxy_Push_Consumer;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self          : access Object;
      Notifications : CosNotification.EventBatch)
   is
      Peer : CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("push sequence of structuredevents " &
                       "to sequencepushsupplier"));

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.SequenceProxyPushConsumer.Is_Nil (Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      CosNotifyChannelAdmin.SequenceProxyPushConsumer.push_structured_events
        (Peer, Notifications);
   end Push;

end CosNotifyComm.SequencePushSupplier.Impl;
