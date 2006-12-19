------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCOMM.STRUCTUREDPUSHCONSUMER.IMPL                 --
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

with CosNotifyComm.StructuredPushConsumer.Skel;
pragma Warnings (Off, CosNotifyComm.StructuredPushConsumer.Skel);

package body CosNotifyComm.StructuredPushConsumer.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Semaphores;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("structuredpushconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Structured_Push_Consumer_Record is record
      This      : Object_Ptr;
      Empty     : Boolean;
      Event     : CosNotification.StructuredEvent;
      Peer      : CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref;
      Semaphore : Semaphore_Access;
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
      pragma Debug (O ("offer_change in structuredpushconsumer"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Offer_Change;

   ---------------------------
   -- Push_Structured_Event --
   ---------------------------

   procedure Push_Structured_Event
     (Self         : access Object;
      Notification : CosNotification.StructuredEvent)
   is
   begin
      Ensure_Initialization;
      pragma Debug (O ("push structuredevent to structuredpushconsumer"));

      Enter (Self_Mutex);
      Self.X.Empty := False;
      Self.X.Event := Notification;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);
   end Push_Structured_Event;

   -----------------------------------------
   -- Disconnect_Structured_Push_Consumer --
   -----------------------------------------

   procedure Disconnect_Structured_Push_Consumer
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref;
      Nil_Ref : CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("disconnect structuredpushconsumer"));

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);

      if not CosNotifyChannelAdmin.StructuredProxyPushSupplier.Is_Nil
             (Peer) then
         CosNotifyChannelAdmin.StructuredProxyPushSupplier.
            disconnect_structured_push_supplier (Peer);
      end if;
   end Disconnect_Structured_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : StructuredPushConsumer.Ref;
      Peer_Ref : CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref;
   begin
      pragma Debug (O ("create structuredpushconsumer"));

      Consumer         := new Object;
      Consumer.X       := new Structured_Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Empty := True;
      Consumer.X.Peer  := Peer_Ref;
      Create (Consumer.X.Semaphore);
      Initiate_Servant (PortableServer.Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   --------------------------------------------
   -- Connect_Structured_Proxy_Push_Supplier --
   --------------------------------------------

   procedure Connect_Structured_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref)
   is
      My_Ref   : StructuredPushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("connect_structured_proxy_push_supplier in structuredpushconsumer"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.StructuredProxyPushSupplier.Is_Nil
      (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

      CosNotifyChannelAdmin.StructuredProxyPushSupplier.
         connect_structured_push_consumer (Proxy, My_Ref);
   end Connect_Structured_Proxy_Push_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull
    (Self  : access Object)
    return CosNotification.StructuredEvent
   is
      Notification : CosNotification.StructuredEvent;
   begin
      Ensure_Initialization;

      loop
         pragma Debug
         (O ("attempt to pull structured event from structuredpushconsumer"));

         P (Self.X.Semaphore);
         Enter (Self_Mutex);

         if CosNotifyChannelAdmin.StructuredProxyPushSupplier.Is_Nil
         (Self.X.Peer) then
            Leave (Self_Mutex);
            CosEventComm.Helper.Raise_Disconnected
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         if not Self.X.Empty then
            Self.X.Empty := True;
            Notification := Self.X.Event;
            Leave (Self_Mutex);
            exit;
         end if;

         Leave (Self_Mutex);
      end loop;

      pragma Debug
      (O ("succeded to pull structured event from structuredpushconsumer"));

      return Notification;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self : access Object;
      Done : out    CORBA.Boolean;
      Data : out    CosNotification.StructuredEvent) is
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("try to pull structured event from structuredpushconsumer"));

      Enter (Self_Mutex);

      if CosNotifyChannelAdmin.StructuredProxyPushSupplier.Is_Nil
      (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Done := not Self.X.Empty;

      if Done then
         Self.X.Empty := True;
         Data := Self.X.Event;
      end if;

      Leave (Self_Mutex);
   end Try_Pull;

end CosNotifyComm.StructuredPushConsumer.Impl;
