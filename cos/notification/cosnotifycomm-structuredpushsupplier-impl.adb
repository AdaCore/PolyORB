------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCOMM.STRUCTUREDPUSHSUPPLIER.IMPL                 --
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

with CosNotifyComm.StructuredPushSupplier.Skel;
pragma Warnings (Off, CosNotifyComm.StructuredPushSupplier.Skel);

package body CosNotifyComm.StructuredPushSupplier.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("structuredpushsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Structured_Push_Supplier_Record is record
      This : Object_Ptr;
      Peer : CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref;
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
      pragma Debug (O ("subscription_change in structuredpushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Subscription_Change;

   -----------------------------------------
   -- Disconnect_Structured_Push_Supplier --
   -----------------------------------------

   procedure Disconnect_Structured_Push_Supplier
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref;
      Nil_Ref : CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("disconnect structuredpushsupplier"));

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not CosNotifyChannelAdmin.StructuredProxyPushConsumer.Is_Nil
      (Peer) then
         CosNotifyChannelAdmin.StructuredProxyPushConsumer.
         disconnect_structured_push_consumer (Peer);
      end if;
   end Disconnect_Structured_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : StructuredPushSupplier.Ref;
      My_Peer  : CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref;
   begin
      pragma Debug (O ("create structuredpushsupplier"));

      Supplier        := new Object;
      Supplier.X      := new Structured_Push_Supplier_Record;
      Supplier.X.This := Supplier;
      Supplier.X.Peer := My_Peer;
      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   --------------------------------------------
   -- Connect_Structured_Proxy_Push_Consumer --
   --------------------------------------------

   procedure Connect_Structured_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref)
   is
      My_Ref  : StructuredPushSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("connect_structured_proxy_push_consumer in structuredpushsupplier"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.StructuredProxyPushConsumer.Is_Nil
      (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

      CosNotifyChannelAdmin.StructuredProxyPushConsumer.
         connect_structured_push_supplier (Proxy, My_Ref);

   end Connect_Structured_Proxy_Push_Consumer;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self         : access Object;
      Notification : CosNotification.StructuredEvent)
   is
      Peer : CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("push new structuredevent to structuredpushsupplier"));

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.StructuredProxyPushConsumer.Is_Nil (Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      CosNotifyChannelAdmin.StructuredProxyPushConsumer.push_structured_event
        (Peer, Notification);
   end Push;

end CosNotifyComm.StructuredPushSupplier.Impl;
