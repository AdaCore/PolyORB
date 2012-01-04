------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      C O S N O T I F Y C O M M . P U S H S U P P L I E R . I M P L       --
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
with CosEventComm.PushSupplier.Helper;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosNotifyComm.PushSupplier.Skel;
pragma Warnings (Off, CosNotifyComm.PushSupplier.Skel);

package body CosNotifyComm.PushSupplier.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("pushsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Push_Supplier_Record is record
      This : Object_Ptr;
      Peer : CosNotifyChannelAdmin.ProxyPushConsumer.Ref;
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
      pragma Debug (O ("subscription_change in pushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Subscription_Change;

   ------------------------------
   -- Disconnect_Push_Supplier --
   ------------------------------

   procedure Disconnect_Push_Supplier
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.ProxyPushConsumer.Ref;
      Nil_Ref : CosNotifyChannelAdmin.ProxyPushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("disconnect push supplier"));

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not CosNotifyChannelAdmin.ProxyPushConsumer.Is_Nil (Peer) then
         CosNotifyChannelAdmin.ProxyPushConsumer.disconnect_push_consumer
           (Peer);
      end if;
   end Disconnect_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : PushSupplier.Ref;
      My_Peer  : CosNotifyChannelAdmin.ProxyPushConsumer.Ref;
   begin
      pragma Debug (O ("create pushsupplier"));

      Supplier        := new Object;
      Supplier.X      := new Push_Supplier_Record;
      Supplier.X.This := Supplier;
      Supplier.X.Peer := My_Peer;
      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   -------------------------------------
   -- Connect_Any_Proxy_Push_Consumer --
   -------------------------------------

   procedure Connect_Any_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.ProxyPushConsumer.Ref)
   is
      My_Ref  : PushSupplier.Ref;
      Sup_Ref : CosEventComm.PushSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("connect_any_proxy_push_consumer in pushsupplier"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.ProxyPushConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;
      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

      Sup_Ref := CosEventComm.PushSupplier.Helper.To_Ref (My_Ref);
      CosNotifyChannelAdmin.ProxyPushConsumer.connect_any_push_supplier
         (Proxy, Sup_Ref);

   end Connect_Any_Proxy_Push_Consumer;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : CORBA.Any)
   is
      My_Peer : CosNotifyChannelAdmin.ProxyPushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("push new data to pushsupplier"));

      Enter (Self_Mutex);
      My_Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.ProxyPushConsumer.Is_Nil (My_Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      CosNotifyChannelAdmin.ProxyPushConsumer.push (My_Peer, Data);
   end Push;

end CosNotifyComm.PushSupplier.Impl;
