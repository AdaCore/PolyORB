------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      C O S N O T I F Y C O M M . P U S H S U P P L I E R . I M P L       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosEventChannelAdmin.Helper;

with CosEventComm.Helper;
with CosEventComm.PushSupplier.Helper;

with CosNotifyChannelAdmin.ProxyPushConsumer;

with CosNotifyComm.PushSupplier.Helper;
pragma Elaborate (CosNotifyComm.PushSupplier.Helper);
pragma Warnings (Off, CosNotifyComm.PushSupplier.Helper);

with CosNotifyComm.PushSupplier.Skel;
pragma Elaborate (CosNotifyComm.PushSupplier.Skel);
pragma Warnings (Off, CosNotifyComm.PushSupplier.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
--  with PolyORB.Tasking.Semaphores;
with PolyORB.Log;

package body CosNotifyComm.PushSupplier.Impl is

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   --  use PolyORB.Tasking.Semaphores;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("pushsupplier");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

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
      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   -------------------------------------
   -- Connect_Any_Proxy_Push_Consumer --
   -------------------------------------

   procedure Connect_Any_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : in     CosNotifyChannelAdmin.ProxyPushConsumer.Ref)
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

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
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
      Data : in CORBA.Any)
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
