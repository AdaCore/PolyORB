------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S N O T I F Y C O M M . P U S H C O N S U M E R . I M P L      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosEventChannelAdmin;

with CosEventComm;
with CosEventComm.PushConsumer.Helper;

with CosNotifyChannelAdmin.ProxyPushSupplier;

with CosNotifyComm.PushConsumer.Helper;
pragma Elaborate (CosNotifyComm.PushConsumer.Helper);
pragma Warnings (Off, CosNotifyComm.PushConsumer.Helper);

with CosNotifyComm.PushConsumer.Skel;
pragma Elaborate (CosNotifyComm.PushConsumer.Skel);
pragma Warnings (Off, CosNotifyComm.PushConsumer.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Semaphores;
with PolyORB.Log;

package body CosNotifyComm.PushConsumer.Impl is

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Semaphores;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("pushconsumer");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Push_Consumer_Record is record
      This      : Object_Ptr;
      Empty     : Boolean;
      Event     : CORBA.Any;
      Peer      : CosNotifyChannelAdmin.ProxyPushSupplier.Ref;
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
      pragma Debug (O ("offer_change in pushconsumer"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Offer_Change;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in     CORBA.Any) is
   begin
      Ensure_Initialization;
      pragma Debug (O ("push new data to push consumer"));

      Enter (Self_Mutex);
      Self.X.Empty := False;
      Self.X.Event := Data;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);
   end Push;

   ------------------------------
   -- Disconnect_Push_Consumer --
   ------------------------------

   procedure Disconnect_Push_Consumer
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.ProxyPushSupplier.Ref;
      Nil_Ref : CosNotifyChannelAdmin.ProxyPushSupplier.Ref;
   begin
      pragma Debug (O ("disconnect push consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);

      if not CosNotifyChannelAdmin.ProxyPushSupplier.Is_Nil (Peer) then
         CosNotifyChannelAdmin.ProxyPushSupplier.disconnect_push_supplier
           (Peer);
      end if;
   end Disconnect_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : PushConsumer.Ref;
      Peer_Ref : CosNotifyChannelAdmin.ProxyPushSupplier.Ref;
   begin
      pragma Debug (O ("create pushconsumer"));

      Consumer         := new Object;
      Consumer.X       := new Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Empty := True;
      Consumer.X.Peer  := Peer_Ref;
      Create (Consumer.X.Semaphore);
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   -------------------------------------
   -- Connect_Any_Proxy_Push_Supplier --
   -------------------------------------

   procedure Connect_Any_Proxy_Push_Supplier
      (Self  : access Object;
       Proxy : in     CosNotifyChannelAdmin.ProxyPushSupplier.Ref)
   is
      Cons_Ref : CosEventComm.PushConsumer.Ref;
      My_Ref   : PushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("connect_any_proxy_push_supplier in pushconsumer"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise CosEventChannelAdmin.AlreadyConnected;
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

      Cons_Ref := CosEventComm.PushConsumer.Helper.To_Ref (My_Ref);
      CosNotifyChannelAdmin.ProxyPushSupplier.connect_any_push_consumer
        (Proxy, Cons_Ref);
   end Connect_Any_Proxy_Push_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull
    (Self  : access Object)
    return CORBA.Any
   is
      Event : CORBA.Any;
   begin
      Ensure_Initialization;

      loop
         pragma Debug (O ("attempt to pull new data from pushconsumer"));

         P (Self.X.Semaphore);
         Enter (Self_Mutex);

         if CosNotifyChannelAdmin.ProxyPushSupplier.Is_Nil (Self.X.Peer) then
            Leave (Self_Mutex);
            raise CosEventComm.Disconnected;
         end if;

         if not Self.X.Empty then
            Self.X.Empty := True;
            Event := Self.X.Event;
            Leave (Self_Mutex);
            exit;
         end if;

         Leave (Self_Mutex);
      end loop;
      pragma Debug (O ("succeded to pull new data from pushconsumer"));

      return Event;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self : access Object;
      Done : out    CORBA.Boolean;
      Data : out    CORBA.Any) is
   begin
      pragma Debug (O ("try to pull new data from push consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);

      if CosNotifyChannelAdmin.ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise CosEventComm.Disconnected;
      end if;

      Done := not Self.X.Empty;

      if Done then
         Self.X.Empty := True;
         Data := Self.X.Event;
      end if;

      Leave (Self_Mutex);
   end Try_Pull;

end CosNotifyComm.PushConsumer.Impl;
