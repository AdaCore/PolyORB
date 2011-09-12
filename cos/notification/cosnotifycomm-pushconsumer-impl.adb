------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      C O S N O T I F Y C O M M . P U S H C O N S U M E R . I M P L       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2011, Free Software Foundation, Inc.          --
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
with CosEventComm.PushConsumer.Helper;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

with CosNotifyComm.PushConsumer.Skel;
pragma Warnings (Off, CosNotifyComm.PushConsumer.Skel);

package body CosNotifyComm.PushConsumer.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("pushconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Push_Consumer_Record is record
      This      : Object_Ptr;
      Empty     : Boolean;
      Event     : CORBA.Any;
      Peer      : CosNotifyChannelAdmin.ProxyPushSupplier.Ref;
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
      pragma Debug (O ("offer_change in pushconsumer"));
      null;
   end Offer_Change;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : CORBA.Any) is
   begin
      pragma Debug (O ("push new data to push consumer"));

      Enter (Self.X.M);
      Self.X.Empty := False;
      Self.X.Event := Data;
      Leave (Self.X.M);
      Signal (Self.X.CV);
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

      Enter (Self.X.M);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);

      Broadcast (Self.X.CV);

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
      Create (Consumer.X.M);
      Create (Consumer.X.CV);
      Initiate_Servant (PortableServer.Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   -------------------------------------
   -- Connect_Any_Proxy_Push_Supplier --
   -------------------------------------

   procedure Connect_Any_Proxy_Push_Supplier
      (Self  : access Object;
       Proxy : CosNotifyChannelAdmin.ProxyPushSupplier.Ref)
   is
      Cons_Ref : CosEventComm.PushConsumer.Ref;
      My_Ref   : PushConsumer.Ref;
   begin
      pragma Debug (O ("connect_any_proxy_push_supplier in pushconsumer"));

      Enter (Self.X.M);
      if not CosNotifyChannelAdmin.ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self.X.M);

      Cons_Ref := CosEventComm.PushConsumer.Helper.To_Ref (My_Ref);
      CosNotifyChannelAdmin.ProxyPushSupplier.connect_any_push_consumer
        (Proxy, Cons_Ref);
   end Connect_Any_Proxy_Push_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull (Self : access Object) return CORBA.Any is
      Event : CORBA.Any;
   begin
      pragma Debug (O ("attempt to pull new data from pushconsumer"));
      Enter (Self.X.M);

      loop
         if CosNotifyChannelAdmin.ProxyPushSupplier.Is_Nil (Self.X.Peer) then
            Leave (Self.X.M);
            CosEventComm.Helper.Raise_Disconnected
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         if not Self.X.Empty then
            Self.X.Empty := True;
            Event := Self.X.Event;
            exit;
         end if;
         Wait (Self.X.CV, Self.X.M);
      end loop;
      Leave (Self.X.M);
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

      Enter (Self.X.M);

      if CosNotifyChannelAdmin.ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Done := not Self.X.Empty;

      if Done then
         Self.X.Empty := True;
         Data := Self.X.Event;
      end if;

      Leave (Self.X.M);
   end Try_Pull;

end CosNotifyComm.PushConsumer.Impl;
