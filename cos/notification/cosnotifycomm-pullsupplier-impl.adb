------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      C O S N O T I F Y C O M M . P U L L S U P P L I E R . I M P L       --
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
with CosEventComm.PullSupplier.Helper;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

with CosNotifyComm.PullSupplier.Skel;
pragma Warnings (Off, CosNotifyComm.PullSupplier.Skel);

package body CosNotifyComm.PullSupplier.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("pullsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Pull_Supplier_Record is record
      This      : Object_Ptr;
      Peer      : CosNotifyChannelAdmin.ProxyPullConsumer.Ref;
      Empty     : Boolean;
      Event     : CORBA.Any;
      M         : Mutex_Access;
      CV        : Condition_Access;
   end record;

   -------------------------------------
   -- Connect_Any_Proxy_Pull_Consumer --
   -------------------------------------

   procedure Connect_Any_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.ProxyPullConsumer.Ref)
   is
      My_Ref  : PullSupplier.Ref;
      Sup_Ref : CosEventComm.PullSupplier.Ref;
   begin
      pragma Debug (O ("connect_any_proxy_pull_consumer in pullsupplier"));

      Enter (Self.X.M);
      if not CosNotifyChannelAdmin.ProxyPullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;
      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self.X.M);

      Sup_Ref := CosEventComm.PullSupplier.Helper.To_Ref (My_Ref);
      CosNotifyChannelAdmin.ProxyPullConsumer.connect_any_pull_supplier
         (Proxy, Sup_Ref);

   end Connect_Any_Proxy_Pull_Consumer;

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
      pragma Debug (O ("subscription_change in pullsupplier"));
      null;
   end Subscription_Change;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.ProxyPullConsumer.Ref;
      Nil_Ref : CosNotifyChannelAdmin.ProxyPullConsumer.Ref;
   begin
      pragma Debug (O ("disconnect pull supplier"));

      Enter (Self.X.M);
      Peer := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);
      Broadcast (Self.X.CV);

      if not CosNotifyChannelAdmin.ProxyPullConsumer.Is_Nil (Peer) then
         CosNotifyChannelAdmin.ProxyPullConsumer.disconnect_pull_consumer
         (Peer);
      end if;
   end Disconnect_Pull_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull (Self : access Object) return CORBA.Any is
      Event : CORBA.Any;
   begin
      Enter (Self.X.M);
      loop
         pragma Debug (O ("attempt to pull new data from pull supplier"));
         if CosNotifyChannelAdmin.ProxyPullConsumer.Is_Nil (Self.X.Peer) then
            Leave (Self.X.M);
            CosEventComm.Helper.Raise_Disconnected
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         if not Self.X.Empty then
            Event := Self.X.Event;
            Self.X.Empty := True;
            exit;
         end if;

         Wait (Self.X.CV, Self.X.M);
      end loop;

      Leave (Self.X.M);
      pragma Debug (O ("succeed to pull new data from pull supplier"));

      return Event;
   end Pull;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : CORBA.Any) is
   begin
      pragma Debug (O ("push new data to pull supplier"));

      Enter (Self.X.M);
      Self.X.Empty := False;
      Self.X.Event := Data;
      Leave (Self.X.M);
      Signal (Self.X.CV);
   end Push;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out    CORBA.Boolean;
      Returns   : out    CORBA.Any) is
   begin
      pragma Debug (O ("try to pull new data from pull supplier"));

      Enter (Self.X.M);
      if CosNotifyChannelAdmin.ProxyPullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Has_Event := not Self.X.Empty;

      if Has_Event then
         Returns := Self.X.Event;
         Self.X.Empty := True;
      end if;

      Leave (Self.X.M);
   end Try_Pull;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : PullSupplier.Ref;
      Peer_Ref : CosNotifyChannelAdmin.ProxyPullConsumer.Ref;
   begin
      pragma Debug (O ("create pullsupplier"));

      Supplier         := new Object;
      Supplier.X       := new Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Empty := True;
      Supplier.X.Peer  := Peer_Ref;
      Create (Supplier.X.M);
      Create (Supplier.X.CV);

      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

end CosNotifyComm.PullSupplier.Impl;
