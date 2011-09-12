------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCOMM.STRUCTUREDPULLSUPPLIER.IMPL                 --
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

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

with CosNotifyComm.StructuredPullSupplier.Skel;
pragma Warnings (Off, CosNotifyComm.StructuredPullSupplier.Skel);

package body CosNotifyComm.StructuredPullSupplier.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("structuredpullsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Structured_Pull_Supplier_Record is record
      This      : Object_Ptr;
      Peer      : CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref;
      Empty     : Boolean;
      Event     : CosNotification.StructuredEvent;
      M         : Mutex_Access;
      CV        : Condition_Access;
   end record;

   --------------------------------------------
   -- Connect_Structured_Proxy_Pull_Consumer --
   --------------------------------------------

   procedure Connect_Structured_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref)
   is
      My_Ref  : StructuredPullSupplier.Ref;
   begin
      pragma Debug
      (O ("connect_structured_proxy_pull_consumer in structuredpullsupplier"));

      Enter (Self.X.M);
      if not CosNotifyChannelAdmin.StructuredProxyPullConsumer.Is_Nil
      (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;
      Self.X.Peer := Proxy;

      Servant_To_Reference (PortableServer.Servant (Self.X.This), My_Ref);
      Leave (Self.X.M);

      CosNotifyChannelAdmin.StructuredProxyPullConsumer.
      connect_structured_pull_supplier (Proxy, My_Ref);

   end Connect_Structured_Proxy_Pull_Consumer;

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
      pragma Debug (O ("subscription_change in structuredpullsupplier"));
      null;
   end Subscription_Change;

   -----------------------------------------
   -- Disconnect_Structured_Pull_Supplier --
   -----------------------------------------

   procedure Disconnect_Structured_Pull_Supplier
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref;
      Nil_Ref : CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref;
   begin
      pragma Debug (O ("disconnect structuredpullsupplier"));

      Enter (Self.X.M);
      Peer := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);
      Broadcast (Self.X.CV);

      if not CosNotifyChannelAdmin.StructuredProxyPullConsumer.Is_Nil
      (Peer) then
         CosNotifyChannelAdmin.StructuredProxyPullConsumer.
         disconnect_structured_pull_consumer (Peer);
      end if;
   end Disconnect_Structured_Pull_Supplier;

   ---------------------------
   -- Pull_Structured_Event --
   ---------------------------

   function Pull_Structured_Event
     (Self : access Object)
     return CosNotification.StructuredEvent
   is
      Event : CosNotification.StructuredEvent;
   begin
      pragma Debug
        (O ("attempt to pull new structured event from pull supplier"));

      Enter (Self.X.M);

      loop
         if CosNotifyChannelAdmin.StructuredProxyPullConsumer.Is_Nil
         (Self.X.Peer) then
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
      pragma Debug
      (O ("succeed to pull new structured event from pull supplier"));

      return Event;
   end Pull_Structured_Event;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : CosNotification.StructuredEvent) is
   begin
      pragma Debug (O ("push new structured event to structuredpullsupplier"));

      Enter (Self.X.M);
      Self.X.Empty := False;
      Self.X.Event := Data;
      Leave (Self.X.M);
      Signal (Self.X.CV);
   end Push;

   -------------------------------
   -- Try_Pull_Structured_Event --
   -------------------------------

   procedure Try_Pull_Structured_Event
     (Self      : access Object;
      Has_Event : out    CORBA.Boolean;
      Returns   : out    CosNotification.StructuredEvent) is
   begin
      pragma Debug
      (O ("try to pull new structured event from structuredpullsupplier"));

      Enter (Self.X.M);
      if CosNotifyChannelAdmin.StructuredProxyPullConsumer.Is_Nil
      (Self.X.Peer) then
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
   end Try_Pull_Structured_Event;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : StructuredPullSupplier.Ref;
      Peer_Ref : CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref;
   begin
      pragma Debug (O ("create structuredpullsupplier"));

      Supplier         := new Object;
      Supplier.X       := new Structured_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Empty := True;
      Supplier.X.Peer  := Peer_Ref;
      Create (Supplier.X.M);
      Create (Supplier.X.CV);

      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

end CosNotifyComm.StructuredPullSupplier.Impl;
