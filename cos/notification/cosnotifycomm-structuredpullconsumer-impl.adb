------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCOMM.STRUCTUREDPULLCONSUMER.IMPL                 --
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

with CosNotifyChannelAdmin.StructuredProxyPullSupplier;

with CosNotifyComm.StructuredPullConsumer.Helper;
pragma Elaborate (CosNotifyComm.StructuredPullConsumer.Helper);
pragma Warnings (Off, CosNotifyComm.StructuredPullConsumer.Helper);

with CosNotifyComm.StructuredPullConsumer.Skel;
pragma Elaborate (CosNotifyComm.StructuredPullConsumer.Skel);
pragma Warnings (Off, CosNotifyComm.StructuredPullConsumer.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
with PolyORB.Log;

package body CosNotifyComm.StructuredPullConsumer.Impl is

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("structuredpullconsumer");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Structured_Pull_Consumer_Record is record
      This : Object_Ptr;
      Peer : CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref;
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

   -----------------------------------------
   -- Disconnect_Structured_Pull_Consumer --
   -----------------------------------------

   procedure Disconnect_Structured_Pull_Consumer
     (Self : access Object)
   is
      Peer    : CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref;
      Nil_Ref : CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("disconnect structuredpullconsumer"));

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not CosNotifyChannelAdmin.StructuredProxyPullSupplier.Is_Nil
      (Peer) then
         CosNotifyChannelAdmin.StructuredProxyPullSupplier.
         disconnect_structured_pull_supplier (Peer);
      end if;
   end Disconnect_Structured_Pull_Consumer;

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
      pragma Debug (O ("offer_change in structuredpullconsumer"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Offer_Change;

   --------------------------------------------
   -- Connect_Structured_Proxy_Pull_Supplier --
   --------------------------------------------

   procedure Connect_Structured_Proxy_Pull_Supplier
      (Self  : access Object;
       Proxy : in     CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref)
   is
      My_Ref   : StructuredPullConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("connect_structured_proxy_pull_supplier in structuredpullconsumer"));

      Enter (Self_Mutex);
      if not CosNotifyChannelAdmin.StructuredProxyPullSupplier.Is_Nil
      (Self.X.Peer) then
         Leave (Self_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Proxy;

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      Leave (Self_Mutex);

      CosNotifyChannelAdmin.StructuredProxyPullSupplier.
      connect_structured_pull_consumer (Proxy, My_Ref);
   end Connect_Structured_Proxy_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : StructuredPullConsumer.Ref;
      Peer_Ref : CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref;
   begin
      pragma Debug (O ("create structuredpullconsumer"));

      Consumer        := new Object;
      Consumer.X      := new Structured_Pull_Consumer_Record;
      Consumer.X.This := Consumer;
      Consumer.X.Peer := Peer_Ref;
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CosNotification.StructuredEvent
   is
      Peer : CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("pull structured event from structuredpullconsumer"));

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.StructuredProxyPullSupplier.Is_Nil (Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      return CosNotifyChannelAdmin.StructuredProxyPullSupplier.
             pull_structured_event (Peer);
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self    : access Object;
      Done    : out    CORBA.Boolean;
      Returns : out    CosNotification.StructuredEvent)
   is
      Peer : CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref;
   begin
      pragma Debug
      (O ("try to pull structured event from structuredpullconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if CosNotifyChannelAdmin.StructuredProxyPullSupplier.Is_Nil (Peer) then
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      CosNotifyChannelAdmin.StructuredProxyPullSupplier.
      try_pull_structured_event (Peer, Done, Returns);
   end Try_Pull;

end CosNotifyComm.StructuredPullConsumer.Impl;
