------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S E V E N T C O M M . P U S H S U P P L I E R . I M P L        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with CosEventChannelAdmin;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosEventComm.PushSupplier.Skel;
pragma Warnings (Off, CosEventComm.PushSupplier.Skel);

package body CosEventComm.PushSupplier.Impl is

   use CosEventChannelAdmin;

   use PortableServer;

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
      This  : Object_Ptr;
      Peer  : ProxyPushConsumer.Ref;
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

   ---------------------------------
   -- Connect_Proxy_Push_Consumer --
   ---------------------------------

   procedure Connect_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : CosEventChannelAdmin.ProxyPushConsumer.Ref)
   is
      My_Ref : PushSupplier.Ref;

   begin
      pragma Debug (O ("connect proxy push supplier to push consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      if not ProxyPushConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave (Self_Mutex);

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPushConsumer.connect_push_supplier (Proxy, My_Ref);
   end Connect_Proxy_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : PushSupplier.Ref;

   begin
      pragma Debug (O ("create push supplier"));

      Supplier        := new Object;
      Supplier.X      := new Push_Supplier_Record;
      Supplier.X.This := Supplier;

      Initiate_Servant (Servant (Supplier), My_Ref);

      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Push_Supplier --
   ------------------------------

   procedure disconnect_push_supplier
     (Self : access Object)
   is
      Peer    : ProxyPushConsumer.Ref;
      Nil_Ref : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("disconnect push supplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not ProxyPushConsumer.Is_Nil (Peer) then
         ProxyPushConsumer.disconnect_push_consumer (Peer);
      end if;
   end disconnect_push_supplier;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : CORBA.Any)
   is
      Peer : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("push new data to push supplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if ProxyPushConsumer.Is_Nil (Peer) then
         raise Disconnected;
      end if;

      ProxyPushConsumer.push (Peer, Data);
   end Push;

end CosEventComm.PushSupplier.Impl;
