------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S E V E N T C O M M . P U S H C O N S U M E R . I M P L        --
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
with PolyORB.Tasking.Condition_Variables;

with CosEventComm.PushConsumer.Skel;
pragma Warnings (Off, CosEventComm.PushConsumer.Skel);

package body CosEventComm.PushConsumer.Impl is

   use PortableServer;

   use CosEventChannelAdmin;

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
      This    : Object_Ptr;
      Peer    : ProxyPushSupplier.Ref;
      Empty   : Boolean;
      Event   : CORBA.Any;
      M       : Mutex_Access;
      CV      : Condition_Access;
   end record;

   ---------------------------------
   -- Connect_Proxy_Push_Supplier --
   ---------------------------------

   procedure Connect_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : CosEventChannelAdmin.ProxyPushSupplier.Ref)
   is
      My_Ref : PushConsumer.Ref;

   begin
      pragma Debug (O ("connect proxy push consumer to push supplier"));

      Enter (Self.X.M);
      if not ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave (Self.X.M);

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPushSupplier.connect_push_consumer (Proxy, My_Ref);
   end Connect_Proxy_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr is
      Consumer : Object_Ptr;
      My_Ref   : PushConsumer.Ref;

   begin
      pragma Debug (O ("create push consumer"));

      Consumer         := new Object;
      Consumer.X       := new Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Empty := True;
      Create (Consumer.X.M);
      Create (Consumer.X.CV);
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Push_Consumer --
   ------------------------------

   procedure Disconnect_Push_Consumer (Self : access Object) is
      Peer    : ProxyPushSupplier.Ref;
      Nil_Ref : ProxyPushSupplier.Ref;

   begin
      pragma Debug (O ("disconnect push consumer"));

      Enter (Self.X.M);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);
      Broadcast (Self.X.CV);

      if not ProxyPushSupplier.Is_Nil (Peer) then
         ProxyPushSupplier.disconnect_push_supplier (Peer);
      end if;
   end Disconnect_Push_Consumer;

   ----------
   -- Pull --
   ----------

   function Pull (Self : access Object) return CORBA.Any is
      Event   : CORBA.Any;

   begin
      Enter (Self.X.M);
      loop
         pragma Debug (O ("attempt to pull new data from push consumer"));

         if ProxyPushSupplier.Is_Nil (Self.X.Peer) then
            Leave (Self.X.M);
            raise Disconnected;
         end if;

         if not Self.X.Empty then
            Self.X.Empty := True;
            Event := Self.X.Event;
            Leave (Self.X.M);

            exit;
         end if;
         Wait (Self.X.CV, Self.X.M);
      end loop;
      Leave (Self.X.M);

      pragma Debug (O ("succeed to pull new data from push consumer"));

      return Event;
   end Pull;

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

      --  What if Self.X.Empty is already False? Are we losing the previous
      --  event???

      Leave (Self.X.M);
      Signal (Self.X.CV);
   end Push;

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

      if ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         raise Disconnected;
      end if;

      Done := not Self.X.Empty;

      if Done then
         Self.X.Empty := True;
         Data := Self.X.Event;
      end if;

      Leave (Self.X.M);
   end Try_Pull;

end CosEventComm.PushConsumer.Impl;
