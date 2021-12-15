------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPULLSUPPLIER.IMPL                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2021, Free Software Foundation, Inc.          --
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

with CosEventComm.PullConsumer;

with PolyORB.Log;
with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Utils.Chained_Lists;

with CosEventChannelAdmin.ProxyPullSupplier.Skel;
pragma Warnings (Off, CosEventChannelAdmin.ProxyPullSupplier.Skel);

package body CosEventChannelAdmin.ProxyPullSupplier.Impl is

   use PortableServer;

   use CosEventComm;
   use CosEventChannelAdmin;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("proxypullsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package Event_Queues is new PolyORB.Utils.Chained_Lists
     (CORBA.Any, CORBA."=");
   use Event_Queues;

   subtype Event_Queue is Event_Queues.List;

   type Proxy_Pull_Supplier_Record is record
      This  : Object_Ptr;
      Peer  : PullConsumer.Ref;
      Admin : ConsumerAdmin.Impl.Object_Ptr;
      Queue : Event_Queue;
      M     : Mutex_Access;
      CV    : Condition_Access;
   end record;

   ---------------------------
   -- Connect_Pull_Consumer --
   ---------------------------

   procedure Connect_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : PullConsumer.Ref) is
   begin
      pragma Debug (O ("connect pull consumer to proxy pull supplier"));

      Enter (Self.X.M);

      if not PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Pull_Consumer;
      Leave (Self.X.M);
   end Connect_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create
     (Admin : ConsumerAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("create proxy pull supplier"));

      Supplier         := new Object;
      Supplier.X       := new Proxy_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;
      Create (Supplier.X.M);
      Create (Supplier.X.CV);

      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier (Self : access Object) is
      Peer    : PullConsumer.Ref;
      Nil_Ref : PullConsumer.Ref;

   begin
      pragma Debug (O ("disconnect proxy pull supplier"));

      Enter (Self.X.M);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);
      Broadcast (Self.X.CV);

      if not PullConsumer.Is_Nil (Peer) then
         PullConsumer.disconnect_pull_consumer (Peer);
      end if;
   end Disconnect_Pull_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : CORBA.Any) is
   begin
      pragma Debug (O ("post new data to proxy pull supplier"));

      Enter (Self.X.M);
      Append (Self.X.Queue, Data);
      Leave (Self.X.M);

      Signal (Self.X.CV);
   end Post;

   ----------
   -- Pull --
   ----------

   function Pull (Self : access Object) return CORBA.Any is
      Event : CORBA.Any;

   begin
      pragma Debug (O ("attempt to pull new data from proxy pull supplier"));

      Enter (Self.X.M);

      loop
         if PullConsumer.Is_Nil (Self.X.Peer) then
            Leave (Self.X.M);
            raise Disconnected;
         end if;

         exit when Length (Self.X.Queue) > 0;

         Wait (Self.X.CV, Self.X.M);
      end loop;

      Extract_First (Self.X.Queue, Event);

      Leave (Self.X.M);

      return Event;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out    CORBA.Boolean;
      Returns   : out    CORBA.Any) is
   begin
      pragma Debug (O ("try to pull new data from proxy pull supplier"));

      Enter (Self.X.M);

      if PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         raise Disconnected;
      end if;

      Has_Event := Length (Self.X.Queue) > 0;

      if Has_Event then
         Extract_First (Self.X.Queue, Returns);
      end if;
      Leave (Self.X.M);
   end Try_Pull;

end CosEventChannelAdmin.ProxyPullSupplier.Impl;
