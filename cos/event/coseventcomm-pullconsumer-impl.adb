------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S E V E N T C O M M . P U L L C O N S U M E R . I M P L        --
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
with PolyORB.Tasking.Mutexes;
with PolyORB.Log;

with CosEventComm.PullConsumer.Skel;
pragma Warnings (Off, CosEventComm.PullConsumer.Skel);

package body CosEventComm.PullConsumer.Impl is

   use CosEventChannelAdmin;
   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("pullconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Pull_Consumer_Record is record
      This  : Object_Ptr;
      Peer  : ProxyPullSupplier.Ref;
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
   -- Connect_Proxy_Pull_Supplier --
   ---------------------------------

   procedure Connect_Proxy_Pull_Supplier
     (Self  : access Object;
      Proxy : ProxyPullSupplier.Ref)
   is
      My_Ref : PullConsumer.Ref;

   begin
      pragma Debug (O ("connect proxy pull consumer to pull supplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      if not ProxyPullSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave (Self_Mutex);

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPullSupplier.connect_pull_consumer (Proxy, My_Ref);
   end Connect_Proxy_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : PullConsumer.Ref;

   begin
      pragma Debug (O ("create pull consumer"));

      Consumer        := new Object;
      Consumer.X      := new Pull_Consumer_Record;
      Consumer.X.This := Consumer;

      Initiate_Servant (Servant (Consumer), My_Ref);

      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Consumer --
   ------------------------------

   procedure Disconnect_Pull_Consumer
     (Self : access Object)
   is
      Peer    : ProxyPullSupplier.Ref;
      Nil_Ref : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("disconnect pull consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not ProxyPullSupplier.Is_Nil (Peer) then
         ProxyPullSupplier.disconnect_pull_supplier (Peer);
      end if;
   end Disconnect_Pull_Consumer;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Peer : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("pull new data from pull consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if ProxyPullSupplier.Is_Nil (Peer) then
         raise Disconnected;
      end if;

      return ProxyPullSupplier.pull (Peer);
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self    : access Object;
      Done    : out    CORBA.Boolean;
      Returns : out    CORBA.Any)
   is
      Peer : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("try to pull new data from pull consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer := Self.X.Peer;
      Leave (Self_Mutex);

      if ProxyPullSupplier.Is_Nil (Peer) then
         raise Disconnected;
      end if;

      ProxyPullSupplier.try_pull (Peer, Done, Returns);
   end Try_Pull;

end CosEventComm.PullConsumer.Impl;
