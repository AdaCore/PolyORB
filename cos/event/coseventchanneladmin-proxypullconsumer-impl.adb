------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPULLCONSUMER.IMPL                --
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

with CORBA.Impl;

with CosEventChannelAdmin;
with CosEventChannelAdmin.SupplierAdmin.Impl;

with CosEventComm;
with CosEventComm.PullSupplier;

with CosTypedEventComm.TypedPullSupplier;
with CosTypedEventComm.TypedPullSupplier.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;

with CosEventChannelAdmin.ProxyPullConsumer.Skel;
pragma Warnings (Off, CosEventChannelAdmin.ProxyPullConsumer.Skel);

package body CosEventChannelAdmin.ProxyPullConsumer.Impl is

   use CosEventComm;
   use CosEventChannelAdmin;

   use CosTypedEventComm;

   use PortableServer;

   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads;

   use PolyORB.CORBA_P.Server_Tools;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("proxypullconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Proxy_Pull_Consumer_Record is record
      This           : Object_Ptr;
      Peer           : PullSupplier.Ref;
      Admin          : SupplierAdmin.Ref;
      Engin_Launched : Boolean := False;
      --  is there a thread launch for the engine
   end record;

   A_S   : Object_Ptr := null;
   --  This variable is used to initialize the threads local variable.
   --  it is used to replace the 'accept' statement.

   Session_Mutex : Mutex_Access;
   Session_Taken : Condition_Access;
   --  Synchornisation of task initialization.

   Peer_Mutex : Mutex_Access;
   --  Protect access on a peer component

   T_Initialized : Boolean := False;

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization is
   begin
      if T_Initialized then
         return;
      end if;
      Create (Session_Mutex);
      Create (Session_Taken);
      Create (Peer_Mutex);

      T_Initialized := True;
   end Ensure_Initialization;

   -------------------------------
   -- Proxy_Pull_Consumer_Engin --
   -------------------------------

   procedure Proxy_Pull_Consumer_Engine;

   procedure Proxy_Pull_Consumer_Engine
   is
      This  : Object_Ptr;
      Peer  : PullSupplier.Ref;
      Event : CORBA.Any;
      Obj   : CORBA.Impl.Object_Ptr;
   begin
      pragma Debug (O ("Session Thread number "
                       & Image (Current_Task)
                       & " is starting"));
      --  Signal end of thread initialization.

      Ensure_Initialization;

      --  Thread initialization.
      --  A_S is a global variable used to pass an argument to this task
      This := A_S;
      --  This is initialized
      --  we can let Connect_Pull_Supplier go
      Enter (Session_Mutex);
      Signal (Session_Taken);
      Leave (Session_Mutex);

      loop
         --  Session thread main loop.
         Enter (Peer_Mutex);
         Peer := This.X.Peer;
         Leave (Peer_Mutex);

         exit when PullSupplier.Is_Nil (Peer);

         pragma Debug
              (O ("pull new data from proxy pull consumer engin"));

         begin
            Event := PullSupplier.pull (Peer);
         exception when others =>
            exit;
         end;

         pragma Debug
           (O ("post new data from proxy pull consumer to admin"));

         Reference_To_Servant (This.X.Admin, Servant (Obj));
         SupplierAdmin.Impl.Post
         (SupplierAdmin.Impl.Object_Ptr (Obj), Event);
      end loop;

      This.X.Engin_Launched := False;
   end Proxy_Pull_Consumer_Engine;

   ---------------------------
   -- Connect_Pull_Supplier --
   ---------------------------

   procedure Connect_Pull_Supplier
     (Self          : access Object;
      Pull_Supplier : CosEventComm.PullSupplier.Ref) is
   begin
      pragma Debug (O ("connect pull supplier to proxy pull consumer"));

      Ensure_Initialization;

      Enter (Session_Mutex);
      if not PullSupplier.Is_Nil (Self.X.Peer) then
         Leave (Session_Mutex);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Pull_Supplier;
      A_S := Self.X.This;

      --  Start engin
      if not Self.X.Engin_Launched then
         Create_Task
           (Proxy_Pull_Consumer_Engine'Access, "Proxy_Poll_Consumer");
         Self.X.Engin_Launched := True;
         --  thread created
      end if;

      --  wait  A_S initialization in Proxy_Pull_Consumer_Engin
      Wait (Session_Taken, Session_Mutex);
      Leave (Session_Mutex);

   end Connect_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create
     (Admin : SupplierAdmin.Ref)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : ProxyPullConsumer.Ref;

   begin
      pragma Debug (O ("create proxy pull consumer"));

      Consumer         := new Object;
      Consumer.X       := new Proxy_Pull_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin;
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
      Obj : CORBA.Impl.Object_Ptr;
   begin
      pragma Debug
        (O ("calling get_typed_supplier from " &
            "proxy pullconsumer to typed pullsupplier"));

      begin
         Reference_To_Servant (Self.X.Peer, Servant (Obj));
         Ref := TypedPullSupplier.Impl.Get_Typed_Supplier
                 (TypedPullSupplier.Impl.Object_Ptr (Obj));
      exception
         when others =>
            pragma Debug (O ("Got exception in Pull"));
            raise;
      end;

      return Ref;

   end Pull;

   ------------------------------
   -- Disconnect_Pull_Consumer --
   ------------------------------

   procedure Disconnect_Pull_Consumer
     (Self : access Object)
   is
      Peer    : PullSupplier.Ref;
      Nil_Ref : PullSupplier.Ref;

   begin
      pragma Debug (O ("disconnect proxy pull consumer"));

      Enter (Peer_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Peer_Mutex);

      if not PullSupplier.Is_Nil (Peer) then
         PullSupplier.disconnect_pull_supplier (Peer);
      end if;

   end Disconnect_Pull_Consumer;

end CosEventChannelAdmin.ProxyPullConsumer.Impl;
