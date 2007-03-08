------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPULLSUPPLIER.IMPL                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

with CosEventComm.PullConsumer;
with CosEventChannelAdmin;

with PolyORB.Log;
with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Semaphores;
with PolyORB.Utils.Chained_Lists;

with CosEventChannelAdmin.ProxyPullSupplier.Skel;
pragma Warnings (Off, CosEventChannelAdmin.ProxyPullSupplier.Skel);

package body CosEventChannelAdmin.ProxyPullSupplier.Impl is

   use PortableServer;

   use CosEventComm;
   use CosEventChannelAdmin;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Semaphores;

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
      This      : Object_Ptr;
      Peer      : PullConsumer.Ref;
      Admin     : ConsumerAdmin.Impl.Object_Ptr;
      Queue     : Event_Queue;
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

   ---------------------------
   -- Connect_Pull_Consumer --
   ---------------------------

   procedure Connect_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : PullConsumer.Ref) is
   begin
      pragma Debug (O ("connect pull consumer to proxy pull supplier"));
      Ensure_Initialization;

      Enter (Self_Mutex);

      if not PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Pull_Consumer;

      Leave (Self_Mutex);
   end Connect_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create
     (Admin : ConsumerAdmin.Impl.Object_Ptr)
     return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("create proxy pull supplier"));

      Supplier         := new Object;
      Supplier.X       := new Proxy_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;
      Create (Supplier.X.Semaphore);

      Initiate_Servant (Servant (Supplier), My_Ref);

      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier
     (Self : access Object)
   is
      Peer    : PullConsumer.Ref;
      Nil_Ref : PullConsumer.Ref;

   begin
      pragma Debug (O ("disconnect proxy pull supplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Append (Self.X.Queue, Data);
      Leave (Self_Mutex);

      V (Self.X.Semaphore);
   end Post;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Event : CORBA.Any;

   begin
      pragma Debug
        (O ("attempt to pull new data from proxy pull supplier"));

      Ensure_Initialization;

      P (Self.X.Semaphore);

      Enter (Self_Mutex);

      if PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise Disconnected;
      end if;

      if State (Self.X.Semaphore) >= 0 then
         Extract_First (Self.X.Queue, Event);
         pragma Debug (O ("succeed to pull data from proxy pull supplier"));
      end if;

      Leave (Self_Mutex);

      --  XXX what if nothing was pulled ?

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

      Ensure_Initialization;

      Enter (Self_Mutex);

      if PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise Disconnected;
      end if;

      Has_Event := State (Self.X.Semaphore) > 0;

      if Has_Event then
         Extract_First (Self.X.Queue, Returns);
         Leave (Self_Mutex);

         P (Self.X.Semaphore);
      end if;

   end Try_Pull;

end CosEventChannelAdmin.ProxyPullSupplier.Impl;
