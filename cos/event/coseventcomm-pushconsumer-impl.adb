------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S E V E N T C O M M . P U S H C O N S U M E R . I M P L        --
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

with CosEventChannelAdmin;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Semaphores;

with CosEventComm.PushConsumer.Skel;
pragma Warnings (Off, CosEventComm.PushConsumer.Skel);

package body CosEventComm.PushConsumer.Impl is

   use PortableServer;

   use CosEventChannelAdmin;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Semaphores;

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      if not ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave (Self_Mutex);

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPushSupplier.connect_push_consumer (Proxy, My_Ref);
   end Connect_Proxy_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : PushConsumer.Ref;

   begin
      pragma Debug (O ("create push consumer"));

      Consumer         := new Object;
      Consumer.X       := new Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Empty := True;
      Create (Consumer.X.Semaphore);
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Push_Consumer --
   ------------------------------

   procedure Disconnect_Push_Consumer
     (Self : access Object)
   is
      Peer    : ProxyPushSupplier.Ref;
      Nil_Ref : ProxyPushSupplier.Ref;

   begin
      pragma Debug (O ("disconnect push consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);

      if not ProxyPushSupplier.Is_Nil (Peer) then
         ProxyPushSupplier.disconnect_push_supplier (Peer);
      end if;
   end Disconnect_Push_Consumer;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Event   : CORBA.Any;

   begin
      Ensure_Initialization;

      loop
         pragma Debug (O ("attempt to pull new data from push consumer"));
         P (Self.X.Semaphore);

         Enter (Self_Mutex);
         if ProxyPushSupplier.Is_Nil (Self.X.Peer) then
            Leave (Self_Mutex);
            raise Disconnected;
         end if;

         if not Self.X.Empty then
            Self.X.Empty := True;
            Event := Self.X.Event;
            Leave (Self_Mutex);
            exit;
         end if;

         Leave (Self_Mutex);
      end loop;
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
      Ensure_Initialization;

      Enter (Self_Mutex);
      Self.X.Empty := False;
      Self.X.Event := Data;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);
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

      Ensure_Initialization;

      Enter (Self_Mutex);

      if ProxyPushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise Disconnected;
      end if;

      Done := not Self.X.Empty;

      if Done then
         Self.X.Empty := True;
         Data := Self.X.Event;
      end if;

      Leave (Self_Mutex);
   end Try_Pull;

end CosEventComm.PushConsumer.Impl;
