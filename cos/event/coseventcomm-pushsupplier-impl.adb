------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S E V E N T C O M M . P U S H S U P P L I E R . I M P L        --
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
