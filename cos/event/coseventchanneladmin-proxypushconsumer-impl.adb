------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPUSHCONSUMER.IMPL                --
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

with CosEventComm;
with CosEventComm.PushSupplier;
with CosEventChannelAdmin;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosEventChannelAdmin.ProxyPushConsumer.Skel;
pragma Warnings (Off, CosEventChannelAdmin.ProxyPushConsumer.Skel);

package body CosEventChannelAdmin.ProxyPushConsumer.Impl is

   use PortableServer;

   use CosEventComm;
   use CosEventChannelAdmin;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("proxypushconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Proxy_Push_Consumer_Record is record
      This   : Object_Ptr;
      Peer   : PushSupplier.Ref;
      Admin  : SupplierAdmin.Impl.Object_Ptr;
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
   -- Connect_Push_Supplier --
   ---------------------------

   procedure Connect_Push_Supplier
     (Self          : access Object;
      Push_Supplier : CosEventComm.PushSupplier.Ref) is
   begin
      pragma Debug (O ("connect push supplier to proxy push consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      if not PushSupplier.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Push_Supplier;

      Leave (Self_Mutex);
   end Connect_Push_Supplier;

   ------------
   -- Create --
   ------------

   function Create
     (Admin : SupplierAdmin.Impl.Object_Ptr)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("create proxy push consumer"));

      Consumer         := new Object;
      Consumer.X       := new Proxy_Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin;

      Initiate_Servant (Servant (Consumer), My_Ref);

      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Push_Consumer --
   ------------------------------

   procedure Disconnect_Push_Consumer
     (Self : access Object)
   is
      Peer    : PushSupplier.Ref;
      Nil_Ref : PushSupplier.Ref;

   begin
      pragma Debug (O ("disconnect proxy push consumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not PushSupplier.Is_Nil (Peer) then
         PushSupplier.disconnect_push_supplier (Peer);
      end if;
   end Disconnect_Push_Consumer;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : CORBA.Any) is
   begin
      pragma Debug
        (O ("push new data from proxy push consumer to supplier admin"));

      SupplierAdmin.Impl.Post (Self.X.Admin, Data);
   end Push;

end CosEventChannelAdmin.ProxyPushConsumer.Impl;
