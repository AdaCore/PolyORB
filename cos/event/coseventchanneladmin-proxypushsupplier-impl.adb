------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPUSHSUPPLIER.IMPL                --
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

with CosEventComm.PushConsumer;
with CosTypedEventComm.TypedPushConsumer;
with CosTypedEventComm.TypedPushConsumer.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosEventChannelAdmin.ProxyPushSupplier.Skel;
pragma Warnings (Off, CosEventChannelAdmin.ProxyPushSupplier.Skel);

package body CosEventChannelAdmin.ProxyPushSupplier.Impl is

   use CosEventComm;
   use CosEventChannelAdmin;

   use CosTypedEventComm;

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("proxypushsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Proxy_Push_Supplier_Record is record
      This   : Object_Ptr;
      Peer   : PushConsumer.Ref;
      Admin  : ConsumerAdmin.Ref;
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
   -- Connect_Push_Consumer --
   ---------------------------

   procedure Connect_Push_Consumer
     (Self          : access Object;
      Push_Consumer : CosEventComm.PushConsumer.Ref) is
   begin
      pragma Debug (O ("connect push consumer to proxy push supplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);

      if not PushConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Push_Consumer;

      Leave (Self_Mutex);
   end Connect_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create
     (Admin : ConsumerAdmin.Ref)
     return Object_Ptr
   is
      Supplier : ProxyPushSupplier.Impl.Object_Ptr;
      My_Ref   : ProxyPushSupplier.Ref;

   begin
      pragma Debug (O ("create proxy push supplier"));

      Supplier         := new Object;
      Supplier.X       := new Proxy_Push_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;

      Initiate_Servant (Servant (Supplier), My_Ref);

      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Push_Supplier --
   ------------------------------

   procedure Disconnect_Push_Supplier
     (Self : access Object)
   is
      Peer    : PushConsumer.Ref;
      Nil_Ref : PushConsumer.Ref;

   begin
      pragma Debug (O ("disconnect proxy push supplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if PushConsumer.Is_Nil (Peer) then
         PushConsumer.disconnect_push_consumer (Peer);
      end if;
   end Disconnect_Push_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : CORBA.Any) is
   begin
      pragma Debug
        (O ("post new data from proxy push supplier to push consumer"));

      begin
         PushConsumer.push (Self.X.Peer, Data);
      exception
         when others =>
            pragma Debug (O ("Got exception in Post"));
            raise;
      end;
   end Post;

   ----------
   -- Post --
   ----------

   function Post
   (Self : access Object)
      return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
      Obj   : CORBA.Impl.Object_Ptr;
   begin
      pragma Debug
        (O ("calling get_typed_consumer from" &
            " proxy pushsupplier to typed push consumer"));

      begin
         Reference_To_Servant (Self.X.Peer, Servant (Obj));
         Ref := TypedPushConsumer.Impl.Get_Typed_Consumer
                (TypedPushConsumer.Impl.Object_Ptr (Obj));
      exception
         when others =>
            pragma Debug (O ("Got exception in Post"));
            raise;
      end;

      return Ref;

   end Post;

end CosEventChannelAdmin.ProxyPushSupplier.Impl;
