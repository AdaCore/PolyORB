------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPUSHSUPPLIER.IMPL                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
pragma Warnings (Off, CORBA.Object);

with PortableServer;

with CosEventComm.PushConsumer;

with CosEventChannelAdmin.ConsumerAdmin;

with CosEventChannelAdmin.ProxyPushSupplier.Helper;
pragma Elaborate (CosEventChannelAdmin.ProxyPushSupplier.Helper);
pragma Warnings (Off, CosEventChannelAdmin.ProxyPushSupplier.Helper);

with CosEventChannelAdmin.ProxyPushSupplier.Skel;
pragma Elaborate (CosEventChannelAdmin.ProxyPushSupplier.Skel);
pragma Warnings (Off, CosEventChannelAdmin.ProxyPushSupplier.Skel);

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;

with PolyORB.Log;

package body CosEventChannelAdmin.ProxyPushSupplier.Impl is

   use PortableServer;

   use CosEventComm;
   use CosEventChannelAdmin;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("proxypushsupplier");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Proxy_Push_Supplier_Record is record
      This   : Object_Ptr;
      Peer   : PushConsumer.Ref;
      Admin  : ConsumerAdmin.Impl.Object_Ptr;
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
      Push_Consumer : in     CosEventComm.PushConsumer.Ref) is
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
     (Admin : ConsumerAdmin.Impl.Object_Ptr)
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
      Data : in     CORBA.Any) is
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

end CosEventChannelAdmin.ProxyPushSupplier.Impl;
