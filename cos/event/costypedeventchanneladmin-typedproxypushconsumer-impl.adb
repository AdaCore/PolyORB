------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          COSTYPEDEVENTCHANNELADMIN.TYPEDPROXYPUSHCONSUMER.IMPL           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with CosEventComm;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosTypedEventChannelAdmin.TypedProxyPushConsumer.Skel;
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedProxyPushConsumer.Skel);

package body CosTypedEventChannelAdmin.TypedProxyPushConsumer.Impl is

   use CosEventChannelAdmin;
   use CosEventComm;

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedproxypushconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type TypedProxy_Push_Consumer_Record is record
      This   : Object_Ptr;
      Peer   : PushSupplier.Ref;
      Admin  : TypedSupplierAdmin.Impl.Object_Ptr;
      supported_interface  : CosTypedEventChannelAdmin.Key;
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
      pragma Debug (O ("connect pushsupplier to typedproxy push consumer"));

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
     (Admin : TypedSupplierAdmin.Impl.Object_Ptr;
      supported_interface : CosTypedEventChannelAdmin.Key)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : TypedProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("create typedproxy push consumer"));

      Consumer         := new Object;
      Consumer.X       := new TypedProxy_Push_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin;
      Consumer.X.supported_interface := supported_interface;

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
      pragma Debug (O ("disconnect typedproxy push consumer"));

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

   procedure Push (Self : access Object; Data : CORBA.Any) is
      pragma Unreferenced (Self, Data);
   begin
      pragma Debug (O ("attempt to push new data to typed pushconsumer"));
      pragma Debug (O ("no need to use generic push in typed pushconsumer"));

      Ensure_Initialization;

      --  No need to implement push in Typed PushConsumer

      raise Program_Error;
   end Push;

   ------------------------
   -- Get_Typed_Consumer --
   ------------------------

   function Get_Typed_Consumer
   (Self : access Object)
      return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("get the mutually agreed interface from "&
                       "typed pushconsumer"));
      pragma Debug (O ("push mutually agreed interface from " &
                       "typed proxy pushconsumer to typed supplieradmin"));
      Ensure_Initialization;

      Enter (Self_Mutex);

      Ref := TypedSupplierAdmin.Impl.Post
             (Self.X.Admin, Self.X.supported_interface);
      Leave (Self_Mutex);

      return Ref;
   end Get_Typed_Consumer;

end CosTypedEventChannelAdmin.TypedProxyPushConsumer.Impl;
