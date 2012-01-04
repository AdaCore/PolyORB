------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            COSTYPEDEVENTCHANNELADMIN.TYPEDCONSUMERADMIN.IMPL             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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
with CosEventChannelAdmin.ConsumerAdmin.Helper;
with CosEventChannelAdmin.ProxyPushSupplier.Impl;

with CosTypedEventChannelAdmin.TypedEventChannel;
with CosTypedEventChannelAdmin.TypedProxyPullSupplier;
with CosTypedEventChannelAdmin.TypedProxyPullSupplier.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Dynamic_Dict;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosTypedEventChannelAdmin.TypedConsumerAdmin.Skel;
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedConsumerAdmin.Skel);

package body CosTypedEventChannelAdmin.TypedConsumerAdmin.Impl is

   use CosEventChannelAdmin;
   use CosEventChannelAdmin.ProxyPushSupplier.Impl;
   use CosTypedEventChannelAdmin.TypedEventChannel.Impl;

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedconsumeradmin");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package ProxyPushSuppliersTable is
      new PolyORB.Dynamic_Dict (ProxyPushSupplier.Impl.Object_Ptr);

   type TypedConsumer_Admin_Record is record
      This    : Object_Ptr;
      ThisRef : TypedConsumerAdmin.Ref;
      Channel : TypedEventChannel.Impl.Object_Ptr;
   end record;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized.

   T_Initialized : Boolean := False;
   Self_Mutex : Mutex_Access;

   procedure Ensure_Initialization is
   begin
      if not T_Initialized then
         Create (Self_Mutex);
         T_Initialized := True;
      end if;
   end Ensure_Initialization;

   ------------
   -- Create --
   ------------

   function Create (Channel : TypedEventChannel.Impl.Object_Ptr)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : TypedConsumerAdmin.Ref;
   begin
      pragma Debug (O ("create typedconsumer admin"));

      Consumer           := new Object;
      Consumer.X         := new TypedConsumer_Admin_Record;
      Consumer.X.This    := Consumer;
      Consumer.X.Channel := Channel;
      Initiate_Servant (Servant (Consumer), My_Ref);
      Consumer.X.ThisRef :=   My_Ref;
      return Consumer;
   end Create;

   --------------------------------
   -- Obtain_Typed_Pull_Supplier --
   --------------------------------

   function obtain_typed_pull_supplier
     (Self : access Object;
      supported_interface : CosTypedEventChannelAdmin.Key)
      return TypedProxyPullSupplier.Ref
   is
      Supplier : TypedProxyPullSupplier.Impl.Object_Ptr;
      Its_Ref  : TypedProxyPullSupplier.Ref;
      MyCreate_Ptr : TypedEventChannel.Impl.Interface_Ptr;
   begin
      pragma Debug (O ("obtain typed proxypullsupplier from " &
                       "typed consumeradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);

      MyCreate_Ptr := TypedEventChannel.Impl.Lookup (supported_interface);
      if MyCreate_Ptr = null then
         raise InterfaceNotSupported;
      end if;

      Supplier := TypedProxyPullSupplier.Impl.Create (Self.X.This,
                                                      supported_interface);

      Leave (Self_Mutex);

      Servant_To_Reference (Servant (Supplier), Its_Ref);
      return Its_Ref;
   end obtain_typed_pull_supplier;

   --------------------------------
   -- Obtain_Typed_Push_Supplier --
   --------------------------------

   function obtain_typed_push_supplier
     (Self : access Object;
      uses_interface : CosTypedEventChannelAdmin.Key)
      return ProxyPushSupplier.Ref
   is
      Supplier : ProxyPushSupplier.Impl.Object_Ptr;
      Its_Ref  : ProxyPushSupplier.Ref;
      MyCreate_Ptr : TypedEventChannel.Impl.Interface_Ptr;
      MyRef : ConsumerAdmin.Ref;
   begin
      pragma Debug (O ("obtain proxypush supplier from " &
                       "typed consumeradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);

      MyCreate_Ptr := TypedEventChannel.Impl.Lookup (uses_interface);

      if MyCreate_Ptr = null then
         raise InterfaceNotSupported;
      end if;

      MyRef := ConsumerAdmin.Helper.To_Ref (Self.X.ThisRef);
      Supplier := ProxyPushSupplier.Impl.Create (MyRef);

      ProxyPushSuppliersTable.Register (To_String (uses_interface), Supplier);

      Leave (Self_Mutex);

      Servant_To_Reference (Servant (Supplier), Its_Ref);
      return Its_Ref;
   end obtain_typed_push_supplier;

   --------------------------
   -- Obtain_Pull_Supplier --
   --------------------------

   function Obtain_Pull_Supplier
     (Self : access Object) return ProxyPullSupplier.Ref
   is
      pragma Unreferenced (Self);
      Its_Ref  : ProxyPullSupplier.Ref;
   begin
      pragma Debug (O ("obtain proxy pull supplier from typed consumeradmin"));
      pragma Debug (O ("no need to get generic proxy pullsupplier "&
                       "from typed consumeradmin"));
      Ensure_Initialization;

      --  No need to implement generic Obtain_Pull_Supplier in
      --  Typed ConsumerAdmin

      raise Program_Error;
      return Its_Ref;
   end Obtain_Pull_Supplier;

   --------------------------
   -- Obtain_Push_Supplier --
   --------------------------

   function Obtain_Push_Supplier
     (Self : access Object) return ProxyPushSupplier.Ref
   is
      pragma Unreferenced (Self);
      Its_Ref  : ProxyPushSupplier.Ref;
   begin
      pragma Debug (O ("obtain proxy push supplier from typed consumeradmin"));
      pragma Debug (O ("no need to get generic proxy pushsupplier "&
                       "from typed consumeradmin"));
      Ensure_Initialization;

      --  No need to implement generic Obtain_Push_Supplier in
      --  Typed ConsumerAdmin

      raise Program_Error;
      return Its_Ref;
   end Obtain_Push_Supplier;

   ----------
   -- Post --
   ----------

   function Post
     (Self           : access Object;
      uses_interface : CosTypedEventChannelAdmin.Key) return CORBA.Object.Ref
   is
      pragma Unreferenced (Self);

      Ref : CORBA.Object.Ref;
      MyProxyPushSupplier : ProxyPushSupplier.Impl.Object_Ptr;
   begin
      Ensure_Initialization;
      pragma Debug (O ("push mutually agreed interface from " &
                       "typed consumeradmin to proxy pushsupplier"));
      Enter (Self_Mutex);

      MyProxyPushSupplier := ProxyPushSuppliersTable.Lookup
                             (To_String (uses_interface), null);

      if MyProxyPushSupplier = null then
         raise InterfaceNotSupported;
      end if;

      Leave (Self_Mutex);

      Ref := ProxyPushSupplier.Impl.Post (MyProxyPushSupplier);
      return Ref;
   end Post;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object;
      uses_interface : CosTypedEventChannelAdmin.Key) return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
   begin
      Ensure_Initialization;

      Enter (Self_Mutex);
      pragma Debug (O ("pull mutually agreed interface from " &
                       "typed consumeradmin to typed eventchannel"));

      Ref := TypedEventChannel.Impl.Pull (Self.X.Channel, uses_interface);

      Leave (Self_Mutex);

      return Ref;
   end Pull;

end CosTypedEventChannelAdmin.TypedConsumerAdmin.Impl;
