------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            COSTYPEDEVENTCHANNELADMIN.TYPEDCONSUMERADMIN.IMPL             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosEventChannelAdmin;
with CosEventChannelAdmin.ConsumerAdmin.Helper;

with CosEventChannelAdmin.ProxyPullSupplier;
with CosEventChannelAdmin.ProxyPullSupplier.Helper;

with CosEventChannelAdmin.ProxyPushSupplier;
with CosEventChannelAdmin.ProxyPushSupplier.Helper;
with CosEventChannelAdmin.ProxyPushSupplier.Impl;

with CosTypedEventChannelAdmin;
with CosTypedEventChannelAdmin.TypedEventChannel;
with CosTypedEventChannelAdmin.TypedEventChannel.Helper;
with CosTypedEventChannelAdmin.TypedEventChannel.Impl;

with CosTypedEventChannelAdmin.TypedProxyPullSupplier;
with CosTypedEventChannelAdmin.TypedProxyPullSupplier.Helper;
with CosTypedEventChannelAdmin.TypedProxyPullSupplier.Impl;

with CosTypedEventChannelAdmin.TypedConsumerAdmin.Helper;
pragma Elaborate (CosTypedEventChannelAdmin.TypedConsumerAdmin.Helper);
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedConsumerAdmin.Helper);

with CosTypedEventChannelAdmin.TypedConsumerAdmin.Skel;
pragma Elaborate (CosTypedEventChannelAdmin.TypedConsumerAdmin.Skel);
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedConsumerAdmin.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;

with PolyORB.Log;

with PolyORB.Dynamic_Dict;

package body CosTypedEventChannelAdmin.TypedConsumerAdmin.Impl is

   use CosEventChannelAdmin;

   use CosEventChannelAdmin.ProxyPushSupplier.Impl;

   use CosTypedEventChannelAdmin.TypedEventChannel.Impl;

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedconsumeradmin");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

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
      supported_interface : in CosTypedEventChannelAdmin.Key)
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
      uses_interface : in CosTypedEventChannelAdmin.Key)
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
     (Self : access Object)
     return ProxyPullSupplier.Ref
   is
      Its_Ref  : ProxyPullSupplier.Ref;
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("obtain proxy pull supplier from typed consumeradmin"));
      pragma Debug (O ("no need to get generic proxy pullsupplier "&
                       "from typed consumeradmin"));
      Ensure_Initialization;
      --  No need to implement generic Obtain_Pull_Supplier in
      --  Typed ConsumerAdmin
      raise PolyORB.Not_Implemented;

      return Its_Ref;
   end Obtain_Pull_Supplier;

   --------------------------
   -- Obtain_Push_Supplier --
   --------------------------

   function Obtain_Push_Supplier
     (Self : access Object)
     return ProxyPushSupplier.Ref
   is
      Its_Ref  : ProxyPushSupplier.Ref;
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("obtain proxy push supplier from typed consumeradmin"));
      pragma Debug (O ("no need to get generic proxy pushsupplier "&
                       "from typed consumeradmin"));
      Ensure_Initialization;
      --  No need to implement generic Obtain_Push_Supplier in
      --  Typed ConsumerAdmin
      raise PolyORB.Not_Implemented;

      return Its_Ref;
   end Obtain_Push_Supplier;

   ----------
   -- Post --
   ----------

   function Post
     (Self : access Object;
      uses_interface : in CosTypedEventChannelAdmin.Key)
     return CORBA.Object.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
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
      uses_interface : in CosTypedEventChannelAdmin.Key)
     return CORBA.Object.Ref
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
