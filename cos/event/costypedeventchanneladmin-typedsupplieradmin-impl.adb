------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            COSTYPEDEVENTCHANNELADMIN.TYPEDSUPPLIERADMIN.IMPL             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPullConsumer.Helper;
with CosEventChannelAdmin.ProxyPullConsumer.Impl;

with CosEventChannelAdmin.ProxyPushConsumer;
with CosEventChannelAdmin.ProxyPushConsumer.Helper;

with CosEventChannelAdmin.SupplierAdmin.Helper;

with CosTypedEventChannelAdmin.TypedEventChannel;
with CosTypedEventChannelAdmin.TypedEventChannel.Helper;
with CosTypedEventChannelAdmin.TypedEventChannel.Impl;

with CosTypedEventChannelAdmin.TypedProxyPushConsumer;
with CosTypedEventChannelAdmin.TypedProxyPushConsumer.Helper;
with CosTypedEventChannelAdmin.TypedProxyPushConsumer.Impl;

with CosTypedEventChannelAdmin.TypedSupplierAdmin.Helper;
pragma Elaborate (CosTypedEventChannelAdmin.TypedSupplierAdmin.Helper);
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedSupplierAdmin.Helper);

with CosTypedEventChannelAdmin.TypedSupplierAdmin.Skel;
pragma Elaborate (CosTypedEventChannelAdmin.TypedSupplierAdmin.Skel);
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedSupplierAdmin.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Dynamic_Dict;
with PolyORB.Tasking.Mutexes;
with PolyORB.Log;

package body CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl is

   use CosEventChannelAdmin;
   use CosEventChannelAdmin.ProxyPullConsumer.Impl;

   use CosTypedEventChannelAdmin.TypedEventChannel.Impl;

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedsupplieradmin");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package ProxyPullConsumersTable is
      new PolyORB.Dynamic_Dict (ProxyPullConsumer.Impl.Object_Ptr);

   type TypedSupplier_Admin_Record is record
     This    : Object_Ptr;
     ThisRef : TypedSupplierAdmin.Ref;
     Channel : TypedEventChannel.Impl.Object_Ptr;
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

   ------------
   -- Create --
   ------------

   function Create (Channel : TypedEventChannel.Impl.Object_Ptr)
     return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : TypedSupplierAdmin.Ref;

   begin
      pragma Debug (O ("create typedsupplier admin"));

      Supplier           := new Object;
      Supplier.X         := new TypedSupplier_Admin_Record;
      Supplier.X.This    := Supplier;
      Supplier.X.Channel := Channel;

      Initiate_Servant (Servant (Supplier), My_Ref);
      Supplier.X.ThisRef := My_Ref;
      return Supplier;
   end Create;

   --------------------------
   -- Obtain_Typed_Push_Consumer --
   --------------------------

   function obtain_typed_push_consumer
     (Self : access Object;
      supported_interface : in CosTypedEventChannelAdmin.Key)
     return TypedProxyPushConsumer.Ref
   is
      Its_Ref  : TypedProxyPushConsumer.Ref;
      MyConsumer   : TypedProxyPushConsumer.Impl.Object_Ptr;
      MyCreate_Ptr : TypedEventChannel.Impl.Interface_Ptr;
   begin
      pragma Debug (O ("obtain typed proxypushconsumer from "&
                       "typed supplieradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);

      MyCreate_Ptr := TypedEventChannel.Impl.Lookup (supported_interface);
      if MyCreate_Ptr = null then
         raise InterfaceNotSupported;
      end if;

      MyConsumer := TypedProxyPushConsumer.Impl.Create (Self.X.This,
                                                        supported_interface);

      Leave (Self_Mutex);

      Servant_To_Reference (Servant (MyConsumer), Its_Ref);
      return Its_Ref;
   end obtain_typed_push_consumer;

   --------------------------------
   -- Obtain_Typed_Pull_Consumer --
   --------------------------------
   function obtain_typed_pull_consumer
     (Self : access Object;
      uses_interface : in CosTypedEventChannelAdmin.Key)
     return ProxyPullConsumer.Ref
   is
      Consumer : ProxyPullConsumer.Impl.Object_Ptr;
      Its_Ref  : ProxyPullConsumer.Ref;
      MyRef    : SupplierAdmin.Ref;
      MyCreate_Ptr : TypedEventChannel.Impl.Interface_Ptr;
   begin
      pragma Debug (O ("obtain proxypullconsumer from typed supplieradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);

      MyCreate_Ptr := TypedEventChannel.Impl.Lookup (uses_interface);
      if MyCreate_Ptr = null then
         raise InterfaceNotSupported;
      end if;

      MyRef := SupplierAdmin.Helper.To_Ref (Self.X.ThisRef);
      Consumer := ProxyPullConsumer.Impl.Create (MyRef);
      ProxyPullConsumersTable.Register (To_String (uses_interface), Consumer);

      Leave (Self_Mutex);

      Servant_To_Reference (Servant (Consumer), Its_Ref);
      return Its_Ref;
   end obtain_typed_pull_consumer;

   --------------------------
   -- Obtain_Pull_Consumer --
   --------------------------

   function Obtain_Pull_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullConsumer.Ref
   is
      Its_Ref  : ProxyPullConsumer.Ref;
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("obtain proxy pull consumer from typed supplieradmin"));
      pragma Debug (O ("No need to get generic proxy pullconsumer "&
                       "from typed supplieradmin"));
      Ensure_Initialization;
      --  No need to implement generic Obtain_Pull_Consumer in
      --  typed supplieradmin
      raise Program_Error;

      return Its_Ref;
   end Obtain_Pull_Consumer;

   --------------------------
   -- Obtain_Push_Consumer --
   --------------------------

   function Obtain_Push_Consumer
     (Self : access Object)
     return ProxyPushConsumer.Ref
   is
      Its_Ref  : ProxyPushConsumer.Ref;
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("obtain proxy push consumer from typed supplieradmin"));
      pragma Debug (O ("No need to get generic proxy pushconsumer "&
                       "from typed supplieradmin"));
      Ensure_Initialization;
      --  No need to implement generic Obtain_Push_Consumer in
      --  typed supplieradmin
      raise Program_Error;

      return Its_Ref;
   end Obtain_Push_Consumer;

   ----------
   -- Post --
   ----------

   function Post
     (Self : access Object;
      uses_interface : in CosTypedEventChannelAdmin.Key)
     return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("push mutually agreed interface from " &
                       "typed supplieradmin to typed eventchannel"));
      Ensure_Initialization;

      Enter (Self_Mutex);

      Ref := TypedEventChannel.Impl.Post (Self.X.Channel, uses_interface);

      Leave (Self_Mutex);

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
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      Ref : CORBA.Object.Ref;
      MyProxyPullConsumer : ProxyPullConsumer.Impl.Object_Ptr;
   begin
      pragma Debug (O ("pull mutually agreed interface from " &
                       "typed supplieradmin to proxy pullconsumer"));
      Ensure_Initialization;

      Enter (Self_Mutex);

      MyProxyPullConsumer := ProxyPullConsumersTable.Lookup
                             (To_String (uses_interface), null);

      if MyProxyPullConsumer = null then
         raise InterfaceNotSupported;
      end if;

      Leave (Self_Mutex);

      Ref := ProxyPullConsumer.Impl.Pull (MyProxyPullConsumer);
      return Ref;
   end Pull;


end CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl;
