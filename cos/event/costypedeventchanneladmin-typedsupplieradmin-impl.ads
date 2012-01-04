------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            COSTYPEDEVENTCHANNELADMIN.TYPEDSUPPLIERADMIN.IMPL             --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with CORBA.Object;

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPushConsumer;

with CosTypedEventChannelAdmin;
with CosTypedEventChannelAdmin.TypedEventChannel.Impl;
with CosTypedEventChannelAdmin.TypedProxyPushConsumer;

with PortableServer;

package CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function obtain_typed_push_consumer
     (Self : access Object;
      supported_interface : CosTypedEventChannelAdmin.Key)
     return TypedProxyPushConsumer.Ref;

   function obtain_typed_pull_consumer
     (Self : access Object;
      uses_interface : CosTypedEventChannelAdmin.Key)
      return CosEventChannelAdmin.ProxyPullConsumer.Ref;

   --  Iherited IDL operations from
   --  CosEventchannelAdmin::SupplierAdmin

   function Obtain_Push_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPushConsumer.Ref;
   --  Return ProxyPushConsumer
   --  No need to implement it in this case

   function Obtain_Pull_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullConsumer.Ref;
   --  Return ProxyPullConsumer
   --  No need to implement it in this case

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create
     (Channel : CosTypedEventChannelAdmin.TypedEventChannel.Impl.Object_Ptr)
     return Object_Ptr;

   function Post
      (Self : access Object;
       uses_interface : CosTypedEventChannelAdmin.Key)
      return CORBA.Object.Ref;
   --  Get mutually agreed interface from Typed PushConsumers

   function Pull
      (Self : access Object;
       uses_interface : CosTypedEventChannelAdmin.Key)
      return CORBA.Object.Ref;
   --  Get mutually agreed interface from Typed PullSuppliers

private

   type TypedSupplier_Admin_Record;
   type TypedSupplier_Admin_Access is access all TypedSupplier_Admin_Record;

   type Object is new PortableServer.Servant_Base with record
      X : TypedSupplier_Admin_Access;
   end record;

end CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl;
