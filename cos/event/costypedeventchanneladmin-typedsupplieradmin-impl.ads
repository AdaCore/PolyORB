------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            COSTYPEDEVENTCHANNELADMIN.TYPEDSUPPLIERADMIN.IMPL             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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
