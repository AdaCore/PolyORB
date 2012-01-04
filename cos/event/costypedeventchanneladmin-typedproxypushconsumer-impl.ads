------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          COSTYPEDEVENTCHANNELADMIN.TYPEDPROXYPUSHCONSUMER.IMPL           --
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

with CosEventComm.PushSupplier;

with CosTypedEventChannelAdmin;
with CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl;

with PortableServer;

package CosTypedEventChannelAdmin.TypedProxyPushConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  Inherited IDL operations from
   --  CosEventchannelAdmin::ProxyPushConsumer

   procedure Connect_Push_Supplier
     (Self          : access Object;
      Push_Supplier : CosEventComm.PushSupplier.Ref);

   procedure Push
     (Self : access Object;
      Data : CORBA.Any);
   --  Call by supplier to push an event
   --  No need to implement it in this case

   procedure Disconnect_Push_Consumer
     (Self : access Object);

   --  Inherited IDL operations from
   --  CosTypedEventComm::TypedPushConsumer

   function Get_Typed_Consumer (Self : access Object) return CORBA.Object.Ref;

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create
     (Admin : CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl.Object_Ptr;
      supported_interface : CosTypedEventChannelAdmin.Key)
     return Object_Ptr;

private

   type TypedProxy_Push_Consumer_Record;

   type TypedProxy_Push_Consumer_Access is access
   all TypedProxy_Push_Consumer_Record;

   type Object is new PortableServer.Servant_Base with record
      X : TypedProxy_Push_Consumer_Access;
   end record;

end CosTypedEventChannelAdmin.TypedProxyPushConsumer.Impl;
