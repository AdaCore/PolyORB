------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         COSTYPEDEVENTCHANNELADMIN.TYPEDPROXYPUSHCONSUMER.IMPL            --
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
