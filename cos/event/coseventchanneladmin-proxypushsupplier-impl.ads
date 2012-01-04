------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPUSHSUPPLIER.IMPL                --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with CORBA.Object;

with PortableServer;

with CosEventChannelAdmin.ConsumerAdmin;

package CosEventChannelAdmin.ProxyPushSupplier.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Connect_Push_Consumer
     (Self          : access Object;
      Push_Consumer : CosEventComm.PushConsumer.Ref);

   procedure Disconnect_Push_Supplier
     (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   procedure Post (Self : access Object;
                   Data : CORBA.Any);

   function Post (Self : access Object) return CORBA.Object.Ref;
   --  Get mutually agreed interface from Typed PushConsumers

   function Create
     (Admin : CosEventChannelAdmin.ConsumerAdmin.Ref)
     return Object_Ptr;

private

   type Proxy_Push_Supplier_Record;
   type Proxy_Push_Supplier_Access is access Proxy_Push_Supplier_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Proxy_Push_Supplier_Access;
   end record;

end CosEventChannelAdmin.ProxyPushSupplier.Impl;
