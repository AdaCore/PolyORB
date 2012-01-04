------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPUSHCONSUMER.IMPL                --
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

with PortableServer;

with CosEventChannelAdmin.SupplierAdmin.Impl;

package CosEventChannelAdmin.ProxyPushConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Connect_Push_Supplier
     (Self          : access Object;
      Push_Supplier : CosEventComm.PushSupplier.Ref);

   procedure Push
     (Self : access Object;
      Data : CORBA.Any);

   procedure Disconnect_Push_Consumer
     (Self : access Object);

   ------------------------
   -- AdaBroker specific --
   ------------------------

   function Create
     (Admin : CosEventChannelAdmin.SupplierAdmin.Impl.Object_Ptr)
     return Object_Ptr;

private

   type Proxy_Push_Consumer_Record;
   type Proxy_Push_Consumer_Access is access all Proxy_Push_Consumer_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Proxy_Push_Consumer_Access;
   end record;

end CosEventChannelAdmin.ProxyPushConsumer.Impl;
