------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSEVENTCHANNELADMIN.SUPPLIERADMIN.IMPL                  --
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

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPushConsumer;
with CosEventChannelAdmin.EventChannel.Impl;
with PortableServer;

package CosEventChannelAdmin.SupplierAdmin.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function Obtain_Push_Consumer
     (Self : access Object)
     return ProxyPushConsumer.Ref;

   function Obtain_Pull_Consumer
     (Self : access Object)
     return ProxyPullConsumer.Ref;

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create
     (Channel : CosEventChannelAdmin.EventChannel.Impl.Object_Ptr)
     return Object_Ptr;

   procedure Post
     (Self : access Object;
      Data : CORBA.Any);

private

   type Supplier_Admin_Record;
   type Supplier_Admin_Access is access all Supplier_Admin_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Supplier_Admin_Access;
   end record;

end CosEventChannelAdmin.SupplierAdmin.Impl;
