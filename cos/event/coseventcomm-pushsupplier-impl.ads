------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S E V E N T C O M M . P U S H S U P P L I E R . I M P L        --
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

with PortableServer;

with CosEventChannelAdmin.ProxyPushConsumer;

package CosEventComm.PushSupplier.Impl is

   --  This implementation is supposed to be application
   --  dependent. This is an example used to test the event service.

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure disconnect_push_supplier
     (Self : access Object);

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Connect_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : CosEventChannelAdmin.ProxyPushConsumer.Ref);

   function Create return Object_Ptr;

   procedure Push
     (Self : access Object;
      Data : CORBA.Any);

private

   type Push_Supplier_Record;
   type Push_Supplier_Access is access Push_Supplier_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Push_Supplier_Access;
   end record;

end CosEventComm.PushSupplier.Impl;
