------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSEVENTCHANNELADMIN.PROXYPUSHCONSUMER.IMPL                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

with PortableServer;

with CosEventChannelAdmin.SupplierAdmin.Impl;

package CosEventChannelAdmin.ProxyPushConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Connect_Push_Supplier
     (Self          : access Object;
      Push_Supplier : in CosEventComm.PushSupplier.Ref);

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any);

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
