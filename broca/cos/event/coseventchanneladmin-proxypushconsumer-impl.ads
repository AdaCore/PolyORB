------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
-- C O S E V E N T C H A N N E L A D M I N . P R O X Y P U S H C O N S U M E R . I M P L  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with PortableServer;

with CosEventChannelAdmin.SupplierAdmin.Impl;

package CosEventChannelAdmin.ProxyPushConsumer.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

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

   type Object is
     new PortableServer.Servant_Base with
      record
         X : Proxy_Push_Consumer_Access;
      end record;

end CosEventChannelAdmin.ProxyPushConsumer.Impl;
