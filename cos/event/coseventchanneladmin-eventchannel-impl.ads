------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
-- C O S E V E N T C H A N N E L A D M I N.E V E N T C H A N N E L.I M P L  --
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

with CosEventChannelAdmin.SupplierAdmin;
with CosEventChannelAdmin.ConsumerAdmin;

with PortableServer;

package CosEventChannelAdmin.EventChannel.Impl is

   type Object is new PortableServer.Servant_Base with private;
   type Object_Ptr is access all Object'Class;

   function For_Consumers
     (Self : access Object)
     return CosEventChannelAdmin.ConsumerAdmin.Ref;

   function For_Suppliers
     (Self : access Object)
     return CosEventChannelAdmin.SupplierAdmin.Ref;

   procedure Destroy
     (Self : access Object);

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Post
     (Self : access Object;
      Data : in     CORBA.Any);

   function Create return Object_Ptr;

private

   type Event_Channel_Record;
   type Event_Channel_Access is access Event_Channel_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Event_Channel_Access;
   end record;

end CosEventChannelAdmin.EventChannel.Impl;
