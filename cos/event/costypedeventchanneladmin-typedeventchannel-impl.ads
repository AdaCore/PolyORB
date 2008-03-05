------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            COSTYPEDEVENTCHANNELADMIN.TYPEDEVENTCHANNEL.IMPL              --
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
with CORBA.Impl;
with CORBA.Object;

with CosTypedEventChannelAdmin.TypedSupplierAdmin;
with CosTypedEventChannelAdmin.TypedConsumerAdmin;

with PortableServer;

package CosTypedEventChannelAdmin.TypedEventChannel.Impl is

   type Object is new PortableServer.Servant_Base with private;
   type Object_Ptr is access all Object'Class;

   function For_Consumers
     (Self : access Object)
     return CosTypedEventChannelAdmin.TypedConsumerAdmin.Ref;

   function For_Suppliers
     (Self : access Object)
     return CosTypedEventChannelAdmin.TypedSupplierAdmin.Ref;

   procedure Destroy
     (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;

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

   type Interface_Ptr is access function return CORBA.Impl.Object_Ptr;

   procedure Register
     (RepositoryID : CosTypedEventChannelAdmin.Key;
     Create_Ptr : Interface_Ptr);
   --  Register a couple of Repository ID and
   --  Pointer_to_Create function in a HashTable

   function Lookup
     (RepositoryID : CosTypedEventChannelAdmin.Key)
     return Interface_Ptr;
   --  Lookup an entry in the HashTable

private

   type TypedEvent_Channel_Record;
   type TypedEvent_Channel_Access is access TypedEvent_Channel_Record;

   type Object is new PortableServer.Servant_Base with record
      X : TypedEvent_Channel_Access;
   end record;

end CosTypedEventChannelAdmin.TypedEventChannel.Impl;
