------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSTYPEDEVENTCOMM.TYPEDPUSHCONSUMER.IMPL                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with CosTypedEventChannelAdmin;
with CosTypedEventChannelAdmin.TypedEventChannel.Impl;

with PortableServer;

package CosTypedEventComm.TypedPushConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function Get_Typed_Consumer (Self : access Object) return CORBA.Object.Ref;

   --  Inherited IDL operations from CosEventComm::PushConsumer

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any);
   --  Call by proxy to push an event
   --  No need to implement it in this case

   procedure Disconnect_Push_Consumer
     (Self : access Object);
   --  Call by proxy to disconnect

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   procedure SetInterface_Ptr
      (Self  : access Object;
       I_Ptr : CosTypedEventChannelAdmin.TypedEventChannel.Impl.Interface_Ptr);
   --  Appopriately set the supported Interface Pointer

private

   type Typed_Push_Consumer_Record;
   type Typed_Push_Consumer_Access is access Typed_Push_Consumer_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Typed_Push_Consumer_Access;
   end record;

end CosTypedEventComm.TypedPushConsumer.Impl;
