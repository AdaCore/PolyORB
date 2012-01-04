------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSTYPEDEVENTCOMM.TYPEDPUSHCONSUMER.IMPL                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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
      Data : CORBA.Any);
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
