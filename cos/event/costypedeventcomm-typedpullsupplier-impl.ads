------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSTYPEDEVENTCOMM.TYPEDPULLSUPPLIER.IMPL                  --
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

with PortableServer;

with CosTypedEventChannelAdmin;
with CosTypedEventChannelAdmin.TypedEventChannel.Impl;

package CosTypedEventComm.TypedPullSupplier.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function Get_Typed_Supplier (Self : access Object) return CORBA.Object.Ref;

   --  Inherited IDL operations from CosEventComm::PullSupplier

   procedure Disconnect_Pull_Supplier
     (Self : access Object);
   --  Call by proxy to disconnect

   function Pull
     (Self : access Object)
     return CORBA.Any;
   --  Call by proxy to pull an event
   --  No need to implement it in this case

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out    CORBA.Boolean;
      Returns   : out    CORBA.Any);
   --  Call by proxy to try to pull an event
   --  No need to implement it in this case

   ------------------------
   -- PolyORB specific --
   ------------------------

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   procedure SetInterface_Ptr
      (Self  : access Object;
       I_Ptr : CosTypedEventChannelAdmin.TypedEventChannel.Impl.Interface_Ptr);
   --  Appopriately set the supported interface pointer

private

   type Typed_Pull_Supplier_Record;
   type Typed_Pull_Supplier_Access is access Typed_Pull_Supplier_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Typed_Pull_Supplier_Access;
   end record;

end CosTypedEventComm.TypedPullSupplier.Impl;
