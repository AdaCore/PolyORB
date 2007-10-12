------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSTYPEDEVENTCOMM.TYPEDPULLSUPPLIER.IMPL                  --
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
