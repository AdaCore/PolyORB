------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            COSTYPEDEVENTCHANNELADMIN.TYPEDEVENTCHANNEL.IMPL              --
--                                                                          --
--                                 B o d y                                  --
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

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosTypedEventChannelAdmin.TypedEventChannel;

with CosTypedEventChannelAdmin.TypedSupplierAdmin;
with CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl;

with CosTypedEventChannelAdmin.TypedConsumerAdmin;
with CosTypedEventChannelAdmin.TypedConsumerAdmin.Impl;

with CosTypedEventChannelAdmin.TypedEventChannel.Helper;
pragma Elaborate (CosTypedEventChannelAdmin.TypedEventChannel.Helper);
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedEventChannel.Helper);

with CosTypedEventChannelAdmin.TypedEventChannel.Skel;
pragma Elaborate (CosTypedEventChannelAdmin.TypedEventChannel.Skel);
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedEventChannel.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Dynamic_Dict;
with PolyORB.Log;

package body CosTypedEventChannelAdmin.TypedEventChannel.Impl is

   use PortableServer;
   use PolyORB.CORBA_P.Server_Tools;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedeventchannel");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package InterfaceTable is new PolyORB.Dynamic_Dict (Interface_Ptr);

   type TypedEvent_Channel_Record is record
      This            : Object_Ptr;
      Consumer        : TypedConsumerAdmin.Impl.Object_Ptr;
      Supplier        : TypedSupplierAdmin.Impl.Object_Ptr;
   end record;

   ------------
   -- Create --
   ------------

   function Create
     return Object_Ptr
   is
      Channel : Object_Ptr;
      My_Ref  : TypedEventChannel.Ref;

   begin
      pragma Debug (O ("create typed eventchannel"));

      Channel            := new Object;
      Channel.X          := new TypedEvent_Channel_Record;
      Channel.X.This     := Channel;
      Channel.X.Consumer := TypedConsumerAdmin.Impl.Create (Channel);
      Channel.X.Supplier := TypedSupplierAdmin.Impl.Create (Channel);
      Initiate_Servant (Servant (Channel), My_Ref);
      return Channel;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      null;
   end Destroy;

   -------------------
   -- For_Consumers --
   -------------------

   function For_Consumers
     (Self : access Object)
     return TypedConsumerAdmin.Ref
   is
      R : TypedConsumerAdmin.Ref;
   begin
      pragma Debug (O ("create typedconsumer admin for typedchannel"));
      Servant_To_Reference (Servant (Self.X.Consumer), R);

      return R;
   end For_Consumers;

   -------------------
   -- For_Suppliers --
   -------------------

   function For_Suppliers
     (Self : access Object)
     return CosTypedEventChannelAdmin.TypedSupplierAdmin.Ref
   is
      R : TypedSupplierAdmin.Ref;

   begin
      pragma Debug (O ("create typedsupplier admin for typedchannel"));

      Servant_To_Reference (Servant (Self.X.Supplier), R);
      return R;
   end For_Suppliers;

   ----------
   -- Post --
   ----------

   function Post
      (Self : access Object;
      uses_interface : in  CosTypedEventChannelAdmin.Key)
      return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Push Mutually Agreed Interface from "&
                       "TypedEventChannel to TypedConsumerAdmin"));
      Ref := TypedConsumerAdmin.Impl.Post (Self.X.Consumer, uses_interface);
      return Ref;
   end Post;

   ----------
   -- Pull --
   ----------

   function Pull
      (Self : access Object;
      uses_interface : in  CosTypedEventChannelAdmin.Key)
      return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Pull Mutually Agreed Interface from "&
                       "TypedEventChannel to TypedSupplierAdmin"));
      Ref := TypedSupplierAdmin.Impl.Pull (Self.X.Supplier, uses_interface);
      return Ref;
   end Pull;

   --------------
   -- Register --
   --------------

   procedure Register (RepositoryID : in CosTypedEventChannelAdmin.Key;
                      Create_Ptr : in Interface_Ptr) is
   begin
      pragma Debug (O ("register a mutually agreed interface in "&
                       "typed eventchannel interfacetable"));
      InterfaceTable.Register (To_String (RepositoryID), Create_Ptr);
   end Register;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (RepositoryID : in CosTypedEventChannelAdmin.Key)
     return Interface_Ptr
   is
      Create_Ptr : Interface_Ptr;
   begin
      pragma Debug (O ("attempt to retreive a mutually agreed interface "&
                       "from typed eventchannel interfacetable"));
      Create_Ptr := InterfaceTable.Lookup (To_String (RepositoryID), null);
      return Create_Ptr;
   end Lookup;

end CosTypedEventChannelAdmin.TypedEventChannel.Impl;
