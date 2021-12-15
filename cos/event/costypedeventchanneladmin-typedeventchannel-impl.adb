------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            COSTYPEDEVENTCHANNELADMIN.TYPEDEVENTCHANNEL.IMPL              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2021, Free Software Foundation, Inc.          --
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

with CosTypedEventChannelAdmin.TypedSupplierAdmin.Impl;
with CosTypedEventChannelAdmin.TypedConsumerAdmin.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Dynamic_Dict;
with PolyORB.Log;

with CosTypedEventChannelAdmin.TypedEventChannel.Skel;
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedEventChannel.Skel);

package body CosTypedEventChannelAdmin.TypedEventChannel.Impl is

   use PortableServer;
   use PolyORB.CORBA_P.Server_Tools;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedeventchannel");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

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

   procedure Destroy (Self : access Object) is
      pragma Unreferenced (Self);
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
      uses_interface : CosTypedEventChannelAdmin.Key)
      return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Push Mutually Agreed Interface from " &
                       "TypedEventChannel to TypedConsumerAdmin"));
      Ref := TypedConsumerAdmin.Impl.Post (Self.X.Consumer, uses_interface);
      return Ref;
   end Post;

   ----------
   -- Pull --
   ----------

   function Pull
      (Self : access Object;
      uses_interface : CosTypedEventChannelAdmin.Key)
      return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Pull Mutually Agreed Interface from " &
                       "TypedEventChannel to TypedSupplierAdmin"));
      Ref := TypedSupplierAdmin.Impl.Pull (Self.X.Supplier, uses_interface);
      return Ref;
   end Pull;

   --------------
   -- Register --
   --------------

   procedure Register (RepositoryID : CosTypedEventChannelAdmin.Key;
                      Create_Ptr : Interface_Ptr) is
   begin
      pragma Debug (O ("register a mutually agreed interface in " &
                       "typed eventchannel interfacetable"));
      InterfaceTable.Register (To_String (RepositoryID), Create_Ptr);
   end Register;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (RepositoryID : CosTypedEventChannelAdmin.Key)
     return Interface_Ptr
   is
      Create_Ptr : Interface_Ptr;
   begin
      pragma Debug (O ("attempt to retreive a mutually agreed interface " &
                       "from typed eventchannel interfacetable"));
      Create_Ptr := InterfaceTable.Lookup (To_String (RepositoryID), null);
      return Create_Ptr;
   end Lookup;

end CosTypedEventChannelAdmin.TypedEventChannel.Impl;
