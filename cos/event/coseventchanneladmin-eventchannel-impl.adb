------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSEVENTCHANNELADMIN.EVENTCHANNEL.IMPL                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CosEventChannelAdmin.SupplierAdmin.Impl;
with CosEventChannelAdmin.ConsumerAdmin.Impl;

with CosEventChannelAdmin.EventChannel;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;

with CosEventChannelAdmin.EventChannel.Skel;
pragma Warnings (Off, CosEventChannelAdmin.EventChannel.Skel);

package body CosEventChannelAdmin.EventChannel.Impl is

   use PortableServer;
   use PolyORB.CORBA_P.Server_Tools;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("eventchannel");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Event_Channel_Record is record
      This            : Object_Ptr;
      Consumer        : ConsumerAdmin.Impl.Object_Ptr;
      Supplier        : SupplierAdmin.Impl.Object_Ptr;
   end record;

   ------------
   -- Create --
   ------------

   function Create
     return Object_Ptr
   is
      Channel : Object_Ptr;
      My_Ref  : EventChannel.Ref;

   begin
      pragma Debug (O ("create channel"));

      Channel            := new Object;
      Channel.X          := new Event_Channel_Record;
      Channel.X.This     := Channel;
      Channel.X.Consumer := ConsumerAdmin.Impl.Create (Channel);
      Channel.X.Supplier := SupplierAdmin.Impl.Create (Channel);
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
     return ConsumerAdmin.Ref
   is
      R : ConsumerAdmin.Ref;
   begin
      pragma Debug (O ("create consumer admin for channel"));
      Servant_To_Reference (Servant (Self.X.Consumer), R);

      return R;
   end For_Consumers;

   -------------------
   -- For_Suppliers --
   -------------------

   function For_Suppliers
     (Self : access Object)
     return CosEventChannelAdmin.SupplierAdmin.Ref
   is
      R : SupplierAdmin.Ref;

   begin
      pragma Debug (O ("create supplier for channel"));

      Servant_To_Reference (Servant (Self.X.Supplier), R);
      return R;
   end For_Suppliers;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : CORBA.Any) is
   begin
      ConsumerAdmin.Impl.Post (Self.X.Consumer, Data);
   end Post;

end CosEventChannelAdmin.EventChannel.Impl;
