------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
-- C O S E V E N T C H A N N E L A D M I N . E V E N T C H A N N E L . I M P L  --
--                                                                          --
--                                 B o d y                                  --
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
with CosEventChannelAdmin.SupplierAdmin.Impl;

with CosEventChannelAdmin.ConsumerAdmin;
with CosEventChannelAdmin.ConsumerAdmin.Impl;

with CosEventChannelAdmin.EventChannel.Helper;
with CosEventChannelAdmin.EventChannel.Skel;

with Broca.Server_Tools; use  Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer; use PortableServer;

with CORBA.Impl;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.EventChannel.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("eventchannel");
   procedure O is new Broca.Debug.Output (Flag);

   -------------
   -- Channel --
   -------------

   type Event_Channel_Record is
      record
         This     : Object_Ptr;
         Consumer : ConsumerAdmin.Impl.Object_Ptr;
         Supplier : SupplierAdmin.Impl.Object_Ptr;
      end record;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
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

   procedure Destroy
     (Self : access Object) is
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
      Data : in CORBA.Any) is
   begin
      ConsumerAdmin.Impl.Post (Self.X.Consumer, Data);
   end Post;

end CosEventChannelAdmin.EventChannel.Impl;
