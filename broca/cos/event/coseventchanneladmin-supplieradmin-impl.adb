------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
-- C O S E V E N T C H A N N E L A D M I N . S U P P L I E R A D M I N . I M P L  --
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

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.EventChannel.Impl;

with CosEventChannelAdmin.ProxyPullConsumer;
with CosEventChannelAdmin.ProxyPullConsumer.Impl;

with CosEventChannelAdmin.ProxyPushConsumer;
with CosEventChannelAdmin.ProxyPushConsumer.Impl;

with CosEventChannelAdmin.SupplierAdmin.Helper;
with CosEventChannelAdmin.SupplierAdmin.Skel;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Impl;

with CORBA.Sequences.Unbounded;

with PortableServer; use PortableServer;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.SupplierAdmin.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("consumeradmin");
   procedure O is new Broca.Debug.Output (Flag);

   package PullConsumers is
      new CORBA.Sequences.Unbounded (ProxyPullConsumer.Impl.Object_Ptr);

   package PushConsumers is
      new CORBA.Sequences.Unbounded (ProxyPushConsumer.Impl.Object_Ptr);

   type Supplier_Admin_Record is
      record
         This    : Object_Ptr;
         Channel : EventChannel.Impl.Object_Ptr;
         Pushs   : PushConsumers.Sequence;
         Pulls   : PullConsumers.Sequence;
      end record;

   ------------
   -- Create --
   ------------

   function Create (Channel : EventChannel.Impl.Object_Ptr)
     return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : SupplierAdmin.Ref;

   begin
      pragma Debug (O ("create supplier admin"));

      Supplier        := new Object;
      Supplier.X      := new Supplier_Admin_Record;
      Supplier.X.This    := Supplier;
      Supplier.X.Channel := Channel;
      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   --------------------------
   -- Obtain_Pull_Consumer --
   --------------------------

   function Obtain_Pull_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullConsumer.Ref
   is
      Consumer : ProxyPullConsumer.Impl.Object_Ptr;
      Its_Ref  : ProxyPullConsumer.Ref;

   begin
      pragma Debug (O ("obtain proxy pull consumer from supplier admin"));

      Enter_Critical_Section;
      Consumer := ProxyPullConsumer.Impl.Create (Self.X.This);
      PullConsumers.Append (Self.X.Pulls, Consumer);
      Leave_Critical_Section;
      Servant_To_Reference (Servant (Consumer), Its_Ref);
      return Its_Ref;
   end Obtain_Pull_Consumer;

   --------------------------
   -- Obtain_Push_Consumer --
   --------------------------

   function Obtain_Push_Consumer
     (Self : access Object)
     return ProxyPushConsumer.Ref
   is
      Consumer : ProxyPushConsumer.Impl.Object_Ptr;
      Its_Ref  : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("obtain proxy push consumer from supplier admin"));

      Enter_Critical_Section;
      Consumer := ProxyPushConsumer.Impl.Create (Self.X.This);
      PushConsumers.Append (Self.X.Pushs, Consumer);
      Leave_Critical_Section;
      Servant_To_Reference (Servant (Consumer), Its_Ref);
      return Its_Ref;
   end Obtain_Push_Consumer;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      pragma Debug (O ("post new data from supplier admin to channel"));

      EventChannel.Impl.Post (Self.X.Channel, Data);
   end Post;

end CosEventChannelAdmin.SupplierAdmin.Impl;
