------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
-- C O S E V E N T C H A N N E L A D M I N . C O N S U M E R A D M I N . I M P L  --
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

with CosEventChannelAdmin.ProxyPullSupplier;
with CosEventChannelAdmin.ProxyPullSupplier.Helper;
with CosEventChannelAdmin.ProxyPullSupplier.Impl;

with CosEventChannelAdmin.ProxyPushSupplier;
with CosEventChannelAdmin.ProxyPushSupplier.Helper;
with CosEventChannelAdmin.ProxyPushSupplier.Impl;

with CosEventChannelAdmin.ConsumerAdmin.Helper;
with CosEventChannelAdmin.ConsumerAdmin.Skel;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with CORBA.Sequences.Unbounded;

with CORBA.Impl;

with PortableServer; use PortableServer;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.ConsumerAdmin.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("consumeradmin");
   procedure O is new Broca.Debug.Output (Flag);

   package PushSuppliers is
      new CORBA.Sequences.Unbounded (ProxyPushSupplier.Impl.Object_Ptr);

   package PullSuppliers is
      new CORBA.Sequences.Unbounded (ProxyPullSupplier.Impl.Object_Ptr);

   type Consumer_Admin_Record is
      record
         This    : Object_Ptr;
         Channel : EventChannel.Impl.Object_Ptr;
         Pushs   : PushSuppliers.Sequence;
         Pulls   : PullSuppliers.Sequence;
      end record;

   ------------
   -- Create --
   ------------

   function Create (Channel : EventChannel.Impl.Object_Ptr)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : ConsumerAdmin.Ref;

   begin
      pragma Debug (O ("create consumer admin"));

      Consumer        := new Object;
      Consumer.X      := new Consumer_Admin_Record;
      Consumer.X.This    := Consumer;
      Consumer.X.Channel := Channel;
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   --------------------------
   -- Obtain_Pull_Supplier --
   --------------------------

   function Obtain_Pull_Supplier
     (Self : access Object)
     return ProxyPullSupplier.Ref
   is
      Supplier : ProxyPullSupplier.Impl.Object_Ptr;
      Its_Ref  : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("obtain proxy pull supplier from consumer admin"));

      Enter_Critical_Section;
      Supplier := ProxyPullSupplier.Impl.Create (Self.X.This);
      PullSuppliers.Append (Self.X.Pulls, Supplier);
      Leave_Critical_Section;
      Servant_To_Reference (Servant (Supplier), Its_Ref);
      return Its_Ref;
   end Obtain_Pull_Supplier;

   --------------------------
   -- Obtain_Push_Supplier --
   --------------------------

   function Obtain_Push_Supplier
     (Self : access Object)
     return ProxyPushSupplier.Ref
   is
      Supplier : ProxyPushSupplier.Impl.Object_Ptr;
      Its_Ref  : ProxyPushSupplier.Ref;

   begin
      pragma Debug (O ("obtain proxy push supplier from consumer admin"));

      Enter_Critical_Section;
      Supplier := ProxyPushSupplier.Impl.Create (Self.X.This);
      PushSuppliers.Append (Self.X.Pushs, Supplier);
      Leave_Critical_Section;
      Servant_To_Reference (Servant (Supplier), Its_Ref);
      return Its_Ref;
   end Obtain_Push_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      Enter_Critical_Section;
      declare
         Pulls : PullSuppliers.Element_Array
           := PullSuppliers.To_Element_Array (Self.X.Pulls);
         Pushs : PushSuppliers.Element_Array
           := PushSuppliers.To_Element_Array (Self.X.Pushs);
      begin
         Leave_Critical_Section;
         pragma Debug (O ("post new data to proxy pull suppliers"));

         for I in Pulls'Range loop
            ProxyPullSupplier.Impl.Post (Pulls (I), Data);
         end loop;
         pragma Debug (O ("post new data to proxy push suppliers"));
         for I in Pushs'Range loop
            ProxyPushSupplier.Impl.Post (Pushs (I), Data);
         end loop;
      end;
   end Post;

end CosEventChannelAdmin.ConsumerAdmin.Impl;
