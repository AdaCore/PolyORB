------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSEVENTCHANNELADMIN.CONSUMERADMIN.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with CORBA.Sequences.Unbounded;

with CosEventChannelAdmin;
with CosEventChannelAdmin.ProxyPullSupplier.Impl;
with CosEventChannelAdmin.ProxyPushSupplier.Impl;

with CosEventChannelAdmin.ConsumerAdmin.Skel;
pragma Warnings (Off, CosEventChannelAdmin.ConsumerAdmin.Skel);

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.CORBA_P.Server_Tools;

package body CosEventChannelAdmin.ConsumerAdmin.Impl is

   use CosEventChannelAdmin;
   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("consumeradmin");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package PushSuppliers is
      new CORBA.Sequences.Unbounded (ProxyPushSupplier.Impl.Object_Ptr);

   package PullSuppliers is
      new CORBA.Sequences.Unbounded (ProxyPullSupplier.Impl.Object_Ptr);

   type Consumer_Admin_Record is record
      This    : Object_Ptr;
      ThisRef : ConsumerAdmin.Ref;
      Channel : EventChannel.Impl.Object_Ptr;
      Pushs   : PushSuppliers.Sequence;
      Pulls   : PullSuppliers.Sequence;
   end record;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized.

   T_Initialized : Boolean := False;
   Self_Mutex : Mutex_Access;

   procedure Ensure_Initialization is
   begin
      if not T_Initialized then
         Create (Self_Mutex);
         T_Initialized := True;
      end if;
   end Ensure_Initialization;

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

      Consumer           := new Object;
      Consumer.X         := new Consumer_Admin_Record;
      Consumer.X.This    := Consumer;
      Consumer.X.Channel := Channel;
      Initiate_Servant (Servant (Consumer), My_Ref);
      Consumer.X.ThisRef := My_Ref;
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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Supplier := ProxyPullSupplier.Impl.Create (Self.X.This);
      PullSuppliers.Append (Self.X.Pulls, Supplier);
      Leave (Self_Mutex);

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Supplier := ProxyPushSupplier.Impl.Create (Self.X.ThisRef);
      PushSuppliers.Append (Self.X.Pushs, Supplier);
      Leave (Self_Mutex);

      Servant_To_Reference (Servant (Supplier), Its_Ref);

      return Its_Ref;
   end Obtain_Push_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : CORBA.Any) is
   begin
      Ensure_Initialization;

      Enter (Self_Mutex);
      declare
         Pulls : constant PullSuppliers.Element_Array
           := PullSuppliers.To_Element_Array (Self.X.Pulls);
         Pushs : constant PushSuppliers.Element_Array
           := PushSuppliers.To_Element_Array (Self.X.Pushs);
      begin
         Leave (Self_Mutex);

         pragma Debug (O ("post new data to proxy pull suppliers"));

         for J in Pulls'Range loop
            ProxyPullSupplier.Impl.Post (Pulls (J), Data);
         end loop;

         pragma Debug (O ("post new data to proxy push suppliers"));
         for J in Pushs'Range loop
            ProxyPushSupplier.Impl.Post (Pushs (J), Data);
         end loop;
      end;
   end Post;

end CosEventChannelAdmin.ConsumerAdmin.Impl;
