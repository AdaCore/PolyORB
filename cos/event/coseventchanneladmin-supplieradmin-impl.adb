------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSEVENTCHANNELADMIN.SUPPLIERADMIN.IMPL                  --
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

with CosEventChannelAdmin.ProxyPullConsumer.Impl;
with CosEventChannelAdmin.ProxyPushConsumer.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosEventChannelAdmin.SupplierAdmin.Skel;
pragma Warnings (Off, CosEventChannelAdmin.SupplierAdmin.Skel);

package body CosEventChannelAdmin.SupplierAdmin.Impl is

   use CosEventChannelAdmin;
   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("supplieradmin");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package PullConsumers is
      new CORBA.Sequences.Unbounded (ProxyPullConsumer.Impl.Object_Ptr);

   package PushConsumers is
      new CORBA.Sequences.Unbounded (ProxyPushConsumer.Impl.Object_Ptr);

   type Supplier_Admin_Record is record
      This    : Object_Ptr;
      ThisRef : SupplierAdmin.Ref;
      Channel : EventChannel.Impl.Object_Ptr;
      Pushs   : PushConsumers.Sequence;
      Pulls   : PullConsumers.Sequence;
   end record;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

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
      Supplier : Object_Ptr;
      My_Ref   : SupplierAdmin.Ref;

   begin
      pragma Debug (O ("create supplier admin"));

      Supplier           := new Object;
      Supplier.X         := new Supplier_Admin_Record;
      Supplier.X.This    := Supplier;
      Supplier.X.Channel := Channel;

      Initiate_Servant (Servant (Supplier), My_Ref);
      Supplier.X.ThisRef := My_Ref;
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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Consumer := ProxyPullConsumer.Impl.Create (Self.X.ThisRef);
      PullConsumers.Append (Self.X.Pulls, Consumer);
      Leave (Self_Mutex);

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Consumer := ProxyPushConsumer.Impl.Create (Self.X.This);
      PushConsumers.Append (Self.X.Pushs, Consumer);
      Leave (Self_Mutex);

      Servant_To_Reference (Servant (Consumer), Its_Ref);

      return Its_Ref;
   end Obtain_Push_Consumer;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : CORBA.Any) is
   begin
      pragma Debug (O ("post new data from supplier admin to channel"));

      EventChannel.Impl.Post (Self.X.Channel, Data);
   end Post;

end CosEventChannelAdmin.SupplierAdmin.Impl;
