------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          COSTYPEDEVENTCHANNELADMIN.TYPEDPROXYPULLSUPPLIER.IMPL           --
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

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosTypedEventChannelAdmin.TypedProxyPullSupplier.Skel;
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedProxyPullSupplier.Skel);

package body CosTypedEventChannelAdmin.TypedProxyPullSupplier.Impl is

   use CosEventComm;
   use CosEventChannelAdmin;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PortableServer;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedproxypullsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type TypedProxy_Pull_Supplier_Record is record
      This  : Object_Ptr;
      Peer  : PullConsumer.Ref;
      Admin : TypedConsumerAdmin.Impl.Object_Ptr;
      M     : Mutex_Access;

      supported_interface : CosTypedEventChannelAdmin.Key;
   end record;

   ---------------------------
   -- Connect_Pull_Consumer --
   ---------------------------

   procedure Connect_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : PullConsumer.Ref) is
   begin
      pragma Debug (O ("connect pull consumer to typed proxy pull supplier"));

      Enter (Self.X.M);

      if not PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Pull_Consumer;
      Leave (Self.X.M);
   end Connect_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create
     (Admin : TypedConsumerAdmin.Impl.Object_Ptr;
      supported_interface : CosTypedEventChannelAdmin.Key)
     return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("create typedproxy pull supplier"));

      Supplier         := new Object;
      Supplier.X       := new TypedProxy_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;
      Supplier.X.supported_interface := supported_interface;
      Create (Supplier.X.M);

      Initiate_Servant (Servant (Supplier), My_Ref);

      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier
     (Self : access Object)
   is
      Peer    : PullConsumer.Ref;
      Nil_Ref : PullConsumer.Ref;

   begin
      pragma Debug (O ("disconnect typedproxy pull supplier"));

      Enter (Self.X.M);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);

      if not PullConsumer.Is_Nil (Peer) then
         PullConsumer.disconnect_pull_consumer (Peer);
      end if;
   end Disconnect_Pull_Supplier;

   ------------------------
   -- Get_Typed_Supplier --
   ------------------------

   function Get_Typed_Supplier
     (Self    : access Object)
     return CORBA.Object.Ref
   is
      Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("Get the mutually agreed Interface TypedPullSupplier"));

      Enter (Self.X.M);
      Ref := TypedConsumerAdmin.Impl.Pull
             (Self.X.Admin, Self.X.supported_interface);
      Leave (Self.X.M);

      return Ref;
   end Get_Typed_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull (Self : access Object) return CORBA.Any is
      pragma Unreferenced (Self);

      Event : CORBA.Any;
   begin
      pragma Debug (O ("attempt to pull new data from " &
                       "typedproxypull supplier"));
      pragma Debug (O ("no need to use generic pull in typed pullsupplier"));

      --  No need to implement generic pull in Typed ProxyPullSupplier
      raise Program_Error;
      return Event;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out    CORBA.Boolean;
      Returns   : out    CORBA.Any)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Has_Event);
      pragma Unreferenced (Returns);

   begin
      pragma Debug (O ("try to pull new data from typedproxypull supplier"));
      pragma Debug (O ("no need to use try_pull in typed pullsupplier"));

      --  No need to implement generic try_pull in Typed ProxyPullSupplier
      raise Program_Error;
   end Try_Pull;

end CosTypedEventChannelAdmin.TypedProxyPullSupplier.Impl;
