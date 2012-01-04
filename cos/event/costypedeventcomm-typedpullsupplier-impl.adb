------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSTYPEDEVENTCOMM.TYPEDPULLSUPPLIER.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with CORBA.Impl;

with CosEventChannelAdmin.ProxyPullConsumer;
with CosTypedEventChannelAdmin.TypedEventChannel;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosTypedEventComm.TypedPullSupplier.Skel;
pragma Warnings (Off, CosTypedEventComm.TypedPullSupplier.Skel);

package body CosTypedEventComm.TypedPullSupplier.Impl is

   use CosEventChannelAdmin;

   use CosTypedEventChannelAdmin;

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedpullsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Typed_Pull_Supplier_Record is record
      This    : Object_Ptr;
      Peer    : ProxyPullConsumer.Ref;
      Empty   : Boolean;
      Event   : CORBA.Any;
      M       : Mutex_Access;
      Supports_Interface : TypedEventChannel.Impl.Interface_Ptr;
   end record;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : TypedPullSupplier.Ref;

   begin
      pragma Debug (O ("Create typedpull supplier"));

      Supplier         := new Object;
      Supplier.X       := new Typed_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Empty := True;
      Create (Supplier.X.M);

      Initiate_Servant (Servant (Supplier), My_Ref);

      return Supplier;
   end Create;

   ----------------------
   -- SetInterface_Ptr --
   ----------------------

   procedure SetInterface_Ptr
     (Self  : access Object;
     I_Ptr  : TypedEventChannel.Impl.Interface_Ptr) is
   begin
      pragma Debug (O ("set the supported interface pointer in " &
                       "typed pullsupplier"));

      Enter (Self.X.M);
      Self.X.Supports_Interface := I_Ptr;
      Leave (Self.X.M);
   end SetInterface_Ptr;

   ------------------------
   -- Get_Typed_Supplier --
   ------------------------

   function Get_Typed_Supplier
      (Self    : access Object)
     return CORBA.Object.Ref
   is
      InterfaceObject : CORBA.Impl.Object_Ptr;
      Ref : CORBA.Object.Ref;
   begin
      pragma Debug (O ("get the mutually agreed interface from " &
                       "typed pullsupplier"));

      Enter (Self.X.M);
      InterfaceObject := Self.X.Supports_Interface.all;
      Leave (Self.X.M);

      Initiate_Servant (PortableServer.Servant (InterfaceObject), Ref);
      return Ref;
   end Get_Typed_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      pragma Unreferenced (Self);

      Event   : CORBA.Any;
   begin
      pragma Debug (O ("attempt to pull new data from typed pullsupplier"));
      pragma Debug (O ("no need to use generic pull in typed pullsupplier"));

      --  No need to implement generic pull in Typed PullSupplier

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
      pragma Debug (O ("try to pull new data from typed pullsupplier"));
      pragma Debug (O ("No need to use try_pull in typed pullsupplier"));

      --  No need to implement generic try_pull in Typed PullSupplier
      raise Program_Error;
   end Try_Pull;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier
     (Self : access Object)
   is
      Peer    : ProxyPullConsumer.Ref;
      Nil_Ref : ProxyPullConsumer.Ref;

   begin
      pragma Debug (O ("disconnect typedpull supplier"));

      Enter (Self.X.M);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);

      if not ProxyPullConsumer.Is_Nil (Peer) then
         ProxyPullConsumer.disconnect_pull_consumer (Peer);
      end if;
   end Disconnect_Pull_Supplier;

end CosTypedEventComm.TypedPullSupplier.Impl;
