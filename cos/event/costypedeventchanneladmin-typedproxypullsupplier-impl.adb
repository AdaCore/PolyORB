------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          COSTYPEDEVENTCHANNELADMIN.TYPEDPROXYPULLSUPPLIER.IMPL           --
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

with CORBA.Object;
pragma Warnings (Off, CORBA.Object);

with CosEventComm.PullConsumer;

with CosTypedEventChannelAdmin;
with CosTypedEventChannelAdmin.TypedConsumerAdmin.Impl;

with CosTypedEventChannelAdmin.TypedProxyPullSupplier.Helper;
pragma Elaborate (CosTypedEventChannelAdmin.TypedProxyPullSupplier.Helper);
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedProxyPullSupplier.Helper);

with CosTypedEventChannelAdmin.TypedProxyPullSupplier.Skel;
pragma Elaborate (CosTypedEventChannelAdmin.TypedProxyPullSupplier.Skel);
pragma Warnings (Off, CosTypedEventChannelAdmin.TypedProxyPullSupplier.Skel);

with PortableServer;

with PolyORB.Log;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Semaphores;

package body CosTypedEventChannelAdmin.TypedProxyPullSupplier.Impl is

   use CosEventComm;
   use CosEventChannelAdmin;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Semaphores;

   use PortableServer;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("typedproxypullsupplier");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type TypedProxy_Pull_Supplier_Record is record
      This      : Object_Ptr;
      Peer      : PullConsumer.Ref;
      Admin     : TypedConsumerAdmin.Impl.Object_Ptr;
      Semaphore : Semaphore_Access;
      supported_interface : CosTypedEventChannelAdmin.Key;
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

   ---------------------------
   -- Connect_Pull_Consumer --
   ---------------------------

   procedure Connect_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : in     PullConsumer.Ref) is
   begin
      pragma Debug (O ("connect pull consumer to typed proxy pull supplier"));
      Ensure_Initialization;

      Enter (Self_Mutex);

      if not PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Pull_Consumer;

      Leave (Self_Mutex);
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
      Create (Supplier.X.Semaphore);

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Ref := TypedConsumerAdmin.Impl.Pull
             (Self.X.Admin, Self.X.supported_interface);
      Leave (Self_Mutex);

      return Ref;
   end Get_Typed_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Event : CORBA.Any;
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("attempt to pull new data from "&
                       "typedproxypull supplier"));
      pragma Debug (O ("no need to use generic pull in typed pullsupplier"));
      Ensure_Initialization;

      --  No need to implement generic pull in Typed ProxyPullSupplier
      raise PolyORB.Not_Implemented;
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
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14

      Null_Any : CORBA.Any; --  WAG:3.15
   begin
      pragma Debug (O ("try to pull new data from typedproxypull supplier"));
      pragma Debug (O ("no need to use try_pull in typed pullsupplier"));
      Ensure_Initialization;

      Has_Event := False; --  WAG:3.15
      Returns := Null_Any; --  WAG:3.15

      --  No need to implement generic try_pull in Typed ProxyPullSupplier
      raise PolyORB.Not_Implemented;
   end Try_Pull;

end CosTypedEventChannelAdmin.TypedProxyPullSupplier.Impl;
