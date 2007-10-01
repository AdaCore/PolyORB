------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSTYPEDEVENTCOMM.TYPEDPULLSUPPLIER.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with CORBA.Impl;

with CosEventChannelAdmin.ProxyPullConsumer;
with CosTypedEventChannelAdmin.TypedEventChannel;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Semaphores;
with PolyORB.Tasking.Mutexes;

with CosTypedEventComm.TypedPullSupplier.Skel;
pragma Warnings (Off, CosTypedEventComm.TypedPullSupplier.Skel);

package body CosTypedEventComm.TypedPullSupplier.Impl is

   use CosEventChannelAdmin;

   use CosTypedEventChannelAdmin;

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Semaphores;
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
      Semaphore : Semaphore_Access;
      Supports_Interface : TypedEventChannel.Impl.Interface_Ptr;
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
      Create (Supplier.X.Semaphore);

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Self.X.Supports_Interface := I_Ptr;
      Leave (Self_Mutex);
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

      Ensure_Initialization;
      Enter (Self_Mutex);
      InterfaceObject := Self.X.Supports_Interface.all;
      Leave (Self_Mutex);

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
      Ensure_Initialization;

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
      Ensure_Initialization;

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

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      V (Self.X.Semaphore);

      if not ProxyPullConsumer.Is_Nil (Peer) then
         ProxyPullConsumer.disconnect_pull_consumer (Peer);
      end if;
   end Disconnect_Pull_Supplier;

end CosTypedEventComm.TypedPullSupplier.Impl;
