------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
-- C O S E V E N T C H A N N E L A D M I N . P R O X Y P U L L S U P P L I E R . I M P L  --
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

with CosEventComm; use CosEventComm;

with CosEventComm.PullConsumer;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPullSupplier.Helper;
with CosEventChannelAdmin.ProxyPullSupplier.Skel;

with CosEventChannelAdmin.ConsumerAdmin.Impl;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

with PortableServer; use PortableServer;

with CORBA.Object;

package body CosEventChannelAdmin.ProxyPullSupplier.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("proxypullsupplier");
   procedure O is new Broca.Debug.Output (Flag);

   type Proxy_Pull_Supplier_Record is
      record
         This    : Object_Ptr;
         Peer    : PullConsumer.Ref;
         Admin   : ConsumerAdmin.Impl.Object_Ptr;
         Event   : CORBA.Any;
         Empty   : Boolean;
         Watcher : Watcher_Access;
      end record;

   ---------------------------
   -- Connect_Pull_Consumer --
   ---------------------------

   procedure Connect_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : in PullConsumer.Ref)
   is
   begin
      pragma Debug (O ("connect pull consumer to proxy pull supplier"));

      Enter_Critical_Section;
      if not PullConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Pull_Consumer;
      Leave_Critical_Section;
   end Connect_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create (Admin : ConsumerAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : ProxyPullSupplier.Ref;

   begin
      pragma Debug (O ("create proxy pull supplier"));

      Supplier         := new Object;
      Supplier.X       := new Proxy_Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
      Supplier.X.Admin := Admin;
      Supplier.X.Empty := True;
      Create (Supplier.X.Watcher);
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
      pragma Debug (O ("disconnect proxy pull supplier"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Update (Self.X.Watcher);
      Leave_Critical_Section;

      if not PullConsumer.Is_Nil (Peer) then
         PullConsumer.Disconnect_Pull_Consumer (Peer);
      end if;
   end Disconnect_Pull_Supplier;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any) is
   begin
      pragma Debug (O ("post new data to proxy pull supplier"));

      Enter_Critical_Section;
      Self.X.Event := Data;
      Self.X.Empty := False;
      Update (Self.X.Watcher);
      Leave_Critical_Section;
   end Post;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Version : Version_Id;
      Event   : CORBA.Any;

   begin
      loop
         pragma Debug
           (O ("attempt to pull new data from proxy pull supplier"));

         Enter_Critical_Section;
         if PullConsumer.Is_Nil (Self.X.Peer) then
            Leave_Critical_Section;
            raise Disconnected;
         end if;

         if not Self.X.Empty then
            Event := Self.X.Event;
            Self.X.Empty := True;
            Leave_Critical_Section;
            exit;
         end if;
         Lookup (Self.X.Watcher, Version);
         Leave_Critical_Section;
         Differ (Self.X.Watcher, Version);
      end loop;
      pragma Debug (O ("succeed to pull new data from proxy pull supplier"));

      return Event;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out CORBA.Boolean;
      Returns   : out CORBA.Any) is
   begin
      pragma Debug (O ("try to pull new data from proxy pull supplier"));
      Enter_Critical_Section;
      if PullConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise Disconnected;
      end if;

      if Self.X.Empty then
         Has_Event := False;

      else
         Has_Event    := True;
         Returns      := Self.X.Event;
         Self.X.Empty := True;
      end if;
      Leave_Critical_Section;
   end Try_Pull;

end CosEventChannelAdmin.ProxyPullSupplier.Impl;
