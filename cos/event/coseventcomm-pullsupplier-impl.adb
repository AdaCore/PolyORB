------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--       C O S E V E N T C O M M . P U L L S U P P L I E R . I M P L        --
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

with CORBA;
with CORBA.Impl;

with CosEventComm.PullSupplier.Helper;
with CosEventComm.PullSupplier.Skel;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPullConsumer;

with Broca.Server_Tools; use  Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer; use PortableServer;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventComm.PullSupplier.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("pullsupplier");
   procedure O is new Broca.Debug.Output (Flag);

   type Pull_Supplier_Record is
      record
         This    : Object_Ptr;
         Peer    : ProxyPullConsumer.Ref;
         Empty   : Boolean;
         Event   : CORBA.Any;
         Watcher : Watcher_Access;
      end record;

   ---------------------------------
   -- Connect_Proxy_Pull_Consumer --
   ---------------------------------

   procedure Connect_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPullConsumer.Ref)
   is
      My_Ref : PullSupplier.Ref;

   begin
      pragma Debug (O ("connect proxy pull supplier to pull consumer"));

      Enter_Critical_Section;
      if not ProxyPullConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave_Critical_Section;

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPullConsumer.Connect_Pull_Supplier (Proxy, My_Ref);
   end Connect_Proxy_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : PullSupplier.Ref;

   begin
      pragma Debug (O ("create pull supplier"));

      Supplier         := new Object;
      Supplier.X       := new Pull_Supplier_Record;
      Supplier.X.This  := Supplier;
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
      Peer    : ProxyPullConsumer.Ref;
      Nil_Ref : ProxyPullConsumer.Ref;

   begin
      pragma Debug (O ("disconnect pull supplier"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Update (Self.X.Watcher);
      Leave_Critical_Section;

      if not ProxyPullConsumer.Is_Nil (Peer) then
         ProxyPullConsumer.Disconnect_Pull_Consumer (Peer);
      end if;
   end Disconnect_Pull_Supplier;

   ----------
   -- Pull --
   ----------

   function Pull
     (Self : access Object)
     return CORBA.Any
   is
      Event   : CORBA.Any;
      Version : Version_Id;

   begin
      loop
         pragma Debug (O ("attempt to pull new data from pull supplier"));

         Enter_Critical_Section;
         if ProxyPullConsumer.Is_Nil (Self.X.Peer) then
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

      pragma Debug (O ("succeed to pull new data from pull supplier"));

      return Event;
   end Pull;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any) is

   begin
      pragma Debug (O ("push new data to pull supplier"));

      Enter_Critical_Section;
      Self.X.Empty := False;
      Self.X.Event := Data;
      Update (Self.X.Watcher);
      Leave_Critical_Section;
   end Push;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out CORBA.Boolean;
      Returns   : out CORBA.Any) is
   begin
      pragma Debug (O ("try to pull new data from pull supplier"));

      Enter_Critical_Section;
      if ProxyPullConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise Disconnected;
      end if;

      if Self.X.Empty then
         Has_Event := False;

      else
         Has_Event := True;
         Returns := Self.X.Event;
         Self.X.Empty := True;
      end if;
      Leave_Critical_Section;
   end Try_Pull;

end CosEventComm.PullSupplier.Impl;
