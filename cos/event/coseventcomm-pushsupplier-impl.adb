------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--       C O S E V E N T C O M M . P U S H S U P P L I E R . I M P L        --
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

with CosEventComm.PushSupplier.Helper;
pragma Elaborate (CosEventComm.PushSupplier.Helper);
pragma Warnings (Off, CosEventComm.PushSupplier.Helper);

with CosEventComm.PushSupplier.Skel;
pragma Elaborate (CosEventComm.PushSupplier.Skel);
pragma Warnings (Off, CosEventComm.PushSupplier.Skel);

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.ProxyPushConsumer;


with PolyORB.CORBA_P.Server_Tools; use  PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Soft_Links; use PolyORB.Tasking.Soft_Links;

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with PortableServer; use PortableServer;

with PolyORB.Log;

package body CosEventComm.PushSupplier.Impl is

   use  PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("pushsupplier");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Push_Supplier_Record is
      record
         This  : Object_Ptr;
         Peer  : ProxyPushConsumer.Ref;
      end record;

   ---------------------------------
   -- Connect_Proxy_Push_Consumer --
   ---------------------------------

   procedure Connect_Proxy_Push_Consumer
     (Self  : access Object;
      Proxy : in CosEventChannelAdmin.ProxyPushConsumer.Ref)
   is
      My_Ref : PushSupplier.Ref;

   begin
      pragma Debug (O ("connect proxy push supplier to push consumer"));

      Enter_Critical_Section;
      if not ProxyPushConsumer.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;
      Self.X.Peer := Proxy;
      Leave_Critical_Section;

      Servant_To_Reference (Servant (Self.X.This), My_Ref);
      ProxyPushConsumer.connect_push_supplier (Proxy, My_Ref);
   end Connect_Proxy_Push_Consumer;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : PushSupplier.Ref;

   begin
      pragma Debug (O ("create push supplier"));

      Supplier := new Object;
      Supplier.X := new Push_Supplier_Record;
      Supplier.X.This := Supplier;
      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ------------------------------
   -- Disconnect_Push_Supplier --
   ------------------------------

   procedure disconnect_push_supplier
     (Self : access Object)
   is
      Peer    : ProxyPushConsumer.Ref;
      Nil_Ref : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("disconnect push supplier"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave_Critical_Section;

      if not ProxyPushConsumer.Is_Nil (Peer) then
         ProxyPushConsumer.disconnect_push_consumer (Peer);
      end if;
   end disconnect_push_supplier;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self : access Object;
      Data : in CORBA.Any)
   is
      Peer : ProxyPushConsumer.Ref;

   begin
      pragma Debug (O ("push new data to push supplier"));

      Enter_Critical_Section;
      Peer := Self.X.Peer;
      Leave_Critical_Section;

      if ProxyPushConsumer.Is_Nil (Peer) then
         raise Disconnected;
      end if;

      ProxyPushConsumer.push (Peer, Data);
   end Push;

end CosEventComm.PushSupplier.Impl;
