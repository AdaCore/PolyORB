------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
-- C O S E V E N T C H A N N E L A D M I N . P R O X Y P U L L C O N S U M E R . I M P L  --
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

with CosEventComm.PullSupplier;

with CosEventChannelAdmin; use CosEventChannelAdmin;

with CosEventChannelAdmin.SupplierAdmin.Impl;

with CosEventChannelAdmin.ProxyPullConsumer.Helper;
with CosEventChannelAdmin.ProxyPullConsumer.Skel;

with CosEventChannelAdmin.SupplierAdmin.Impl;

with Broca.Server_Tools; use Broca.Server_Tools;
with Broca.Soft_Links;    use  Broca.Soft_Links;

with PortableServer; use PortableServer;

with CORBA.Object;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body CosEventChannelAdmin.ProxyPullConsumer.Impl is

   Flag : constant Natural := Broca.Debug.Is_Active ("proxypullconsumer");
   procedure O is new Broca.Debug.Output (Flag);

   task type Proxy_Pull_Consumer_Engin is
      entry Connect (Consumer : in Object_Ptr);
   end Proxy_Pull_Consumer_Engin;

   type Proxy_Pull_Consumer_Engin_Access is access Proxy_Pull_Consumer_Engin;

   type Proxy_Pull_Consumer_Record is
      record
         This   : Object_Ptr;
         Peer   : PullSupplier.Ref;
         Admin  : SupplierAdmin.Impl.Object_Ptr;
         Engin  : Proxy_Pull_Consumer_Engin_Access;
      end record;

   -------------------------------
   -- Proxy_Pull_Consumer_Engin --
   -------------------------------

   task body Proxy_Pull_Consumer_Engin
   is
      This  : Object_Ptr;
      Peer  : PullSupplier.Ref;
      Event : CORBA.Any;

   begin
      loop
         select
            accept Connect
              (Consumer : Object_Ptr)
            do
               This := Consumer;
            end Connect;
         or
            terminate;
         end select;

         loop
            Enter_Critical_Section;
            Peer := This.X.Peer;
            Leave_Critical_Section;

            exit when PullSupplier.Is_Nil (Peer);

            pragma Debug
              (O ("pull new data from proxy pull consumer engin"));

            begin
               Event := PullSupplier.Pull (Peer);
            exception when others =>
               exit;
            end;

            pragma Debug
              (O ("post new data from proxy pull consumer to admin"));

            SupplierAdmin.Impl.Post (This.X.Admin, Event);
         end loop;
      end loop;
   end Proxy_Pull_Consumer_Engin;

   ---------------------------
   -- Connect_Pull_Supplier --
   ---------------------------

   procedure Connect_Pull_Supplier
     (Self          : access Object;
      Pull_Supplier : in CosEventComm.PullSupplier.Ref) is
   begin
      pragma Debug (O ("connect pull supplier to proxy pull consumer"));

      Enter_Critical_Section;
      if not PullSupplier.Is_Nil (Self.X.Peer) then
         Leave_Critical_Section;
         raise AlreadyConnected;
      end if;

      Self.X.Peer := Pull_Supplier;

      --  Start engin
      if Self.X.Engin = null then
         Self.X.Engin := new Proxy_Pull_Consumer_Engin;
      end if;
      Self.X.Engin.Connect (Self.X.This);
      Leave_Critical_Section;
   end Connect_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create (Admin : SupplierAdmin.Impl.Object_Ptr) return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : ProxyPullConsumer.Ref;

   begin
      pragma Debug (O ("create proxy pull consumer"));

      Consumer         := new Object;
      Consumer.X       := new Proxy_Pull_Consumer_Record;
      Consumer.X.This  := Consumer;
      Consumer.X.Admin := Admin;
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

   ------------------------------
   -- Disconnect_Pull_Consumer --
   ------------------------------

   procedure Disconnect_Pull_Consumer
     (Self : access Object)
   is
      Peer    : PullSupplier.Ref;
      Nil_Ref : PullSupplier.Ref;

   begin
      pragma Debug (O ("disconnect proxy pull consumer"));

      Enter_Critical_Section;
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave_Critical_Section;

      if not PullSupplier.Is_Nil (Peer) then
         PullSupplier.Disconnect_Pull_Supplier (Peer);
      end if;
   end Disconnect_Pull_Consumer;

end CosEventChannelAdmin.ProxyPullConsumer.Impl;
