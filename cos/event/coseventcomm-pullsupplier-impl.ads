------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       C O S E V E N T C O M M . P U L L S U P P L I E R . I M P L        --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;

with PortableServer;

with CosEventChannelAdmin.ProxyPullConsumer;

package CosEventComm.PullSupplier.Impl is

   --  This implementation is supposed to be application
   --  dependent. This is an example used to test the event service.

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Disconnect_Pull_Supplier
     (Self : access Object);
   --  Call by proxy to disconnect

   function Pull
     (Self : access Object)
     return CORBA.Any;
   --  Call by proxy to pull an event

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out    CORBA.Boolean;
      Returns   : out    CORBA.Any);
   --  Call by proxy to try yo pull an event

   ------------------------
   -- AdaBroker specific --
   ------------------------

   procedure Connect_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : CosEventChannelAdmin.ProxyPullConsumer.Ref);
   --  Call by application to connect object with proxy

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   procedure Push
     (Self : access Object;
      Data : CORBA.Any);
   --  Call by application to produce an event

private

   type Pull_Supplier_Record;
   type Pull_Supplier_Access is access Pull_Supplier_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Pull_Supplier_Access;
   end record;

end CosEventComm.PullSupplier.Impl;
