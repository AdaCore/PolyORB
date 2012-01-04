------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCOMM.STRUCTUREDPULLSUPPLIER.IMPL                 --
--                                                                          --
--                                 S p e c                                  --
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

with PortableServer;

with CosNotifyChannelAdmin.StructuredProxyPullConsumer;

package CosNotifyComm.StructuredPullSupplier.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  Inherited IDL operations from CosNotifyComm::NotifySubscribe

   procedure Subscription_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   --  IDL operations

   function Pull_Structured_Event
     (Self : access Object)
     return CosNotification.StructuredEvent;
   --  Call by proxy to pull a structured event

   procedure Try_Pull_Structured_Event
     (Self      : access Object;
      Has_Event : out    CORBA.Boolean;
      Returns   : out    CosNotification.StructuredEvent);
   --  Call by proxy to try to pull a structured event

   procedure Disconnect_Structured_Pull_Supplier
     (Self : access Object);
   --  Call by proxy to disconnect

   ----------------------
   -- PolyORB specific --
   ----------------------

   procedure Connect_Structured_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref);
   --  Call by application to connect object with proxy

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   procedure Push
     (Self : access Object;
      Data : CosNotification.StructuredEvent);
   --  Call by application to produce a structured event

private

   type Structured_Pull_Supplier_Record;

   type Structured_Pull_Supplier_Access is access
        Structured_Pull_Supplier_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Structured_Pull_Supplier_Access;
   end record;

end CosNotifyComm.StructuredPullSupplier.Impl;
