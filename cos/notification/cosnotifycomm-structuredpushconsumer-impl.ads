------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCOMM.STRUCTUREDPUSHCONSUMER.IMPL                 --
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

with CosNotifyChannelAdmin.StructuredProxyPushSupplier;

with CosNotification;

package CosNotifyComm.StructuredPushConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  Inherited IDL operations from CosNotifyComm::NotifyPublish

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   --  IDL operations

   procedure Push_Structured_Event
     (Self         : access Object;
      Notification : CosNotification.StructuredEvent);

   procedure Disconnect_Structured_Push_Consumer (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;

   procedure Connect_Structured_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref);
   --  Call by application to connect object with proxy

   function Pull (Self  : access Object)
     return CosNotification.StructuredEvent;
   --  Call by application to consume an event

   procedure Try_Pull
     (Self : access Object;
      Done : out    CORBA.Boolean;
      Data : out    CosNotification.StructuredEvent);
   --  Call by application to try to consume a structured event

private

   type Structured_Push_Consumer_Record;

   type Structured_Push_Consumer_Access is access
        Structured_Push_Consumer_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Structured_Push_Consumer_Access;
   end record;

end CosNotifyComm.StructuredPushConsumer.Impl;
