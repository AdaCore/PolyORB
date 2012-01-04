------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCOMM.SEQUENCEPUSHCONSUMER.IMPL                  --
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

with CosNotifyChannelAdmin.SequenceProxyPushSupplier;

with CosNotification;

package CosNotifyComm.SequencePushConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  Inherited IDL operations from CosNotifyComm::NotifyPublish

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   --  IDL operations

   procedure Push_Structured_Events
     (Self          : access Object;
      Notifications : CosNotification.EventBatch);

   procedure Disconnect_Sequence_Push_Consumer (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;

   procedure Connect_Sequence_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref);
   --  Call by application to connect object with proxy

   function Pull (Self  : access Object)
     return CosNotification.EventBatch;
   --  Call by application to consume a sequence of structured events

   procedure Try_Pull
     (Self : access Object;
      Done : out    CORBA.Boolean;
      Data : out    CosNotification.EventBatch);
   --  Call by application to try to consume a sequence of structured events

private

   type Sequence_Push_Consumer_Record;

   type Sequence_Push_Consumer_Access is access
        Sequence_Push_Consumer_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Sequence_Push_Consumer_Access;
   end record;

end CosNotifyComm.SequencePushConsumer.Impl;
