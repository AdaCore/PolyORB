------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCOMM.SEQUENCEPULLSUPPLIER.IMPL                  --
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

with CosNotifyChannelAdmin.SequenceProxyPullConsumer;

package CosNotifyComm.SequencePullSupplier.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  Inherited IDL operations from CosNotifyComm::NotifySubscribe

   procedure Subscription_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   --  IDL operations

   function Pull_Structured_Events
     (Self       : access Object;
      Max_Number : CORBA.Long)
     return CosNotification.EventBatch;
   --  Call by proxy to pull a sequence of structured events

   procedure Try_Pull_Structured_Events
     (Self       : access Object;
      Max_Number : CORBA.Long;
      Has_Event  : out CORBA.Boolean;
      Returns    : out CosNotification.EventBatch);
   --  Call by proxy to try to pull a structured event

   procedure Disconnect_Sequence_Pull_Supplier
     (Self : access Object);
   --  Call by proxy to disconnect

   ----------------------
   -- PolyORB specific --
   ----------------------

   procedure Connect_Sequence_Proxy_Pull_Consumer
     (Self  : access Object;
      Proxy : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref);
   --  Call by application to connect object with proxy

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   procedure Push
     (Self : access Object;
      Data : CosNotification.EventBatch);
   --  Call by application to produce a sequence of structured event

private

   type Sequence_Pull_Supplier_Record;

   type Sequence_Pull_Supplier_Access is access
        Sequence_Pull_Supplier_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Sequence_Pull_Supplier_Access;
   end record;

end CosNotifyComm.SequencePullSupplier.Impl;
