------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  COSNOTIFYCOMM.SEQUENCEPUSHCONSUMER.IMPL                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
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
      Notifications : in CosNotification.EventBatch);

   procedure Disconnect_Sequence_Push_Consumer (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;

   procedure Connect_Sequence_Proxy_Push_Supplier
     (Self  : access Object;
      Proxy : in     CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref);
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
