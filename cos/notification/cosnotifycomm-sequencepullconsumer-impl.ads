------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  COSNOTIFYCOMM.SEQUENCEPULLCONSUMER.IMPL                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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

with CosNotifyChannelAdmin.SequenceProxyPullSupplier;

package CosNotifyComm.SequencePullConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  Inherited IDL operations from CosNotifyComm::NotifyPublish

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   --  IDL operations

   procedure Disconnect_Sequence_Pull_Consumer
     (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create return Object_Ptr;
   --  Call by application to create an object and activate servant

   procedure Connect_Sequence_Proxy_Pull_Supplier
      (Self  : access Object;
       Proxy : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref);
   --  Call by application to connect object to proxy

   function Pull
     (Self       : access Object;
      Max_Number : CORBA.Long)
     return CosNotification.EventBatch;
   --  Call by application to consume a sequence of structured events

   procedure Try_Pull
     (Self       : access Object;
      Max_Number : CORBA.Long;
      Done       : out    CORBA.Boolean;
      Returns    : out    CosNotification.EventBatch);
   --  Call by application to try to consume a sequence of structured events

private

   type Sequence_Pull_Consumer_Record;
   type Sequence_Pull_Consumer_Access is access
        Sequence_Pull_Consumer_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Sequence_Pull_Consumer_Access;
   end record;

end CosNotifyComm.SequencePullConsumer.Impl;
