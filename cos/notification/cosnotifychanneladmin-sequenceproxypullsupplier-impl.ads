------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           COSNOTIFYCHANNELADMIN.SEQUENCEPROXYPULLSUPPLIER.IMPL           --
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

with CosNotifyChannelAdmin.ConsumerAdmin;

with CosNotifyFilter.Filter;

with CosNotifyFilter.MappingFilter;

with PortableServer;

package CosNotifyChannelAdmin.SequenceProxyPullSupplier.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   procedure Connect_Sequence_Pull_Consumer
     (Self : access Object;
      Pull_Consumer : CosNotifyComm.SequencePullConsumer.Ref);

   --  IDL operations inherited from CosNotifyChannelAdmin::ProxySupplier

   function Get_MyType
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyType;

   function Get_MyAdmin
     (Self : access Object)
     return CosNotifyChannelAdmin.ConsumerAdmin_Forward.Ref;

   function Get_Priority_Filter
     (Self : access Object)
     return CosNotifyFilter.MappingFilter.Ref;

   procedure Set_Priority_Filter
     (Self : access Object;
      To   : CosNotifyFilter.MappingFilter.Ref);

   function Get_Lifetime_Filter
     (Self : access Object)
     return CosNotifyFilter.MappingFilter.Ref;

   procedure Set_Lifetime_Filter
     (Self : access Object;
      To   : CosNotifyFilter.MappingFilter.Ref);

   function Obtain_Offered_Types
     (Self : access Object;
      Mode : CosNotifyChannelAdmin.ObtainInfoMode)
     return CosNotification.EventTypeSeq;

   procedure Validate_Event_QoS
     (Self          : access Object;
      Required_QoS  : CosNotification.QoSProperties;
      Available_QoS : out CosNotification.NamedPropertyRangeSeq);

   function Get_QoS
     (Self : access Object)
     return CosNotification.QoSProperties;

   procedure Set_QoS
     (Self : access Object;
      QoS  : CosNotification.QoSProperties);

   procedure Validate_QoS
     (Self          : access Object;
      Required_QoS  : CosNotification.QoSProperties;
      Available_QoS : out CosNotification.NamedPropertyRangeSeq);

   function Add_Filter
     (Self       : access Object;
      New_Filter : CosNotifyFilter.Filter.Ref)
     return CosNotifyFilter.FilterID;

   procedure Remove_Filter
     (Self   : access Object;
      Filter : CosNotifyFilter.FilterID);

   function Get_Filter
     (Self   : access Object;
      Filter : CosNotifyFilter.FilterID)
     return CosNotifyFilter.Filter.Ref;

   function Get_All_Filters
     (Self : access Object)
     return CosNotifyFilter.FilterIDSeq;

   procedure Remove_All_Filters (Self : access Object);

   --  IDL operations inherited from CosNotifyComm::SequencePullSupplier

   procedure Subscription_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   function Pull_Structured_Events
     (Self       : access Object;
      Max_Number : CORBA.Long)
     return CosNotification.EventBatch;

   procedure Try_Pull_Structured_Events
     (Self       : access Object;
      Max_Number : CORBA.Long;
      Has_Event  : out    CORBA.Boolean;
      Returns    : out    CosNotification.EventBatch);

   procedure Disconnect_Sequence_Pull_Supplier
     (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create
     (Admin       : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      Initial_QoS : CosNotification.QoSProperties;
      Ptype       : CosNotifyChannelAdmin.ProxyType;
      Proxy_Id    : CosNotifyChannelAdmin.ProxyID)
   return Object_Ptr;

   procedure Sequence_Post
     (Self : access Object;
      Data : CosNotification.EventBatch);

private

   type Sequence_Proxy_Pull_Supplier_Record;
   type Sequence_Proxy_Pull_Supplier_Access is access
        Sequence_Proxy_Pull_Supplier_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Sequence_Proxy_Pull_Supplier_Access;
   end record;

end CosNotifyChannelAdmin.SequenceProxyPullSupplier.Impl;
