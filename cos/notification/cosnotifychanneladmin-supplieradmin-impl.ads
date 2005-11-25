------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  COSNOTIFYCHANNELADMIN.SUPPLIERADMIN.IMPL                --
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

with CosNotifyChannelAdmin.EventChannel;

with PortableServer;

package CosNotifyChannelAdmin.SupplierAdmin.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function Get_MyID
     (Self : access Object)
     return CosNotifyChannelAdmin.AdminID;

   function Get_MyChannel
     (Self : access Object)
     return CosNotifyChannelAdmin.EventChannel_Forward.Ref;

   function Get_MyOperator
     (Self : access Object)
     return CosNotifyChannelAdmin.InterFilterGroupOperator;

   function Get_Pull_Consumers
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyIDSeq;

   function Get_Push_Consumers
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyIDSeq;

   function Get_Proxy_Consumer
     (Self       : access Object;
      Proxy_Id   : in CosNotifyChannelAdmin.ProxyID)
     return CosNotifyChannelAdmin.ProxyConsumer.Ref;

   procedure Obtain_Notification_Pull_Consumer
     (Self     : access Object;
      Ctype    : in CosNotifyChannelAdmin.ClientType;
      Proxy_Id : out CosNotifyChannelAdmin.ProxyID;
      Returns  : out CosNotifyChannelAdmin.ProxyConsumer.Ref);

   procedure Obtain_Notification_Push_Consumer
     (Self     : access Object;
      Ctype    : in CosNotifyChannelAdmin.ClientType;
      Proxy_Id : out CosNotifyChannelAdmin.ProxyID;
      Returns  : out CosNotifyChannelAdmin.ProxyConsumer.Ref);

   procedure Destroy (Self : access Object);

   --  IDL Operations inherited from CosNotification::QoSAdmin

   function Get_QoS
     (Self : access Object)
     return CosNotification.QoSProperties;

   procedure Set_QoS
     (Self : access Object;
      QoS  : in CosNotification.QoSProperties);

   procedure Validate_QoS
     (Self          : access Object;
      Required_QoS  : in CosNotification.QoSProperties;
      Available_QoS : out CosNotification.NamedPropertyRangeSeq);

   --  Inherited IDL operations from CosNotifyComm::NotifyPublish

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   --  Inherited IDL operations from CosNotifyFilter::FilterAdmin

   function Add_Filter
     (Self       : access Object;
      New_Filter : in CosNotifyFilter.Filter.Ref)
     return CosNotifyFilter.FilterID;

   procedure Remove_Filter
     (Self   : access Object;
      Filter : in CosNotifyFilter.FilterID);

   function Get_Filter
     (Self   : access Object;
      Filter : in CosNotifyFilter.FilterID)
     return CosNotifyFilter.Filter.Ref;

   function Get_All_Filters
     (Self : access Object)
     return CosNotifyFilter.FilterIDSeq;

   procedure Remove_All_Filters (Self : access Object);

   --  Inherited IDL operations from CosEventChannelAdmin::SupplierAdmin

   function Obtain_Push_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPushConsumer.Ref;

   function Obtain_Pull_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullConsumer.Ref;

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create
     (Channel     : CosNotifyChannelAdmin.EventChannel.Ref;
      Initial_QoS : CosNotification.QoSProperties;
      MyID        : CosNotifyChannelAdmin.AdminID;
      MyOp        : CosNotifyChannelAdmin.InterFilterGroupOperator := AND_OP)
     return Object_Ptr;

   function GetTotalConsumers
     (Self : access Object)
     return CORBA.Long;
   --  Returns the total number of Consumers created by this Admin

   procedure Post
     (Self : access Object;
      Data : in CORBA.Any);

   procedure Structured_Post
     (Self         : access Object;
      Notification : in CosNotification.StructuredEvent);

   procedure Sequence_Post
     (Self          : access Object;
      Notifications : in CosNotification.EventBatch);

private

   type Supplier_Admin_Record;
   type Supplier_Admin_Access is access Supplier_Admin_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Supplier_Admin_Access;
   end record;

end CosNotifyChannelAdmin.SupplierAdmin.Impl;
