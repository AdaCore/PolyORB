------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCHANNELADMIN.SUPPLIERADMIN.IMPL                  --
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
      Proxy_Id   : CosNotifyChannelAdmin.ProxyID)
     return CosNotifyChannelAdmin.ProxyConsumer.Ref;

   procedure Obtain_Notification_Pull_Consumer
     (Self     : access Object;
      Ctype    : CosNotifyChannelAdmin.ClientType;
      Proxy_Id : out CosNotifyChannelAdmin.ProxyID;
      Returns  : out CosNotifyChannelAdmin.ProxyConsumer.Ref);

   procedure Obtain_Notification_Push_Consumer
     (Self     : access Object;
      Ctype    : CosNotifyChannelAdmin.ClientType;
      Proxy_Id : out CosNotifyChannelAdmin.ProxyID;
      Returns  : out CosNotifyChannelAdmin.ProxyConsumer.Ref);

   procedure Destroy (Self : access Object);

   --  IDL Operations inherited from CosNotification::QoSAdmin

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

   --  Inherited IDL operations from CosNotifyComm::NotifyPublish

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   --  Inherited IDL operations from CosNotifyFilter::FilterAdmin

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
      Data : CORBA.Any);

   procedure Structured_Post
     (Self         : access Object;
      Notification : CosNotification.StructuredEvent);

   procedure Sequence_Post
     (Self          : access Object;
      Notifications : CosNotification.EventBatch);

private

   type Supplier_Admin_Record;
   type Supplier_Admin_Access is access Supplier_Admin_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Supplier_Admin_Access;
   end record;

end CosNotifyChannelAdmin.SupplierAdmin.Impl;
