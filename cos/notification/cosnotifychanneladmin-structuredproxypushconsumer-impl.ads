------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         COSNOTIFYCHANNELADMIN.STRUCTUREDPROXYPUSHCONSUMER.IMPL           --
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

with CosNotifyChannelAdmin.SupplierAdmin;

with CosNotifyFilter.Filter;

with PortableServer;

package CosNotifyChannelAdmin.StructuredProxyPushConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL Operations

   procedure Connect_Structured_Push_Supplier
     (Self : access Object;
      Push_Supplier : CosNotifyComm.StructuredPushSupplier.Ref);

   --  IDL operations inherited from CosNotifyChannelAdmin::ProxyConsumer

   function Get_MyType
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyType;

   function Get_MyAdmin
     (Self : access Object)
     return CosNotifyChannelAdmin.SupplierAdmin_Forward.Ref;

   function Obtain_Subscription_Types
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

   --  Inherited IDL operations from CosNotifyComm::StructuredPushConsumer

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   procedure Push_Structured_Event
     (Self         : access Object;
      Notification : CosNotification.StructuredEvent);

   procedure Disconnect_Structured_Push_Consumer (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create
     (Admin       : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      Initial_QoS : CosNotification.QoSProperties;
      Ptype       : CosNotifyChannelAdmin.ProxyType;
      Proxy_Id    : CosNotifyChannelAdmin.ProxyID)
     return Object_Ptr;

private

   type Structured_Proxy_Push_Consumer_Record;

   type Structured_Proxy_Push_Consumer_Access is access
        Structured_Proxy_Push_Consumer_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Structured_Proxy_Push_Consumer_Access;
   end record;

end CosNotifyChannelAdmin.StructuredProxyPushConsumer.Impl;
