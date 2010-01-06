------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  COSNOTIFYCHANNELADMIN.CONSUMERADMIN.IMPL                --
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

with CosNotifyChannelAdmin.EventChannel;

with CosNotifyFilter.MappingFilter;

with PortableServer;

package CosNotifyChannelAdmin.ConsumerAdmin.Impl is

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

   function Get_Pull_Suppliers
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyIDSeq;

   function Get_Push_Suppliers
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyIDSeq;

   function Get_Proxy_Supplier
     (Self       : access Object;
      Proxy_Id   : CosNotifyChannelAdmin.ProxyID)
     return CosNotifyChannelAdmin.ProxySupplier.Ref;

   procedure Obtain_Notification_Pull_Supplier
     (Self     : access Object;
      Ctype    : CosNotifyChannelAdmin.ClientType;
      Proxy_Id : out CosNotifyChannelAdmin.ProxyID;
      Returns  : out CosNotifyChannelAdmin.ProxySupplier.Ref);

   procedure Obtain_Notification_Push_Supplier
     (Self     : access Object;
      Ctype    : CosNotifyChannelAdmin.ClientType;
      Proxy_Id : out CosNotifyChannelAdmin.ProxyID;
      Returns  : out CosNotifyChannelAdmin.ProxySupplier.Ref);

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

   --  Inherited IDL operations from CosNotifyComm::NotifySubscribe

   procedure Subscription_Change
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

   --  Inherited IDL operations from CosEventChannelAdmin::ConsumerAdmin

   function Obtain_Push_Supplier
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPushSupplier.Ref;

   function Obtain_Pull_Supplier
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullSupplier.Ref;

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create
      (Channel     : CosNotifyChannelAdmin.EventChannel.Ref;
       Initial_QoS : CosNotification.QoSProperties;
       MyID        : CosNotifyChannelAdmin.AdminID;
       MyOp        : CosNotifyChannelAdmin.InterFilterGroupOperator := AND_OP)
      return Object_Ptr;

   function GetTotalSuppliers
     (Self : access Object)
     return CORBA.Long;
   --  Returns the total number of Suppliers created by this Admin

   procedure Post
     (Self          : access Object;
      Data          : CORBA.Any;
      Internal_Post : CORBA.Boolean := False);

   procedure Structured_Post
     (Self          : access Object;
      Notification  : CosNotification.StructuredEvent;
      Internal_Post : CORBA.Boolean := False);

   procedure Sequence_Post
     (Self          : access Object;
      Notifications : CosNotification.EventBatch;
      Internal_Post : CORBA.Boolean := False);

private

   type Consumer_Admin_Record;
   type Consumer_Admin_Access is access Consumer_Admin_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Consumer_Admin_Access;
   end record;

end CosNotifyChannelAdmin.ConsumerAdmin.Impl;
