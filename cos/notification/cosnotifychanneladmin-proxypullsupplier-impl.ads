------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              COSNOTIFYCHANNELADMIN.PROXYPULLSUPPLIER.IMPL                --
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

with CosNotifyChannelAdmin.ConsumerAdmin;

with CosNotifyFilter.Filter;

with CosNotifyFilter.MappingFilter;

with PortableServer;

package CosNotifyChannelAdmin.ProxyPullSupplier.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   procedure Connect_Any_Pull_Consumer
     (Self : access Object;
      Pull_Consumer : CosEventComm.PullConsumer.Ref);

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

   --  IDL operations inherited from CosNotifyComm::PullSupplier

   procedure Subscription_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   function Pull
     (Self : access Object)
     return CORBA.Any;

   procedure Try_Pull
     (Self      : access Object;
      Has_Event : out    CORBA.Boolean;
      Returns   : out    CORBA.Any);

   procedure Disconnect_Pull_Supplier
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

   procedure Post
     (Self : access Object;
      Data : CORBA.Any);

private

   type Proxy_Pull_Supplier_Record;
   type Proxy_Pull_Supplier_Access is access Proxy_Pull_Supplier_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Proxy_Pull_Supplier_Access;
   end record;

end CosNotifyChannelAdmin.ProxyPullSupplier.Impl;
