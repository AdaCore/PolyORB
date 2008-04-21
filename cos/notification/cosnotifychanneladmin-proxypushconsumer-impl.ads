------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               COSNOTIFYCHANNELADMIN.PROXYPUSHCONSUMER.IMPL               --
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

--  with CORBA;

with CosNotifyChannelAdmin.SupplierAdmin;

with CosNotifyFilter.Filter;

with PortableServer;

package CosNotifyChannelAdmin.ProxyPushConsumer.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL Operations

   procedure Connect_Any_Push_Supplier
     (Self : access Object;
      Push_Supplier : CosEventComm.PushSupplier.Ref);

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

   --  Inherited IDL operations from CosNotifyComm::PushConsumer

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq);

   procedure Push
     (Self : access Object;
      Data : CORBA.Any);

   procedure Disconnect_Push_Consumer (Self : access Object);

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

   type Proxy_Push_Consumer_Record;

   type Proxy_Push_Consumer_Access is access Proxy_Push_Consumer_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Proxy_Push_Consumer_Access;
   end record;

end CosNotifyChannelAdmin.ProxyPushConsumer.Impl;
