------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCHANNELADMIN.EVENTCHANNEL.IMPL                  --
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

with CORBA;

with CosNotifyChannelAdmin.EventChannelFactory;

with PortableServer;

package CosNotifyChannelAdmin.EventChannel.Impl is

   type Object is new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   --  IDL operations

   function Get_MyFactory
     (Self : access Object)
     return CosNotifyChannelAdmin.EventChannelFactory_Forward.Ref;

   function Get_Default_Consumer_Admin
     (Self : access Object)
     return CosNotifyChannelAdmin.ConsumerAdmin.Ref;

   function Get_Default_Supplier_Admin
     (Self : access Object)
     return CosNotifyChannelAdmin.SupplierAdmin.Ref;

   function Get_Default_Filter_Factory
     (Self : access Object)
     return CosNotifyFilter.FilterFactory.Ref;

   procedure New_For_Consumers
      (Self    : access Object;
       Op      : CosNotifyChannelAdmin.InterFilterGroupOperator;
       Id      : out CosNotifyChannelAdmin.AdminID;
       Returns : out CosNotifyChannelAdmin.ConsumerAdmin.Ref);

   procedure New_For_Suppliers
      (Self    : access Object;
       Op      : CosNotifyChannelAdmin.InterFilterGroupOperator;
       Id      : out CosNotifyChannelAdmin.AdminID;
       Returns : out CosNotifyChannelAdmin.SupplierAdmin.Ref);

   function Get_ConsumerAdmin
      (Self : access Object;
       Id   : CosNotifyChannelAdmin.AdminID)
       return CosNotifyChannelAdmin.ConsumerAdmin.Ref;

   function Get_SupplierAdmin
      (Self : access Object;
       Id   : CosNotifyChannelAdmin.AdminID)
       return CosNotifyChannelAdmin.SupplierAdmin.Ref;

   function Get_All_ConsumerAdmins
      (Self : access Object)
      return CosNotifyChannelAdmin.AdminIDSeq;

   function Get_All_SupplierAdmins
      (Self : access Object)
      return CosNotifyChannelAdmin.AdminIDSeq;

   --  IDL Operations inherited from CosNotification::QoSAdmin

   function Get_QoS
      (Self : access Object)
      return CosNotification.QoSProperties;

   procedure Set_QoS
      (Self : access Object;
       QoS  : CosNotification.QoSProperties);

   procedure Validate_QoS
      (Self         : access Object;
      Required_QoS  : CosNotification.QoSProperties;
      Available_QoS : out CosNotification.NamedPropertyRangeSeq);

   --  IDL Operations inherited from CosNotification::AdminPropertiesAdmin

   function Get_Admin
     (Self : access Object)
     return CosNotification.AdminProperties;

   procedure Set_Admin
      (Self  : access Object;
       Admin : CosNotification.AdminProperties);

   --  IDL Operations inherited from CosEventChannelAdmin::EventChannel

   function For_Consumers
     (Self : access Object)
     return CosEventChannelAdmin.ConsumerAdmin.Ref;

   function For_Suppliers
     (Self : access Object)
     return CosEventChannelAdmin.SupplierAdmin.Ref;

   procedure Destroy
     (Self : access Object);

   ----------------------
   -- PolyORB specific --
   ----------------------

   function Create
     (Factory : CosNotifyChannelAdmin.EventChannelFactory.Ref;
      Initial_QoS   : CosNotification.QoSProperties;
      Initial_Admin : CosNotification.AdminProperties)
     return Object_Ptr;

   function GetTotalConsumers
     (Self : access Object)
     return CORBA.Long;
   --  Returns total number of proxy consumers connected to channel

   function GetTotalSuppliers
     (Self : access Object)
     return CORBA.Long;
   --  Returns total number of proxy suppliers connected to channel

   procedure Post
     (Self : access Object;
      Data : CORBA.Any);

   procedure Structured_Post
     (Self         : access Object;
      Notification : CosNotification.StructuredEvent);

   procedure Sequence_Post
     (Self          : access Object;
      Notifications : CosNotification.EventBatch);

   function TestConsumerLimit
     (Self : access Object)
     return CORBA.Boolean;
   --  Tests whether more consumers can be created by SupplierAdmin

   function TestSupplierLimit
     (Self : access Object)
     return CORBA.Boolean;
   --  Tests whether more suppliers can be created by ConsumerAdmin

private

   type Event_Channel_Record;
   type Event_Channel_Access is access Event_Channel_Record;

   type Object is new PortableServer.Servant_Base with record
      X : Event_Channel_Access;
   end record;

end CosNotifyChannelAdmin.EventChannel.Impl;
