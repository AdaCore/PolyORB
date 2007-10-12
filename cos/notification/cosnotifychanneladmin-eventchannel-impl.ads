------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  COSNOTIFYCHANNELADMIN.EVENTCHANNEL.IMPL                 --
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
       Op      : in CosNotifyChannelAdmin.InterFilterGroupOperator;
       Id      : out CosNotifyChannelAdmin.AdminID;
       Returns : out CosNotifyChannelAdmin.ConsumerAdmin.Ref);

   procedure New_For_Suppliers
      (Self    : access Object;
       Op      : in CosNotifyChannelAdmin.InterFilterGroupOperator;
       Id      : out CosNotifyChannelAdmin.AdminID;
       Returns : out CosNotifyChannelAdmin.SupplierAdmin.Ref);

   function Get_ConsumerAdmin
      (Self : access Object;
       Id   : in CosNotifyChannelAdmin.AdminID)
       return CosNotifyChannelAdmin.ConsumerAdmin.Ref;

   function Get_SupplierAdmin
      (Self : access Object;
       Id   : in CosNotifyChannelAdmin.AdminID)
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
       QoS  : in CosNotification.QoSProperties);

   procedure Validate_QoS
      (Self         : access Object;
      Required_QoS  : in CosNotification.QoSProperties;
      Available_QoS : out CosNotification.NamedPropertyRangeSeq);

   --  IDL Operations inherited from CosNotification::AdminPropertiesAdmin

   function Get_Admin
     (Self : access Object)
     return CosNotification.AdminProperties;

   procedure Set_Admin
      (Self  : access Object;
       Admin : in CosNotification.AdminProperties);

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
      Initial_QoS   : in CosNotification.QoSProperties;
      Initial_Admin : in CosNotification.AdminProperties)
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
      Data : in CORBA.Any);

   procedure Structured_Post
     (Self         : access Object;
      Notification : in CosNotification.StructuredEvent);

   procedure Sequence_Post
     (Self          : access Object;
      Notifications : in CosNotification.EventBatch);

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
