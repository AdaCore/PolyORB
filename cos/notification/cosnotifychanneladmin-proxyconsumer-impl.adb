------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCHANNELADMIN.PROXYCONSUMER.IMPL                 --
--                                                                          --
--                                 B o d y                                  --
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

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosNotifyChannelAdmin.ProxyConsumer.Helper;
pragma Elaborate (CosNotifyChannelAdmin.ProxyConsumer.Helper);
pragma Warnings (Off, CosNotifyChannelAdmin.ProxyConsumer.Helper);

with CosNotifyChannelAdmin.ProxyConsumer.Skel;
pragma Elaborate (CosNotifyChannelAdmin.ProxyConsumer.Skel);
pragma Warnings (Off, CosNotifyChannelAdmin.ProxyConsumer.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
--  with PolyORB.Tasking.Semaphores;
with PolyORB.Log;

package body CosNotifyChannelAdmin.ProxyConsumer.Impl is

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   --  use PolyORB.Tasking.Semaphores;

   package Convert is new
      SupplierAdmin_Forward.Convert (CosNotifyChannelAdmin.SupplierAdmin.Ref);

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("proxyconsumer");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Proxy_Consumer_Record is record
      This   : Object_Ptr;
      Admin  : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      MyId   : CosNotifyChannelAdmin.ProxyID;
      MyType : CosNotifyChannelAdmin.ProxyType;
   end record;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

   T_Initialized : Boolean := False;
   Self_Mutex : Mutex_Access;

   procedure Ensure_Initialization is
   begin
      if not T_Initialized then
         Create (Self_Mutex);
         T_Initialized := True;
      end if;
   end Ensure_Initialization;

   ----------------
   -- Get_MyType --
   ----------------

   function Get_MyType
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyType
   is
      MyType : CosNotifyChannelAdmin.ProxyType;
   begin
      pragma Debug (O ("get_mytype in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyType := Self.X.MyType;
      Leave (Self_Mutex);

      return MyType;
   end Get_MyType;

   -----------------
   -- Get_MyAdmin --
   -----------------

   function Get_MyAdmin
     (Self : access Object)
     return CosNotifyChannelAdmin.SupplierAdmin_Forward.Ref
   is
      MyAdmin : CosNotifyChannelAdmin.SupplierAdmin_Forward.Ref;
   begin
      pragma Debug (O ("get_myadmin in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyAdmin := Convert.To_Forward (Self.X.Admin);
      Leave (Self_Mutex);

      return MyAdmin;
   end Get_MyAdmin;

   -------------------------------
   -- Obtain_Subscription_Types --
   -------------------------------

   function Obtain_Subscription_Types
     (Self : access Object;
      Mode : in CosNotifyChannelAdmin.ObtainInfoMode)
     return CosNotification.EventTypeSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Mode);
      pragma Warnings (On);  --  WAG:3.14
      MySeq : CosNotification.EventTypeSeq;
   begin
      pragma Debug (O ("obtain_subscription_types in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Obtain_Subscription_Types;

   ------------------------
   -- Validate_Event_QoS --
   ------------------------

   procedure Validate_Event_QoS
     (Self          : access Object;
      Required_QoS  : in CosNotification.QoSProperties;
      Available_QoS : out CosNotification.NamedPropertyRangeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Required_QoS, Available_QoS);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("validate_event_qos in proxyconsumer"));
      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Validate_Event_QoS;

   -------------
   -- Get_QoS --
   -------------

   function Get_QoS
      (Self : access Object)
      return CosNotification.QoSProperties
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyProp : CosNotification.QoSProperties;
   begin
      pragma Debug (O ("get_qos in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyProp;
   end Get_QoS;

   -------------
   -- Set_QoS --
   -------------

   procedure Set_QoS
     (Self : access Object;
      QoS  : in CosNotification.QoSProperties)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, QoS);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("set_qos in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Set_QoS;

   ------------------
   -- Validate_QoS --
   ------------------

   procedure Validate_QoS
      (Self         : access Object;
       Required_QoS  : in CosNotification.QoSProperties;
       Available_QoS : out CosNotification.NamedPropertyRangeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Required_QoS, Available_QoS);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("validate_qos in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Validate_QoS;

   ----------------
   -- Add_Filter --
   ----------------

   function Add_Filter
     (Self       : access Object;
      New_Filter : in CosNotifyFilter.Filter.Ref)
     return CosNotifyFilter.FilterID
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, New_Filter);
      pragma Warnings (On);  --  WAG:3.14
      MyFilterID : CosNotifyFilter.FilterID;
      MyID       : CORBA.Long;
   begin
      pragma Debug (O ("add_filter in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      MyID := 0;
      MyFilterID := CosNotifyFilter.FilterID (MyID);
      return MyFilterID;
   end Add_Filter;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
     (Self   : access Object;
      Filter : in CosNotifyFilter.FilterID)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filter);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("remove_filter in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Remove_Filter;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
     (Self   : access Object;
      Filter : in CosNotifyFilter.FilterID)
     return CosNotifyFilter.Filter.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filter);
      pragma Warnings (On);  --  WAG:3.14
      MyFilter : CosNotifyFilter.Filter.Ref;
   begin
      pragma Debug (O ("get_filter in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Get_Filter;

   ---------------------
   -- Get_All_Filters --
   ---------------------

   function Get_All_Filters
     (Self : access Object)
     return CosNotifyFilter.FilterIDSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyFilterSeq : CosNotifyFilter.FilterIDSeq;
   begin
      pragma Debug (O ("get_all_filters in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilterSeq;
   end Get_All_Filters;

   ------------------------
   -- Remove_All_Filters --
   ------------------------

   procedure Remove_All_Filters
     (Self : access Object)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("remove_all_filters in proxyconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Remove_All_Filters;

   ------------
   -- Create --
   ------------

   function Create
     (Admin    : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      Ptype    : CosNotifyChannelAdmin.ProxyType;
      Proxy_Id : CosNotifyChannelAdmin.ProxyID)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : ProxyConsumer.Ref;
   begin
      pragma Debug (O ("create proxyconsumer"));

      Consumer          := new Object;
      Consumer.X        := new Proxy_Consumer_Record;
      Consumer.X.Admin  := Admin;
      Consumer.X.MyId   := Proxy_Id;
      Consumer.X.MyType := Ptype;
      Consumer.X.This   := Consumer;
      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

end CosNotifyChannelAdmin.ProxyConsumer.Impl;
