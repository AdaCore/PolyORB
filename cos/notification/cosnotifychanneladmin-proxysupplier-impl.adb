------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCHANNELADMIN.PROXYSUPPLIER.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosNotifyChannelAdmin.ProxySupplier.Skel;
pragma Warnings (Off, CosNotifyChannelAdmin.ProxySupplier.Skel);

package body CosNotifyChannelAdmin.ProxySupplier.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("proxysupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Proxy_Supplier_Record is record
      This    : Object_Ptr;
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
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyType : CosNotifyChannelAdmin.ProxyType;
   begin
      pragma Debug (O ("get_mytype in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      MyType := PUSH_ANY;
      return MyType;
   end Get_MyType;

   -----------------
   -- Get_MyAdmin --
   -----------------

   function Get_MyAdmin
     (Self : access Object)
     return CosNotifyChannelAdmin.ConsumerAdmin_Forward.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyAdmin : CosNotifyChannelAdmin.ConsumerAdmin_Forward.Ref;
   begin
      pragma Debug (O ("get_myadmin in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyAdmin;
   end Get_MyAdmin;

   -------------------------
   -- Get_Priority_Filter --
   -------------------------

   function Get_Priority_Filter
     (Self : access Object)
     return CosNotifyFilter.MappingFilter.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyFilter : CosNotifyFilter.MappingFilter.Ref;
   begin
      pragma Debug (O ("get_priority_filter in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Get_Priority_Filter;

   -------------------------
   -- Set_Priority_Filter --
   -------------------------

   procedure Set_Priority_Filter
     (Self : access Object;
      To   : CosNotifyFilter.MappingFilter.Ref)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, To);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("set_priority_filter in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Set_Priority_Filter;

   -------------------------
   -- Get_Lifetime_Filter --
   -------------------------

   function Get_Lifetime_Filter
     (Self : access Object)
     return CosNotifyFilter.MappingFilter.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyFilter : CosNotifyFilter.MappingFilter.Ref;
   begin
      pragma Debug (O ("get_lifetime_filter in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Get_Lifetime_Filter;

   -------------------------
   -- Set_Lifetime_Filter --
   -------------------------

   procedure Set_Lifetime_Filter
     (Self : access Object;
      To   : CosNotifyFilter.MappingFilter.Ref)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, To);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("set_lifetime_filter in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Set_Lifetime_Filter;

   --------------------------
   -- Obtain_Offered_Types --
   --------------------------

   function Obtain_Offered_Types
     (Self : access Object;
      Mode : CosNotifyChannelAdmin.ObtainInfoMode)
     return CosNotification.EventTypeSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Mode);
      pragma Warnings (On);  --  WAG:3.14
      MySeq : CosNotification.EventTypeSeq;
   begin
      pragma Debug (O ("obtain_offered_types in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Obtain_Offered_Types;

   ------------------------
   -- Validate_Event_QoS --
   ------------------------

   procedure Validate_Event_QoS
     (Self          : access Object;
      Required_QoS  : CosNotification.QoSProperties;
      Available_QoS : out CosNotification.NamedPropertyRangeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Required_QoS, Available_QoS);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("validate_event_qos in proxysupplier"));
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
      pragma Debug (O ("get_qos in proxysupplier"));

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
      QoS  : CosNotification.QoSProperties)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, QoS);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("set_qos in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Set_QoS;

   ------------------
   -- Validate_QoS --
   ------------------

   procedure Validate_QoS
      (Self         : access Object;
       Required_QoS  : CosNotification.QoSProperties;
       Available_QoS : out CosNotification.NamedPropertyRangeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Required_QoS, Available_QoS);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("validate_qos in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Validate_QoS;

   ----------------
   -- Add_Filter --
   ----------------

   function Add_Filter
     (Self       : access Object;
      New_Filter : CosNotifyFilter.Filter.Ref)
     return CosNotifyFilter.FilterID
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, New_Filter);
      pragma Warnings (On);  --  WAG:3.14
      MyFilterID : CosNotifyFilter.FilterID;
      MyID       : CORBA.Long;
   begin
      pragma Debug (O ("add_filter in proxysupplier"));

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
      Filter : CosNotifyFilter.FilterID)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filter);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("remove_filter in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Remove_Filter;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
     (Self   : access Object;
      Filter : CosNotifyFilter.FilterID)
     return CosNotifyFilter.Filter.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filter);
      pragma Warnings (On);  --  WAG:3.14
      MyFilter : CosNotifyFilter.Filter.Ref;
   begin
      pragma Debug (O ("get_filter in proxysupplier"));

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
      pragma Debug (O ("get_all_filters in proxysupplier"));

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
      pragma Debug (O ("remove_all_filters in proxysupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Remove_All_Filters;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : ProxySupplier.Ref;
   begin
      pragma Debug (O ("create proxysupplier"));

      Supplier         := new Object;
      Supplier.X       := new Proxy_Supplier_Record;
      Supplier.X.This  := Supplier;
      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

end CosNotifyChannelAdmin.ProxySupplier.Impl;
