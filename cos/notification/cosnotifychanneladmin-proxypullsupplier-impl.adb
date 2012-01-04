------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              COSNOTIFYCHANNELADMIN.PROXYPULLSUPPLIER.IMPL                --
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

with CosEventChannelAdmin.Helper;

with CosEventComm.Helper;
with CosEventComm.PullConsumer;

with CosNotification;
with CosNotification.Helper;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Utils.Chained_Lists;

with CosNotifyChannelAdmin.ProxyPullSupplier.Skel;
pragma Warnings (Off, CosNotifyChannelAdmin.ProxyPullSupplier.Skel);

package body CosNotifyChannelAdmin.ProxyPullSupplier.Impl is

   use CosNotification;
   use IDL_SEQUENCE_CosNotification_Property;
   use IDL_SEQUENCE_CosNotification_PropertyError;
   use IDL_SEQUENCE_CosNotification_NamedPropertyRange;

   use CORBA;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   package Convert is new
      ConsumerAdmin_Forward.Convert (CosNotifyChannelAdmin.ConsumerAdmin.Ref);

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("proxypullsupplier");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package Event_Queues is new PolyORB.Utils.Chained_Lists
     (CORBA.Any, CORBA."=");
   use Event_Queues;

   subtype Event_Queue is Event_Queues.List;

   type Proxy_Pull_Supplier_Record is record
      This       : Object_Ptr;
      Admin      : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      MyId       : CosNotifyChannelAdmin.ProxyID;
      MyType     : CosNotifyChannelAdmin.ProxyType;
      Peer       : CosEventComm.PullConsumer.Ref;
      QoSPropSeq : CosNotification.QoSProperties;
      Queue      : Event_Queue;
      M          : Mutex_Access;
      CV         : Condition_Access;
   end record;

   -------------------------------
   -- Connect_Any_Pull_Consumer --
   -------------------------------

   procedure Connect_Any_Pull_Consumer
     (Self          : access Object;
      Pull_Consumer : CosEventComm.PullConsumer.Ref)
   is
   begin
      pragma Debug (O ("connect_any_pull_consumer in proxypullsupplier"));

      Enter (Self.X.M);
      if not CosEventComm.PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Pull_Consumer;
      Leave (Self.X.M);
   end Connect_Any_Pull_Consumer;

   ----------------
   -- Get_MyType --
   ----------------

   function Get_MyType
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyType
   is
      MyType : CosNotifyChannelAdmin.ProxyType;
   begin
      pragma Debug (O ("get_mytype in proxypullsupplier"));

      Enter (Self.X.M);
      MyType := Self.X.MyType;
      Leave (Self.X.M);

      return MyType;
   end Get_MyType;

   -----------------
   -- Get_MyAdmin --
   -----------------

   function Get_MyAdmin
     (Self : access Object)
     return CosNotifyChannelAdmin.ConsumerAdmin_Forward.Ref
   is
      MyAdmin : CosNotifyChannelAdmin.ConsumerAdmin_Forward.Ref;
   begin
      pragma Debug (O ("get_myadmin in proxypullsupplier"));

      Enter (Self.X.M);
      MyAdmin := Convert.To_Forward (Self.X.Admin);
      Leave (Self.X.M);

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
      pragma Debug (O ("get_priority_filter in proxypullsupplier"));
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
      pragma Debug (O ("set_priority_filter in proxypullsupplier"));
      null;
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
      pragma Debug (O ("get_lifetime_filter in proxypullsupplier"));

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
      pragma Debug (O ("set_lifetime_filter in proxypullsupplier"));
      null;
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
      pragma Debug (O ("obtain_offered_types in proxypullsupplier"));
      return MySeq;
   end Obtain_Offered_Types;

   ------------------------
   -- Validate_Event_QoS --
   ------------------------

   procedure Validate_Event_QoS
     (Self          : access Object;
      Required_QoS  : CosNotification.QoSProperties;
      Available_QoS :    out CosNotification.NamedPropertyRangeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Required_QoS, Available_QoS);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("validate_event_qos in proxypullsupplier"));
      null;
   end Validate_Event_QoS;

   -------------
   -- Get_QoS --
   -------------

   function Get_QoS
      (Self : access Object)
      return CosNotification.QoSProperties
   is
      MyQoS : CosNotification.QoSProperties;
   begin
      pragma Debug (O ("get_qos in proxypullsupplier"));

      Enter (Self.X.M);
      MyQoS := Self.X.QoSPropSeq;
      Leave (Self.X.M);

      return MyQoS;
   end Get_QoS;

   -------------
   -- Set_QoS --
   -------------

   procedure Set_QoS
     (Self : access Object;
      QoS  : CosNotification.QoSProperties)
   is
      MyProp     : CosNotification.Property;
      MyError    : CosNotification.PropertyError;
      MyErrCode  : CosNotification.QoSError_code;
      MyRange    : CosNotification.PropertyRange;
      MyErrorSeq : CosNotification.PropertyErrorSeq;
      SeqLen     : Integer;
   begin
      pragma Debug (O ("set_qos in proxypullsupplier"));

      SeqLen := Length (QoS);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (QoS, Index);
         if MyProp.name = "EventReliability" then
               MyErrCode := UNAVAILABLE_PROPERTY;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "ConnectionReliability" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "Priority" then
            if CORBA.Short'(From_Any (MyProp.value))
              not in -32_767 .. 32_767
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (-32767)),
                             To_Any (CORBA.Short (32767)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "StartTime" then
               MyErrCode := UNAVAILABLE_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StopTime" then
               MyErrCode := UNAVAILABLE_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "Timeout" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "OrderPolicy" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
              and then CORBA.Short'(From_Any (MyProp.value)) /= 2
              and then CORBA.Short'(From_Any (MyProp.value)) /= 3
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (3)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "DiscardPolicy" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
              and then CORBA.Short'(From_Any (MyProp.value)) /= 2
              and then CORBA.Short'(From_Any (MyProp.value)) /= 3
              and then CORBA.Short'(From_Any (MyProp.value)) /= 4
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (4)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "MaximumBatchSize" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "PacingInterval" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StartTimeSupported" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StopTimeSupported" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "MaxEventsPerConsumer" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         else
            MyErrCode := BAD_PROPERTY;
            MyError   := (MyErrCode, MyProp.name, MyRange);
            Append (MyErrorSeq, MyError);
         end if;
      end loop;

      if Length (MyErrorSeq) > 0 then
         CosNotification.Helper.Raise_UnsupportedQoS
           ((CORBA.IDL_Exception_Members with qos_err => MyErrorSeq));
      end if;

      SeqLen := Length (QoS);
      Enter (Self.X.M);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (QoS, Index);
         if MyProp.name = "ConnectionReliability" then
            Replace_Element (Self.X.QoSPropSeq, 2, MyProp);
         elsif MyProp.name = "Priority" then
            Replace_Element (Self.X.QoSPropSeq, 3, MyProp);
         elsif MyProp.name = "OrderPolicy" then
            Replace_Element (Self.X.QoSPropSeq, 4, MyProp);
         elsif MyProp.name = "DiscardPolicy" then
            Replace_Element (Self.X.QoSPropSeq, 5, MyProp);
         end if;
      end loop;
      Leave (Self.X.M);

   end Set_QoS;

   ------------------
   -- Validate_QoS --
   ------------------

   procedure Validate_QoS
     (Self          : access Object;
      Required_QoS  : CosNotification.QoSProperties;
      Available_QoS :    out CosNotification.NamedPropertyRangeSeq)
   is
      MyProp       : CosNotification.Property;
      MyError      : CosNotification.PropertyError;
      MyErrCode    : CosNotification.QoSError_code;
      MyNamedRange : CosNotification.NamedPropertyRange;
      MyRange      : CosNotification.PropertyRange;
      MyErrorSeq   : CosNotification.PropertyErrorSeq;
      SeqLen       : Integer;
   begin
      pragma Debug (O ("validate_qos in proxypullsupplier"));

      SeqLen := Length (Required_QoS);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Required_QoS, Index);
         if MyProp.name = "EventReliability" then
               MyErrCode := UNAVAILABLE_PROPERTY;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "ConnectionReliability" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "Priority" then
            if CORBA.Short'(From_Any (MyProp.value))
              not in -32_767 .. 32_767
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (-32767)),
                             To_Any (CORBA.Short (32767)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "StartTime" then
               MyErrCode := UNAVAILABLE_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StopTime" then
               MyErrCode := UNAVAILABLE_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "Timeout" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "OrderPolicy" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
              and then CORBA.Short'(From_Any (MyProp.value)) /= 2
              and then CORBA.Short'(From_Any (MyProp.value)) /= 3
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (3)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "DiscardPolicy" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
              and then CORBA.Short'(From_Any (MyProp.value)) /= 2
              and then CORBA.Short'(From_Any (MyProp.value)) /= 3
              and then CORBA.Short'(From_Any (MyProp.value)) /= 4
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (4)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "MaximumBatchSize" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "PacingInterval" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StartTimeSupported" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StopTimeSupported" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "MaxEventsPerConsumer" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         else
            MyErrCode := BAD_PROPERTY;
            MyError   := (MyErrCode, MyProp.name, MyRange);
            Append (MyErrorSeq, MyError);
         end if;
      end loop;

      if Length (MyErrorSeq) > 0 then
         CosNotification.Helper.Raise_UnsupportedQoS
           ((CORBA.IDL_Exception_Members with qos_err => MyErrorSeq));
      end if;

      Enter (Self.X.M);
      SeqLen := Length (Self.X.QoSPropSeq);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Self.X.QoSPropSeq, Index);
         if MyProp.name = "ConnectionReliability" then
               MyRange      := (From_Any (MyProp.value),
                                To_Any (CORBA.Short (0)));
               MyNamedRange := (MyProp.name, MyRange);
               Append (Available_QoS, MyNamedRange);
         elsif MyProp.name = "Priority" then
               MyRange      := (To_Any (CORBA.Short (-32767)),
                                To_Any (CORBA.Short (32767)));
               MyNamedRange := (MyProp.name, MyRange);
               Append (Available_QoS, MyNamedRange);
         elsif MyProp.name = "OrderPolicy" then
               MyRange      := (To_Any (CORBA.Short (0)),
                                To_Any (CORBA.Short (3)));
               MyNamedRange := (MyProp.name, MyRange);
               Append (Available_QoS, MyNamedRange);
         elsif MyProp.name = "DiscardPolicy" then
               MyRange      := (To_Any (CORBA.Short (0)),
                                To_Any (CORBA.Short (4)));
               MyNamedRange := (MyProp.name, MyRange);
               Append (Available_QoS, MyNamedRange);
         end if;
      end loop;
      Leave (Self.X.M);
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
      pragma Debug (O ("add_filter in proxypullsupplier"));

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
      pragma Debug (O ("remove_filter in proxypullsupplier"));
      null;
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
      pragma Debug (O ("get_filter in proxypullsupplier"));
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
      pragma Debug (O ("get_all_filters in proxypullsupplier"));
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
      pragma Debug (O ("remove_all_filters in proxypullsupplier"));
      null;
   end Remove_All_Filters;

   -------------------------
   -- Subscription_Change --
   -------------------------

   procedure Subscription_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Added, Removed);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("subscription_change in proxypullsupplier"));
      null;
   end Subscription_Change;

   ----------
   -- Pull --
   ----------

   function Pull (Self : access Object) return CORBA.Any is
      Event : CORBA.Any;
   begin
      pragma Debug (O ("attempt to pull new data from proxy pull supplier"));

      Enter (Self.X.M);

      loop
         if CosEventComm.PullConsumer.Is_Nil (Self.X.Peer) then
            Leave (Self.X.M);
            CosEventComm.Helper.Raise_Disconnected
              ((CORBA.IDL_Exception_Members with null record));
         end if;

         exit when Length (Self.X.Queue) > 0;

         Wait (Self.X.CV, Self.X.M);
      end loop;

      Extract_First (Self.X.Queue, Event);
      Leave (Self.X.M);
      return Event;
   end Pull;

   --------------
   -- Try_Pull --
   --------------

   procedure Try_Pull
     (Self      : access Object;
      Has_Event :    out CORBA.Boolean;
      Returns   :    out CORBA.Any)
   is
   begin
      pragma Debug (O ("try to pull new data from proxy pull supplier"));

      Enter (Self.X.M);

      if CosEventComm.PullConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self.X.M);
         CosEventComm.Helper.Raise_Disconnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Has_Event := Length (Self.X.Queue) > 0;

      if Has_Event then
         Extract_First (Self.X.Queue, Returns);
      end if;

      Leave (Self.X.M);
   end Try_Pull;

   ------------------------------
   -- Disconnect_Pull_Supplier --
   ------------------------------

   procedure Disconnect_Pull_Supplier
     (Self : access Object)
   is
      Peer    : CosEventComm.PullConsumer.Ref;
      Nil_Ref : CosEventComm.PullConsumer.Ref;
   begin
      pragma Debug (O ("disconnect proxypullsupplier"));

      Enter (Self.X.M);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self.X.M);
      Signal (Self.X.CV);

      if not CosEventComm.PullConsumer.Is_Nil (Peer) then
         CosEventComm.PullConsumer.disconnect_pull_consumer (Peer);
      end if;
   end Disconnect_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create
     (Admin       : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      Initial_QoS : CosNotification.QoSProperties;
      Ptype       : CosNotifyChannelAdmin.ProxyType;
      Proxy_Id    : CosNotifyChannelAdmin.ProxyID)
      return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : ProxyPullSupplier.Ref;
   begin
      pragma Debug (O ("create proxy pullsupplier"));

      Supplier              := new Object;
      Supplier.X            := new Proxy_Pull_Supplier_Record;
      Supplier.X.Admin      := Admin;
      Supplier.X.MyId       := Proxy_Id;
      Supplier.X.MyType     := Ptype;
      Supplier.X.This       := Supplier;
      Supplier.X.QoSPropSeq := Initial_QoS;
      Create (Supplier.X.M);
      Create (Supplier.X.CV);

      Initiate_Servant (PortableServer.Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : CORBA.Any)
   is
   begin
      pragma Debug (O ("post new data to proxy pull supplier"));

      Enter (Self.X.M);
      Append (Self.X.Queue, Data);
      Leave (Self.X.M);
      Signal (Self.X.CV);
   end Post;

end CosNotifyChannelAdmin.ProxyPullSupplier.Impl;
