------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         COSNOTIFYCHANNELADMIN.STRUCTUREDPROXYPUSHSUPPLIER.IMPL           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosEventChannelAdmin;

with CosNotification;
with CosNotification.Helper;

with CosNotifyChannelAdmin.StructuredProxyPushSupplier.Helper;
pragma Elaborate (CosNotifyChannelAdmin.StructuredProxyPushSupplier.Helper);
pragma Warnings
   (Off, CosNotifyChannelAdmin.StructuredProxyPushSupplier.Helper);

with CosNotifyChannelAdmin.StructuredProxyPushSupplier.Skel;
pragma Elaborate (CosNotifyChannelAdmin.StructuredProxyPushSupplier.Skel);
pragma Warnings (Off, CosNotifyChannelAdmin.StructuredProxyPushSupplier.Skel);

with PortableServer;

with PolyORB.Exceptions;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;

with PolyORB.Log;

package body CosNotifyChannelAdmin.StructuredProxyPushSupplier.Impl is

   use CosNotification;
   use IDL_SEQUENCE_CosNotification_Property;
   use IDL_SEQUENCE_CosNotification_PropertyError;
   use IDL_SEQUENCE_CosNotification_NamedPropertyRange;

   use PortableServer;

   use CORBA;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   package Convert is new
      ConsumerAdmin_Forward.Convert (CosNotifyChannelAdmin.ConsumerAdmin.Ref);

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("structuredproxypushsupplier");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Structured_Proxy_Push_Supplier_Record is record
      This       : Object_Ptr;
      Admin      : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      MyId       : CosNotifyChannelAdmin.ProxyID;
      MyType     : CosNotifyChannelAdmin.ProxyType;
      Peer       : CosNotifyComm.StructuredPushConsumer.Ref;
      QoSPropSeq : CosNotification.QoSProperties;
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

   --------------------------------------
   -- Connect_Structured_Push_consumer --
   --------------------------------------

   procedure Connect_Structured_Push_consumer
     (Self          : access Object;
      Push_Consumer : in CosNotifyComm.StructuredPushConsumer.Ref) is
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("connect_structured_push_consumer in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      if not CosNotifyComm.StructuredPushConsumer.Is_Nil (Self.X.Peer) then
         Leave (Self_Mutex);
         raise CosEventChannelAdmin.AlreadyConnected;
      end if;

      Self.X.Peer := Push_Consumer;
      Leave (Self_Mutex);
   end Connect_Structured_Push_consumer;

   ------------------------
   -- Suspend_Connection --
   ------------------------

   procedure Suspend_Connection
     (Self : access Object)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      Ensure_Initialization;
      pragma Debug (O ("suspend_connection in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Suspend_Connection;

   -----------------------
   -- Resume_Connection --
   -----------------------

   procedure Resume_Connection
     (Self : access Object)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      Ensure_Initialization;
      pragma Debug (O ("resume_connection in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Resume_Connection;

   ----------------
   -- Get_MyType --
   ----------------

   function Get_MyType
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyType
   is
      MyType : CosNotifyChannelAdmin.ProxyType;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_mytype in structuredproxypushsupplier"));

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
     return CosNotifyChannelAdmin.ConsumerAdmin_Forward.Ref
   is
      MyAdmin : CosNotifyChannelAdmin.ConsumerAdmin_Forward.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_myadmin in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      MyAdmin := Convert.To_Forward (Self.X.Admin);
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
      Ensure_Initialization;
      pragma Debug (O ("get_priority_filter in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Get_Priority_Filter;

   -------------------------
   -- Set_Priority_Filter --
   -------------------------

   procedure Set_Priority_Filter
     (Self : access Object;
      To   : in CosNotifyFilter.MappingFilter.Ref)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, To);
      pragma Warnings (On);  --  WAG:3.14
   begin
      Ensure_Initialization;
      pragma Debug (O ("set_priority_filter in structuredproxypushsupplier"));

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
      Ensure_Initialization;
      pragma Debug (O ("get_lifetime_filter in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Get_Lifetime_Filter;

   -------------------------
   -- Set_Lifetime_Filter --
   -------------------------

   procedure Set_Lifetime_Filter
     (Self : access Object;
      To   : in CosNotifyFilter.MappingFilter.Ref)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, To);
      pragma Warnings (On);  --  WAG:3.14
   begin
      Ensure_Initialization;
      pragma Debug (O ("set_lifetime_filter in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Set_Lifetime_Filter;

   --------------------------
   -- Obtain_Offered_Types --
   --------------------------

   function Obtain_Offered_Types
     (Self : access Object;
      Mode : in CosNotifyChannelAdmin.ObtainInfoMode)
     return CosNotification.EventTypeSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Mode);
      pragma Warnings (On);  --  WAG:3.14
      MySeq : CosNotification.EventTypeSeq;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_offered_types in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Obtain_Offered_Types;

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
      Ensure_Initialization;
      pragma Debug (O ("validate_event_qos in structuredproxypushsupplier"));

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
      MyQoS : CosNotification.QoSProperties;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_qos in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      MyQoS := Self.X.QoSPropSeq;
      Leave (Self_Mutex);

      return MyQoS;
   end Get_QoS;

   -------------
   -- Set_QoS --
   -------------

   procedure Set_QoS
     (Self : access Object;
      QoS  : in CosNotification.QoSProperties)
   is
      MyProp     : CosNotification.Property;
      MyError    : CosNotification.PropertyError;
      MyErrCode  : CosNotification.QoSError_code;
      MyRange    : CosNotification.PropertyRange;
      MyErrorSeq : CosNotification.PropertyErrorSeq;
      SeqLen     : Integer;
   begin
      Ensure_Initialization;
      pragma Debug (O ("set_qos in structuredproxypushsupplier"));

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
            if CORBA.Short'(From_Any (MyProp.value)) < -32767
              or else CORBA.Short'(From_Any (MyProp.value)) > 32767
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
         declare
            Members : CORBA.IDL_Exception_Members'Class
                    := UnsupportedQoS_Members'(qos_err => MyErrorSeq);
         begin
            PolyORB.Exceptions.User_Raise_Exception
              (UnsupportedQoS'Identity, Members);
         end;
      end if;

      SeqLen := Length (QoS);
      Enter (Self_Mutex);
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
      MyProp       : CosNotification.Property;
      MyError      : CosNotification.PropertyError;
      MyErrCode    : CosNotification.QoSError_code;
      MyNamedRange : CosNotification.NamedPropertyRange;
      MyRange      : CosNotification.PropertyRange;
      MyErrorSeq   : CosNotification.PropertyErrorSeq;
      SeqLen       : Integer;
   begin
      Ensure_Initialization;
      pragma Debug (O ("validate_qos in structuredproxypushsupplier"));

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
            if CORBA.Short'(From_Any (MyProp.value)) < -32767
              or else CORBA.Short'(From_Any (MyProp.value)) > 32767
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
         declare
            Members : CORBA.IDL_Exception_Members'Class
                    := UnsupportedQoS_Members'(qos_err => MyErrorSeq);
         begin
            PolyORB.Exceptions.User_Raise_Exception
              (UnsupportedQoS'Identity, Members);
         end;
      end if;

      Enter (Self_Mutex);
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
      Ensure_Initialization;
      pragma Debug (O ("add_filter in structuredproxypushsupplier"));

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
      Ensure_Initialization;
      pragma Debug (O ("remove_filter in structuredproxypushsupplier"));

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
      Ensure_Initialization;
      pragma Debug (O ("get_filter in structuredproxypushsupplier"));

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
      Ensure_Initialization;
      pragma Debug (O ("get_all_filters in structuredproxypushsupplier"));

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
      Ensure_Initialization;
      pragma Debug (O ("remove_all_filters in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);
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
      Ensure_Initialization;
      pragma Debug (O ("subscription_change in structuredproxypushsupplier"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Subscription_Change;

   -----------------------------------------
   -- Disconnect_Structured_Push_Supplier --
   -----------------------------------------

   procedure Disconnect_Structured_Push_Supplier
     (Self : access Object)
   is
      Peer    : CosNotifyComm.StructuredPushConsumer.Ref;
      Nil_Ref : CosNotifyComm.StructuredPushConsumer.Ref;
   begin
      pragma Debug (O ("disconnect structuredproxypushsupplier"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Self_Mutex);

      if not CosNotifyComm.StructuredPushConsumer.Is_Nil (Peer) then
         CosNotifyComm.StructuredPushConsumer.
            disconnect_structured_push_consumer (Peer);
      end if;
   end Disconnect_Structured_Push_Supplier;

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
      My_Ref   : StructuredProxyPushSupplier.Ref;
   begin
      pragma Debug (O ("create structuredproxypushsupplier"));

      Supplier              := new Object;
      Supplier.X            := new Structured_Proxy_Push_Supplier_Record;
      Supplier.X.Admin      := Admin;
      Supplier.X.MyId       := Proxy_Id;
      Supplier.X.MyType     := Ptype;
      Supplier.X.This       := Supplier;
      Supplier.X.QoSPropSeq := Initial_QoS;

      Initiate_Servant (Servant (Supplier), My_Ref);
      return Supplier;
   end Create;

   ---------------------
   -- Structured_Post --
   ---------------------

   procedure Structured_Post
     (Self         : access Object;
      Notification : in CosNotification.StructuredEvent)
   is
      MyPeer : CosNotifyComm.StructuredPushConsumer.Ref;
   begin
      pragma Debug
         (O ("post new structured event from structuredproxypushsupplier" &
             "to structured pushconsumer"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyPeer := Self.X.Peer;
      Leave (Self_Mutex);

      begin
         CosNotifyComm.StructuredPushConsumer.push_structured_event
           (MyPeer, Notification);
      exception
         when others =>
            pragma Debug
              (O ("Got exception in post at structuredproxypushsupplier"));
            raise;
      end;
   end Structured_Post;

end CosNotifyChannelAdmin.StructuredProxyPushSupplier.Impl;
