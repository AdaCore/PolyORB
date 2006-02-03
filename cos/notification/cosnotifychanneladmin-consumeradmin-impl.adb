------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCHANNELADMIN.CONSUMERADMIN.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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

with CosNotification;
with CosNotification.Helper;

with CosNotifyChannelAdmin.EventChannel.Impl;

with CosNotifyChannelAdmin.ProxyPushSupplier.Impl;
with CosNotifyChannelAdmin.ProxyPushSupplier.Helper;

with CosNotifyChannelAdmin.ProxyPullSupplier.Impl;
with CosNotifyChannelAdmin.ProxyPullSupplier.Helper;

with CosNotifyChannelAdmin.SequenceProxyPullSupplier.Impl;
with CosNotifyChannelAdmin.SequenceProxyPullSupplier.Helper;

with CosNotifyChannelAdmin.SequenceProxyPushSupplier.Impl;
with CosNotifyChannelAdmin.SequenceProxyPushSupplier.Helper;

with CosNotifyChannelAdmin.StructuredProxyPullSupplier.Impl;
with CosNotifyChannelAdmin.StructuredProxyPullSupplier.Helper;

with CosNotifyChannelAdmin.StructuredProxyPushSupplier.Impl;
with CosNotifyChannelAdmin.StructuredProxyPushSupplier.Helper;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosNotifyChannelAdmin.ConsumerAdmin.Skel;
pragma Warnings (Off, CosNotifyChannelAdmin.ConsumerAdmin.Skel);

package body CosNotifyChannelAdmin.ConsumerAdmin.Impl is

   use IDL_SEQUENCE_CosNotifyChannelAdmin_ProxyID;

   use CosNotification;
   use IDL_SEQUENCE_CosNotification_Property;
   use IDL_SEQUENCE_CosNotification_PropertyError;
   use IDL_SEQUENCE_CosNotification_NamedPropertyRange;

   use IDL_SEQUENCE_CosNotification_StructuredEvent;

   use CORBA;
   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   package Convert is new
      EventChannel_Forward.Convert (CosNotifyChannelAdmin.EventChannel.Ref);

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("consumeradmin");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package AllProxies is new
      CORBA.Sequences.Unbounded (CORBA.Long);

   package PullSuppliers is new
      CORBA.Sequences.Unbounded (CosNotifyChannelAdmin.ProxyPullSupplier.Ref);

   package PushSuppliers is new
      CORBA.Sequences.Unbounded (CosNotifyChannelAdmin.ProxyPushSupplier.Ref);

   package SequencePullSuppliers is new CORBA.Sequences.Unbounded
      (CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref);

   package SequencePushSuppliers is new CORBA.Sequences.Unbounded
      (CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref);

   package StructuredPullSuppliers is new CORBA.Sequences.Unbounded
      (CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref);

   package StructuredPushSuppliers is new CORBA.Sequences.Unbounded
      (CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref);

   type Consumer_Admin_Record is record
      This          : Object_Ptr;
      Channel       : CosNotifyChannelAdmin.EventChannel.Ref;
      Id            : CosNotifyChannelAdmin.AdminID;
      Op            : CosNotifyChannelAdmin.InterFilterGroupOperator;
      AllPxs        : AllProxies.Sequence;
      Pulls         : PullSuppliers.Sequence;
      Pushs         : PushSuppliers.Sequence;
      SequencePulls : SequencePullSuppliers.Sequence;
      SequencePushs : SequencePushSuppliers.Sequence;
      StructPulls   : StructuredPullSuppliers.Sequence;
      StructPushs   : StructuredPushSuppliers.Sequence;
      PullIDSeq     : CosNotifyChannelAdmin.ProxyIDSeq;
      PushIDSeq     : CosNotifyChannelAdmin.ProxyIDSeq;
      QoSPropSeq    : CosNotification.QoSProperties;
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

   --------------
   -- Get_MyID --
   --------------

   function Get_MyID
     (Self : access Object)
     return CosNotifyChannelAdmin.AdminID
   is
      MyID : CosNotifyChannelAdmin.AdminID;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_myid in consumeradmin"));

      Enter (Self_Mutex);
      MyID := Self.X.Id;
      Leave (Self_Mutex);

      return MyID;
   end Get_MyID;

   -------------------
   -- Get_MyChannel --
   -------------------

   function Get_MyChannel
     (Self : access Object)
     return CosNotifyChannelAdmin.EventChannel_Forward.Ref
   is
      MyChannel : CosNotifyChannelAdmin.EventChannel_Forward.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_mychannel in consumeradmin"));

      Enter (Self_Mutex);
      MyChannel := Convert.To_Forward (Self.X.Channel);
      Leave (Self_Mutex);

      return MyChannel;
   end Get_MyChannel;

   --------------------
   -- Get_MyOperator --
   --------------------

   function Get_MyOperator
     (Self : access Object)
     return CosNotifyChannelAdmin.InterFilterGroupOperator
   is
      MyOperator : CosNotifyChannelAdmin.InterFilterGroupOperator;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_myoperator in consumeradmin"));

      Enter (Self_Mutex);
      MyOperator := Self.X.Op;
      Leave (Self_Mutex);

      return MyOperator;
   end Get_MyOperator;

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
      pragma Debug (O ("get_priority_filter in consumeradmin"));

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
      Ensure_Initialization;
      pragma Debug (O ("set_priority_filter in consumeradmin"));

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
      pragma Debug (O ("get_lifetime_filter in consumeradmin"));

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
      Ensure_Initialization;
      pragma Debug (O ("set_lifetime_filter in consumeradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Set_Lifetime_Filter;

   -------------------------
   -- Get_Pull_Suppliers --
   -------------------------

   function Get_Pull_Suppliers
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyIDSeq
   is
      MySeq : CosNotifyChannelAdmin.ProxyIDSeq;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_pull_suppliers in consumeradmin"));

      Enter (Self_Mutex);
      MySeq := Self.X.PullIDSeq;
      Leave (Self_Mutex);

      return MySeq;
   end Get_Pull_Suppliers;

   -------------------------
   -- Get_Push_Suppliers --
   -------------------------

   function Get_Push_Suppliers
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyIDSeq
   is
      MySeq : CosNotifyChannelAdmin.ProxyIDSeq;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_push_suppliers in consumeradmin"));

      Enter (Self_Mutex);
      MySeq := Self.X.PushIDSeq;
      Leave (Self_Mutex);

      return MySeq;
   end Get_Push_Suppliers;

   ------------------------
   -- Get_Proxy_Supplier --
   ------------------------

   function Get_Proxy_Supplier
     (Self       : access Object;
      Proxy_Id   : CosNotifyChannelAdmin.ProxyID)
     return CosNotifyChannelAdmin.ProxySupplier.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Proxy_Id);
      pragma Warnings (On);  --  WAG:3.14
      MySupplier : CosNotifyChannelAdmin.ProxySupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_proxy_supplier in consumeradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySupplier;
   end Get_Proxy_Supplier;

   ---------------------------------------
   -- Obtain_Notification_Pull_Supplier --
   ---------------------------------------

   procedure Obtain_Notification_Pull_Supplier
     (Self     : access Object;
      Ctype    : CosNotifyChannelAdmin.ClientType;
      Proxy_Id :    out CosNotifyChannelAdmin.ProxyID;
      Returns  :    out CosNotifyChannelAdmin.ProxySupplier.Ref)
   is
      Channel         : CosNotifyChannelAdmin.EventChannel.Impl.Object_Ptr;
      MyRef           : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      Ptype           : CosNotifyChannelAdmin.ProxyType;
      Res             : CORBA.Boolean;
      Supplier        : CosNotifyChannelAdmin.ProxyPullSupplier.
                        Impl.Object_Ptr;
      Seq_Supplier    : CosNotifyChannelAdmin.SequenceProxyPullSupplier.
                        Impl.Object_Ptr;
      Struct_Supplier : CosNotifyChannelAdmin.StructuredProxyPullSupplier.
                        Impl.Object_Ptr;
      SRef            : CosNotifyChannelAdmin.ProxyPullSupplier.Ref;
      Seq_SRef        : CosNotifyChannelAdmin.SequenceProxyPullSupplier.Ref;
      Struct_SRef     : CosNotifyChannelAdmin.StructuredProxyPullSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_notification_pull_supplier in consumeradmin"));

      Enter (Self_Mutex);
      Reference_To_Servant (Self.X.Channel, Servant (Channel));
      Res := CosNotifyChannelAdmin.EventChannel.Impl.
             TestSupplierLimit (Channel);

      if Res = False then
         Leave (Self_Mutex);
         raise AdminLimitExceeded;
      end if;

      case Ctype is
         when ANY_EVENT =>
            Ptype := PULL_ANY;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);
            Supplier := CosNotifyChannelAdmin.ProxyPullSupplier.Impl.Create
                        (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);
            Servant_To_Reference (Servant (Supplier), Returns);
            SRef := CosNotifyChannelAdmin.ProxyPullSupplier.Helper.To_Ref
                    (Returns);
            PullSuppliers.Append (Self.X.Pulls, SRef);
            Append (Self.X.PullIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

         when STRUCTURED_EVENT =>
            Ptype := PULL_STRUCTURED;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);

            Struct_Supplier :=
            CosNotifyChannelAdmin.StructuredProxyPullSupplier.Impl.Create
            (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);

            Servant_To_Reference (Servant (Struct_Supplier), Returns);
            Struct_SRef := CosNotifyChannelAdmin.StructuredProxyPullSupplier.
                           Helper.To_Ref (Returns);
            StructuredPullSuppliers.Append (Self.X.StructPulls, Struct_SRef);
            Append (Self.X.PullIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

         when SEQUENCE_EVENT =>
            Ptype := PULL_SEQUENCE;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);

            Seq_Supplier :=
            CosNotifyChannelAdmin.SequenceProxyPullSupplier.Impl.Create
            (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);

            Servant_To_Reference (Servant (Seq_Supplier), Returns);
            Seq_SRef := CosNotifyChannelAdmin.SequenceProxyPullSupplier.
                           Helper.To_Ref (Returns);
            SequencePullSuppliers.Append (Self.X.SequencePulls, Seq_SRef);
            Append (Self.X.PullIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

      end case;

      Leave (Self_Mutex);

   end Obtain_Notification_Pull_Supplier;

   ---------------------------------------
   -- Obtain_Notification_Push_Supplier --
   ---------------------------------------

   procedure Obtain_Notification_Push_Supplier
     (Self     : access Object;
      Ctype    : CosNotifyChannelAdmin.ClientType;
      Proxy_Id :    out CosNotifyChannelAdmin.ProxyID;
      Returns  :    out CosNotifyChannelAdmin.ProxySupplier.Ref)
   is
      Channel         : CosNotifyChannelAdmin.EventChannel.Impl.Object_Ptr;
      MyRef           : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      Ptype           : CosNotifyChannelAdmin.ProxyType;
      Res             : CORBA.Boolean;
      Supplier        : CosNotifyChannelAdmin.ProxyPushSupplier.
                        Impl.Object_Ptr;
      Seq_Supplier    : CosNotifyChannelAdmin.SequenceProxyPushSupplier.
                        Impl.Object_Ptr;
      Struct_Supplier : CosNotifyChannelAdmin.StructuredProxyPushSupplier.
                        Impl.Object_Ptr;
      SRef            : CosNotifyChannelAdmin.ProxyPushSupplier.Ref;
      Seq_SRef        : CosNotifyChannelAdmin.SequenceProxyPushSupplier.Ref;
      Struct_SRef     : CosNotifyChannelAdmin.StructuredProxyPushSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_notification_push_supplier in consumeradmin"));

      Enter (Self_Mutex);

      Reference_To_Servant (Self.X.Channel, Servant (Channel));
      Res := CosNotifyChannelAdmin.EventChannel.Impl.
             TestSupplierLimit (Channel);

      if Res = False then
         Leave (Self_Mutex);
         raise AdminLimitExceeded;
      end if;

      case Ctype is
         when ANY_EVENT =>
            Ptype := PUSH_ANY;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);
            Supplier := CosNotifyChannelAdmin.ProxyPushSupplier.Impl.Create
                        (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);
            Servant_To_Reference (Servant (Supplier), Returns);
            SRef := CosNotifyChannelAdmin.ProxyPushSupplier.Helper.To_Ref
                    (Returns);
            PushSuppliers.Append (Self.X.Pushs, SRef);
            Append (Self.X.PushIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

         when STRUCTURED_EVENT =>
            Ptype := PUSH_STRUCTURED;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);

            Struct_Supplier :=
            CosNotifyChannelAdmin.StructuredProxyPushSupplier.Impl.Create
            (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);

            Servant_To_Reference (Servant (Struct_Supplier), Returns);
            Struct_SRef := CosNotifyChannelAdmin.StructuredProxyPushSupplier.
                           Helper.To_Ref (Returns);
            StructuredPushSuppliers.Append (Self.X.StructPushs, Struct_SRef);
            Append (Self.X.PushIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

         when SEQUENCE_EVENT =>
            Ptype := PUSH_SEQUENCE;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);

            Seq_Supplier :=
            CosNotifyChannelAdmin.SequenceProxyPushSupplier.Impl.Create
            (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);

            Servant_To_Reference (Servant (Seq_Supplier), Returns);
            Seq_SRef := CosNotifyChannelAdmin.SequenceProxyPushSupplier.
                           Helper.To_Ref (Returns);
            SequencePushSuppliers.Append (Self.X.SequencePushs, Seq_SRef);
            Append (Self.X.PushIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

      end case;

      Leave (Self_Mutex);

   end Obtain_Notification_Push_Supplier;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      Ensure_Initialization;
      pragma Debug (O ("destroy in consumeradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Destroy;

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
      pragma Debug (O ("get_qos in consumeradmin"));

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
      QoS  : CosNotification.QoSProperties)
   is
      My_Ptr     : ConsumerAdmin.Impl.Object_Ptr;
      MyProp     : CosNotification.Property;
      MyError    : CosNotification.PropertyError;
      MyErrCode  : CosNotification.QoSError_code;
      MyRange    : CosNotification.PropertyRange;
      MyErrorSeq : CosNotification.PropertyErrorSeq;
      SeqLen     : Integer;
      Suppliers  : CORBA.Long;
   begin
      Ensure_Initialization;
      pragma Debug (O ("set_qos in consumeradmin"));

      Enter (Self_Mutex);
      My_Ptr := Self.X.This;
      Leave (Self_Mutex);

      Suppliers := GetTotalSuppliers (My_Ptr);

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
            if Suppliers > 0 then
               MyErrCode := UNAVAILABLE_PROPERTY;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            elsif CORBA.Short'(From_Any (MyProp.value)) /= 0
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
     (Self          : access Object;
      Required_QoS  : CosNotification.QoSProperties;
      Available_QoS :    out CosNotification.NamedPropertyRangeSeq)
   is
      My_Ptr       : ConsumerAdmin.Impl.Object_Ptr;
      MyProp       : CosNotification.Property;
      MyError      : CosNotification.PropertyError;
      MyErrCode    : CosNotification.QoSError_code;
      MyNamedRange : CosNotification.NamedPropertyRange;
      MyRange      : CosNotification.PropertyRange;
      MyErrorSeq   : CosNotification.PropertyErrorSeq;
      SeqLen       : Integer;
      Suppliers    : CORBA.Long;
   begin
      Ensure_Initialization;
      pragma Debug (O ("validate_qos in consumeradmin"));

      Enter (Self_Mutex);
      My_Ptr := Self.X.This;
      Leave (Self_Mutex);

      Suppliers := GetTotalSuppliers (My_Ptr);
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
            if Suppliers > 0 then
               MyErrCode := UNAVAILABLE_PROPERTY;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            elsif CORBA.Short'(From_Any (MyProp.value)) /= 0
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
      pragma Debug (O ("subscription_change in consumeradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Subscription_Change;

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
      Ensure_Initialization;
      pragma Debug (O ("add_filter in consumeradmin"));

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
      Ensure_Initialization;
      pragma Debug (O ("remove_filter in consumeradmin"));

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
      Ensure_Initialization;
      pragma Debug (O ("get_filter in consumeradmin"));

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
      pragma Debug (O ("get_all_filters in consumeradmin"));

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
      pragma Debug (O ("remove_all_filters in consumeradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Remove_All_Filters;

   --------------------------
   -- Obtain_Push_Supplier --
   --------------------------

   function Obtain_Push_Supplier
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPushSupplier.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MySupplier : CosEventChannelAdmin.ProxyPushSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_push_supplier in consumeradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySupplier;
   end Obtain_Push_Supplier;

   --------------------------
   -- Obtain_Pull_Supplier --
   --------------------------

   function Obtain_Pull_Supplier
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullSupplier.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MySupplier : CosEventChannelAdmin.ProxyPullSupplier.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_pull_supplier in consumeradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySupplier;
   end Obtain_Pull_Supplier;

   ------------
   -- Create --
   ------------

   function Create
     (Channel     : CosNotifyChannelAdmin.EventChannel.Ref;
      Initial_QoS : CosNotification.QoSProperties;
      MyID        : CosNotifyChannelAdmin.AdminID;
      MyOp        : CosNotifyChannelAdmin.InterFilterGroupOperator
        := AND_OP)
      return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
   begin
      pragma Debug (O ("create consumeradmin"));

      Consumer              := new Object;
      Consumer.X            := new Consumer_Admin_Record;
      Consumer.X.This       := Consumer;
      Consumer.X.Channel    := Channel;
      Consumer.X.Id         := MyID;
      Consumer.X.Op         := MyOp;
      Consumer.X.QoSPropSeq := Initial_QoS;
      Initiate_Servant (Servant (Consumer), My_Ref);

      return Consumer;
   end Create;

   -----------------------
   -- GetTotalSuppliers --
   -----------------------

   function GetTotalSuppliers
     (Self : access Object)
     return CORBA.Long
   is
      MyCount : CORBA.Long;
   begin
      Ensure_Initialization;
      pragma Debug (O ("gettotalsuppliers from consumeradmin"));

      MyCount := CORBA.Long (AllProxies.Length (Self.X.AllPxs));

      return MyCount;
   end GetTotalSuppliers;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self          : access Object;
      Data          : CORBA.Any;
      Internal_Post : CORBA.Boolean := False)
   is
      PullSupplier : CosNotifyChannelAdmin.ProxyPullSupplier.Impl.Object_Ptr;
      PushSupplier : CosNotifyChannelAdmin.ProxyPushSupplier.Impl.Object_Ptr;
   begin
      Ensure_Initialization;

      Enter (Self_Mutex);
      declare
         Pulls : constant PullSuppliers.Element_Array
           := PullSuppliers.To_Element_Array (Self.X.Pulls);
         Pushs : constant PushSuppliers.Element_Array
           := PushSuppliers.To_Element_Array (Self.X.Pushs);
      begin
         Leave (Self_Mutex);

         pragma Debug
           (O ("post new data from consumeradmin to proxy pull suppliers"));

         for J in Pulls'Range loop
            Reference_To_Servant (Pulls (J), Servant (PullSupplier));
            CosNotifyChannelAdmin.ProxyPullSupplier.Impl.Post
              (PullSupplier, Data);
         end loop;

         pragma Debug
           (O ("post new data from consumeradmin to proxy push suppliers"));

         for J in Pushs'Range loop
            Reference_To_Servant (Pushs (J), Servant (PushSupplier));
            CosNotifyChannelAdmin.ProxyPushSupplier.Impl.Post
              (PushSupplier, Data);
         end loop;
      end;

      if not Internal_Post then

         declare
            MyEventHeader         : CosNotification.EventHeader;
            MyFixedEventHeader    : CosNotification.FixedEventHeader;
            MyOpt_HeaderFields    : CosNotification.OptionalHeaderFields;
            MyEventType           : CosNotification.EventType;

            MyFilterableEventBody : CosNotification.FilterableEventBody;
            MyRemainder_Of_Body   : constant CORBA.Any := Data;

            MyStructuredEvent     : CosNotification.StructuredEvent;
            MyStruct_Sequence     : CosNotification.EventBatch;
            Intern_Post           : constant CORBA.Boolean := True;
         begin

            MyEventType.domain_name := To_CORBA_String ("");
            MyEventType.type_name   := To_CORBA_String ("%ANY");

            MyFixedEventHeader.event_type  := MyEventType;
            MyFixedEventHeader.event_name  := To_CORBA_String ("");

            MyEventHeader := (MyFixedEventHeader, MyOpt_HeaderFields);

            MyStructuredEvent.header := MyEventHeader;
            MyStructuredEvent.filterable_data := MyFilterableEventBody;
            MyStructuredEvent.remainder_of_body := MyRemainder_Of_Body;

            --  After converting every Any into StructuredEvent call
            --  Structured_Post to deliver it to all structured proxy supplier
            Structured_Post (Self.X.This, MyStructuredEvent, Intern_Post);

            --  Create a Sequence of Structured Events and append the newly
            --  created structured event into this sequence and send it to
            --  all sequence proxy suppliers by calling Sequence_Post
            Append (MyStruct_Sequence, MyStructuredEvent);
            Sequence_Post (Self.X.This, MyStruct_Sequence, Intern_Post);
         end;

      end if;

   end Post;

   ---------------------
   -- Structured_Post --
   ---------------------

   procedure Structured_Post
     (Self          : access Object;
      Notification  : CosNotification.StructuredEvent;
      Internal_Post : CORBA.Boolean := False)
   is
      PushSupplier : CosNotifyChannelAdmin.StructuredProxyPushSupplier.
                        Impl.Object_Ptr;
      PullSupplier : CosNotifyChannelAdmin.StructuredProxyPullSupplier.
                        Impl.Object_Ptr;
   begin
      Ensure_Initialization;

      Enter (Self_Mutex);
      declare
         StructPushs : constant StructuredPushSuppliers.Element_Array
           := StructuredPushSuppliers.To_Element_Array (Self.X.StructPushs);
         StructPulls : constant StructuredPullSuppliers.Element_Array
           := StructuredPullSuppliers.To_Element_Array (Self.X.StructPulls);
      begin
         Leave (Self_Mutex);

         pragma Debug
           (O ("post new structedevent from consumeradmin " &
               "to structedproxypull suppliers"));

         for J in StructPulls'Range loop
            Reference_To_Servant (StructPulls (J), Servant (PullSupplier));
            CosNotifyChannelAdmin.StructuredProxyPullSupplier.Impl.
               Structured_Post (PullSupplier, Notification);
         end loop;

         pragma Debug
           (O ("post new structedevent from consumeradmin " &
               "to structedproxypush suppliers"));

         for J in StructPushs'Range loop
            Reference_To_Servant (StructPushs (J), Servant (PushSupplier));
            CosNotifyChannelAdmin.StructuredProxyPushSupplier.Impl.
               Structured_Post (PushSupplier, Notification);
         end loop;
      end;

      if not Internal_Post then
         declare
            Data : constant CORBA.Any := CosNotification.Helper.
                                         To_Any (Notification);
            MyStruct_Sequence : CosNotification.EventBatch;
            Intern_Post : constant CORBA.Boolean := True;
         begin
            --  After converting notification into Any call
            --  post for delivery to all untyped proxy suppliers
            Post (Self.X.This, Data, Intern_Post);

            --  Create a Sequence of Structured Events and append
            --  notification into this sequence. Send it to
            --  all sequence proxy suppliers by calling Sequence_Post
            Append (MyStruct_Sequence, Notification);
            Sequence_Post (Self.X.This, MyStruct_Sequence, Intern_Post);
         end;
      end if;

   end Structured_Post;

   -------------------
   -- Sequence_Post --
   -------------------

   procedure Sequence_Post
     (Self          : access Object;
      Notifications : CosNotification.EventBatch;
      Internal_Post : CORBA.Boolean := False)
   is
      PushSupplier : CosNotifyChannelAdmin.SequenceProxyPushSupplier.
                        Impl.Object_Ptr;
      PullSupplier : CosNotifyChannelAdmin.SequenceProxyPullSupplier.
                        Impl.Object_Ptr;
   begin
      Ensure_Initialization;

      Enter (Self_Mutex);
      declare
         SequencePushs : constant SequencePushSuppliers.Element_Array
           := SequencePushSuppliers.To_Element_Array (Self.X.SequencePushs);
         SequencePulls : constant SequencePullSuppliers.Element_Array
           := SequencePullSuppliers.To_Element_Array (Self.X.SequencePulls);
      begin
         Leave (Self_Mutex);

         pragma Debug
           (O ("post new sequence of structedevent from consumeradmin " &
               "to sequenceproxypull suppliers"));

         for J in SequencePulls'Range loop
            Reference_To_Servant (SequencePulls (J), Servant (PullSupplier));
            CosNotifyChannelAdmin.SequenceProxyPullSupplier.Impl.
               Sequence_Post (PullSupplier, Notifications);
         end loop;

         pragma Debug
           (O ("post new sequence of structedevent from consumeradmin " &
               "to sequenceproxypush suppliers"));

         for J in SequencePushs'Range loop
            Reference_To_Servant (SequencePushs (J), Servant (PushSupplier));
            CosNotifyChannelAdmin.SequenceProxyPushSupplier.Impl.
               Sequence_Post (PushSupplier, Notifications);
         end loop;
      end;

      if not Internal_Post then
         declare
            Data         : CORBA.Any;
            Intern_Post  : constant CORBA.Boolean := True;
            Notification : CosNotification.StructuredEvent;
            SeqLen       : constant Integer := Length (Notifications);
         begin
            for Index in 1 .. SeqLen loop
               Notification := Get_Element (Notifications, Index);

               --  Send every structedevent to all structured proxy suppliers
               --  by calling Structured_Post every time
               Structured_Post (Self.X.This, Notification, Intern_Post);

               --  Convert every structedevent into any and call post
               --  for delivery to all untyped proxy suppliers
               Data := CosNotification.Helper.To_Any (Notification);
               Post (Self.X.This, Data, Intern_Post);
            end loop;
         end;
      end if;

   end Sequence_Post;

end CosNotifyChannelAdmin.ConsumerAdmin.Impl;
