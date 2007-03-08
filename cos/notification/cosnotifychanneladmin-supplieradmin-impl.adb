------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFYCHANNELADMIN.SUPPLIERADMIN.IMPL                  --
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
with CosNotifyChannelAdmin.Helper;

with CosNotifyChannelAdmin.ProxyPushConsumer.Impl;
with CosNotifyChannelAdmin.ProxyPushConsumer.Helper;

with CosNotifyChannelAdmin.ProxyPullConsumer.Impl;
with CosNotifyChannelAdmin.ProxyPullConsumer.Helper;

with CosNotifyChannelAdmin.SequenceProxyPullConsumer.Impl;
with CosNotifyChannelAdmin.SequenceProxyPullConsumer.Helper;

with CosNotifyChannelAdmin.SequenceProxyPushConsumer.Impl;
with CosNotifyChannelAdmin.SequenceProxyPushConsumer.Helper;

with CosNotifyChannelAdmin.StructuredProxyPushConsumer.Impl;
with CosNotifyChannelAdmin.StructuredProxyPushConsumer.Helper;

with CosNotifyChannelAdmin.StructuredProxyPullConsumer.Impl;
with CosNotifyChannelAdmin.StructuredProxyPullConsumer.Helper;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
with PolyORB.Log;

with CosNotifyChannelAdmin.SupplierAdmin.Skel;
pragma Warnings (Off, CosNotifyChannelAdmin.SupplierAdmin.Skel);

package body CosNotifyChannelAdmin.SupplierAdmin.Impl is

   use IDL_SEQUENCE_CosNotifyChannelAdmin_ProxyID;

   use CosNotification;
   use IDL_SEQUENCE_CosNotification_Property;
   use IDL_SEQUENCE_CosNotification_PropertyError;
   use IDL_SEQUENCE_CosNotification_NamedPropertyRange;

   use CORBA;
   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   package Convert is new
      EventChannel_Forward.Convert (CosNotifyChannelAdmin.EventChannel.Ref);

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("supplieradmin");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package AllProxies is new
      CORBA.Sequences.Unbounded (CORBA.Long);

   package PullConsumers is new
      CORBA.Sequences.Unbounded (CosNotifyChannelAdmin.ProxyPullConsumer.Ref);

   package PushConsumers is new
      CORBA.Sequences.Unbounded (CosNotifyChannelAdmin.ProxyPushConsumer.Ref);

   package SequencePullConsumers is new CORBA.Sequences.Unbounded
      (CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref);

   package SequencePushConsumers is new CORBA.Sequences.Unbounded
      (CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref);

   package StructuredPullConsumers is new CORBA.Sequences.Unbounded
      (CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref);

   package StructuredPushConsumers is new CORBA.Sequences.Unbounded
      (CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref);

   type Supplier_Admin_Record is record
      This          : Object_Ptr;
      Channel       : CosNotifyChannelAdmin.EventChannel.Ref;
      Id            : CosNotifyChannelAdmin.AdminID;
      Op            : CosNotifyChannelAdmin.InterFilterGroupOperator;
      AllPxs        : AllProxies.Sequence;
      Pulls         : PullConsumers.Sequence;
      Pushs         : PushConsumers.Sequence;
      SequencePulls : SequencePullConsumers.Sequence;
      SequencePushs : SequencePushConsumers.Sequence;
      StructPulls   : StructuredPullConsumers.Sequence;
      StructPushs   : StructuredPushConsumers.Sequence;
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
      pragma Debug (O ("get_myid in supplieradmin"));

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
      pragma Debug (O ("get_mychannel in supplieradmin"));

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
      pragma Debug (O ("get_myoperator in supplieradmin"));

      Enter (Self_Mutex);
      MyOperator := Self.X.Op;
      Leave (Self_Mutex);

      return MyOperator;
   end Get_MyOperator;

   -------------------------
   -- Get_Pull_Consumers --
   -------------------------

   function Get_Pull_Consumers
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyIDSeq
   is
      MySeq : CosNotifyChannelAdmin.ProxyIDSeq;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_pull_consumers in supplieradmin"));

      Enter (Self_Mutex);
      MySeq := Self.X.PullIDSeq;
      Leave (Self_Mutex);

      return MySeq;
   end Get_Pull_Consumers;

   -------------------------
   -- Get_Push_Consumers --
   -------------------------

   function Get_Push_Consumers
     (Self : access Object)
     return CosNotifyChannelAdmin.ProxyIDSeq
   is
      MySeq : CosNotifyChannelAdmin.ProxyIDSeq;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_push_consumers in supplieradmin"));

      Enter (Self_Mutex);
      MySeq := Self.X.PushIDSeq;
      Leave (Self_Mutex);

      return MySeq;
   end Get_Push_Consumers;

   ------------------------
   -- Get_Proxy_Consumer --
   ------------------------

   function Get_Proxy_Consumer
     (Self       : access Object;
      Proxy_Id   : CosNotifyChannelAdmin.ProxyID)
     return CosNotifyChannelAdmin.ProxyConsumer.Ref
   is
      MyConsumer : CosNotifyChannelAdmin.ProxyConsumer.Ref;
      SeqLen     : CosNotifyChannelAdmin.ProxyID;
   begin
      Ensure_Initialization;
      pragma Debug (O ("get_proxy_consumer in supplieradmin"));

      Enter (Self_Mutex);
      SeqLen := CosNotifyChannelAdmin.ProxyID
                  (AllProxies.Length (Self.X.AllPxs));
      if Proxy_Id >= SeqLen then
         Leave (Self_Mutex);
         CosNotifyChannelAdmin.Helper.Raise_ProxyNotFound
           ((CORBA.IDL_Exception_Members with null record));
      end if;
      --  NK How to search an element in Sequence
      --  Here we have to search in two sequences : Pushs and Pulls
      --  to find and return a suitable Proxy Ref
      Leave (Self_Mutex);

      return MyConsumer;
   end Get_Proxy_Consumer;

   ---------------------------------------
   -- Obtain_Notification_Pull_Consumer --
   ---------------------------------------

   procedure Obtain_Notification_Pull_Consumer
     (Self     : access Object;
      Ctype    : CosNotifyChannelAdmin.ClientType;
      Proxy_Id :    out CosNotifyChannelAdmin.ProxyID;
      Returns  :    out CosNotifyChannelAdmin.ProxyConsumer.Ref)
   is
      Channel         : CosNotifyChannelAdmin.EventChannel.Impl.Object_Ptr;
      Consumer        : CosNotifyChannelAdmin.ProxyPullConsumer.
                        Impl.Object_Ptr;
      Seq_Consumer    : CosNotifyChannelAdmin.SequenceProxyPullConsumer.
                        Impl.Object_Ptr;
      Struct_Consumer : CosNotifyChannelAdmin.StructuredProxyPullConsumer.
                        Impl.Object_Ptr;
      CRef            : CosNotifyChannelAdmin.ProxyPullConsumer.Ref;
      Seq_CRef        : CosNotifyChannelAdmin.SequenceProxyPullConsumer.Ref;
      Struct_CRef     : CosNotifyChannelAdmin.StructuredProxyPullConsumer.Ref;
      MyRef           : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      Ptype           : CosNotifyChannelAdmin.ProxyType;
      Res             : CORBA.Boolean;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_notification_pull_consumer in supplieradmin"));

      Enter (Self_Mutex);
      Reference_To_Servant (Self.X.Channel, Servant (Channel));
      Res := CosNotifyChannelAdmin.EventChannel.Impl.
             TestConsumerLimit (Channel);

      if Res = False then
         Leave (Self_Mutex);
         raise AdminLimitExceeded;
      end if;

      case Ctype is
         when ANY_EVENT =>
            Ptype    := PULL_ANY;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);
            Consumer := CosNotifyChannelAdmin.ProxyPullConsumer.Impl.Create
                        (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);
            Servant_To_Reference (Servant (Consumer), Returns);
            CRef     := CosNotifyChannelAdmin.ProxyPullConsumer.Helper.To_Ref
                        (Returns);
            PullConsumers.Append (Self.X.Pulls, CRef);
            Append (Self.X.PullIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

         when STRUCTURED_EVENT =>
            Ptype    := PULL_STRUCTURED;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);
            Struct_Consumer :=
            CosNotifyChannelAdmin.StructuredProxyPullConsumer.Impl.Create
            (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);

            Servant_To_Reference (Servant (Struct_Consumer), Returns);
            Struct_CRef := CosNotifyChannelAdmin.StructuredProxyPullConsumer.
                           Helper.To_Ref (Returns);
            StructuredPullConsumers.Append (Self.X.StructPulls, Struct_CRef);
            Append (Self.X.PullIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

         when SEQUENCE_EVENT =>
            Ptype    := PULL_SEQUENCE;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);
            Seq_Consumer :=
            CosNotifyChannelAdmin.SequenceProxyPullConsumer.Impl.Create
            (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);

            Servant_To_Reference (Servant (Seq_Consumer), Returns);
            Seq_CRef := CosNotifyChannelAdmin.SequenceProxyPullConsumer.
                           Helper.To_Ref (Returns);
            SequencePullConsumers.Append (Self.X.SequencePulls, Seq_CRef);
            Append (Self.X.PullIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

      end case;

      Leave (Self_Mutex);

   end Obtain_Notification_Pull_Consumer;

   ---------------------------------------
   -- Obtain_Notification_Push_Consumer --
   ---------------------------------------

   procedure Obtain_Notification_Push_Consumer
     (Self     : access Object;
      Ctype    : CosNotifyChannelAdmin.ClientType;
      Proxy_Id :    out CosNotifyChannelAdmin.ProxyID;
      Returns  :    out CosNotifyChannelAdmin.ProxyConsumer.Ref)
   is
      Channel         : CosNotifyChannelAdmin.EventChannel.Impl.Object_Ptr;
      Consumer        : CosNotifyChannelAdmin.ProxyPushConsumer.
                        Impl.Object_Ptr;
      Seq_Consumer    : CosNotifyChannelAdmin.SequenceProxyPushConsumer.
                        Impl.Object_Ptr;
      Struct_Consumer : CosNotifyChannelAdmin.StructuredProxyPushConsumer.
                        Impl.Object_Ptr;
      CRef            : CosNotifyChannelAdmin.ProxyPushConsumer.Ref;
      Seq_CRef        : CosNotifyChannelAdmin.SequenceProxyPushConsumer.Ref;
      Struct_CRef     : CosNotifyChannelAdmin.StructuredProxyPushConsumer.Ref;
      MyRef           : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      Ptype           : CosNotifyChannelAdmin.ProxyType;
      Res             : CORBA.Boolean;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_notification_push_consumer in supplieradmin"));

      Enter (Self_Mutex);

      Reference_To_Servant (Self.X.Channel, Servant (Channel));
      Res := CosNotifyChannelAdmin.EventChannel.Impl.
             TestConsumerLimit (Channel);

      if Res = False then
         Leave (Self_Mutex);
         raise AdminLimitExceeded;
      end if;

      case Ctype is
         when ANY_EVENT =>
            Ptype    := PUSH_ANY;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);
            Consumer := CosNotifyChannelAdmin.ProxyPushConsumer.Impl.Create
                        (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);
            Servant_To_Reference (Servant (Consumer), Returns);
            CRef     := CosNotifyChannelAdmin.ProxyPushConsumer.Helper.To_Ref
                        (Returns);
            PushConsumers.Append (Self.X.Pushs, CRef);
            Append (Self.X.PushIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

         when STRUCTURED_EVENT =>
            Ptype    := PUSH_STRUCTURED;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);
            Struct_Consumer :=
            CosNotifyChannelAdmin.StructuredProxyPushConsumer.Impl.Create
            (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);

            Servant_To_Reference (Servant (Struct_Consumer), Returns);
            Struct_CRef := CosNotifyChannelAdmin.StructuredProxyPushConsumer.
                           Helper.To_Ref (Returns);
            StructuredPushConsumers.Append (Self.X.StructPushs, Struct_CRef);
            Append (Self.X.PushIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

         when SEQUENCE_EVENT =>
            Ptype    := PUSH_SEQUENCE;
            Proxy_Id := CosNotifyChannelAdmin.ProxyID
                        (AllProxies.Length (Self.X.AllPxs));
            Servant_To_Reference (Servant (Self.X.This), MyRef);
            Seq_Consumer :=
            CosNotifyChannelAdmin.SequenceProxyPushConsumer.Impl.Create
            (MyRef, Self.X.QoSPropSeq, Ptype, Proxy_Id);

            Servant_To_Reference (Servant (Seq_Consumer), Returns);
            Seq_CRef := CosNotifyChannelAdmin.SequenceProxyPushConsumer.Helper.
                        To_Ref (Returns);
            SequencePushConsumers.Append (Self.X.SequencePushs, Seq_CRef);
            Append (Self.X.PushIDSeq, Proxy_Id);
            AllProxies.Append (Self.X.AllPxs, CORBA.Long (Proxy_Id));

      end case;

      Leave (Self_Mutex);

   end Obtain_Notification_Push_Consumer;

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
      pragma Debug (O ("destroy in supplieradmin"));

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
      pragma Debug (O ("get_qos in supplieradmin"));

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
      Consumers  : CORBA.Long;
      My_Ptr     : SupplierAdmin.Impl.Object_Ptr;
      MyProp     : CosNotification.Property;
      MyError    : CosNotification.PropertyError;
      MyErrCode  : CosNotification.QoSError_code;
      MyRange    : CosNotification.PropertyRange;
      MyErrorSeq : CosNotification.PropertyErrorSeq;
      SeqLen     : Integer;
   begin
      Ensure_Initialization;
      pragma Debug (O ("set_qos in supplieradmin"));

      Enter (Self_Mutex);
      My_Ptr := Self.X.This;
      Leave (Self_Mutex);

      Consumers := GetTotalConsumers (My_Ptr);
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
            if Consumers > 0 then
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
      Consumers    : CORBA.Long;
      My_Ptr       : SupplierAdmin.Impl.Object_Ptr;
      MyProp       : CosNotification.Property;
      MyError      : CosNotification.PropertyError;
      MyErrCode    : CosNotification.QoSError_code;
      MyNamedRange : CosNotification.NamedPropertyRange;
      MyRange      : CosNotification.PropertyRange;
      MyErrorSeq   : CosNotification.PropertyErrorSeq;
      SeqLen       : Integer;
   begin
      Ensure_Initialization;
      pragma Debug (O ("validate_qos in supplieradmin"));

      Enter (Self_Mutex);
      My_Ptr := Self.X.This;
      Leave (Self_Mutex);

      Consumers := GetTotalConsumers (My_Ptr);
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
            if Consumers > 0 then
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

   ------------------
   -- Offer_Change --
   ------------------

   procedure Offer_Change
     (Self    : access Object;
      Added   : CosNotification.EventTypeSeq;
      Removed : CosNotification.EventTypeSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Added, Removed);
      pragma Warnings (On);  --  WAG:3.14
   begin
      Ensure_Initialization;
      pragma Debug (O ("offer_change in supplieradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Offer_Change;

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
      pragma Debug (O ("add_filter in supplieradmin"));

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
      pragma Debug (O ("remove_filter in supplieradmin"));

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
      pragma Debug (O ("get_filter in supplieradmin"));

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
      pragma Debug (O ("get_all_filters in supplieradmin"));

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
      pragma Debug (O ("remove_all_filters in supplieradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Remove_All_Filters;

   --------------------------
   -- Obtain_Push_Consumer --
   --------------------------

   function Obtain_Push_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPushConsumer.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyProxy : CosEventChannelAdmin.ProxyPushConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_push_consumer in supplieradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyProxy;
   end Obtain_Push_Consumer;

   --------------------------
   -- Obtain_Pull_Consumer --
   --------------------------

   function Obtain_Pull_Consumer
     (Self : access Object)
     return CosEventChannelAdmin.ProxyPullConsumer.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyProxy : CosEventChannelAdmin.ProxyPullConsumer.Ref;
   begin
      Ensure_Initialization;
      pragma Debug (O ("obtain_pull_consumer in supplieradmin"));

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyProxy;
   end Obtain_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create
     (Channel     : CosNotifyChannelAdmin.EventChannel.Ref;
      Initial_QoS : CosNotification.QoSProperties;
      MyID        : CosNotifyChannelAdmin.AdminID;
      MyOp        : CosNotifyChannelAdmin.InterFilterGroupOperator := AND_OP)
     return Object_Ptr
   is
      Supplier : Object_Ptr;
      My_Ref   : CosNotifyChannelAdmin.SupplierAdmin.Ref;
   begin
      pragma Debug (O ("create supplieradmin"));

      Supplier              := new Object;
      Supplier.X            := new Supplier_Admin_Record;
      Supplier.X.This       := Supplier;
      Supplier.X.Channel    := Channel;
      Supplier.X.Id         := MyID;
      Supplier.X.Op         := MyOp;
      Supplier.X.QoSPropSeq := Initial_QoS;
      Initiate_Servant (Servant (Supplier), My_Ref);

      return Supplier;
   end Create;

   -----------------------
   -- GetTotalConsumers --
   -----------------------

   function GetTotalConsumers
     (Self : access Object)
     return CORBA.Long
   is
      MyCount : CORBA.Long;
   begin
      Ensure_Initialization;
      pragma Debug (O ("gettotalconsumers from supplieradmin"));

      MyCount := CORBA.Long (AllProxies.Length (Self.X.AllPxs));

      return MyCount;
   end GetTotalConsumers;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : CORBA.Any)
   is
      Channel : CosNotifyChannelAdmin.EventChannel.Impl.Object_Ptr;
   begin
      Ensure_Initialization;
      pragma Debug (O ("post new data from supplieradmin to eventchannel"));

      Enter (Self_Mutex);
      Reference_To_Servant (Self.X.Channel, Servant (Channel));
      Leave (Self_Mutex);

      CosNotifyChannelAdmin.EventChannel.Impl.Post (Channel, Data);

   end Post;

   ---------------------
   -- Structured_Post --
   ---------------------

   procedure Structured_Post
     (Self         : access Object;
      Notification : CosNotification.StructuredEvent)
   is
      Channel : CosNotifyChannelAdmin.EventChannel.Impl.Object_Ptr;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("post new structured data from supplieradmin to eventchannel"));

      Enter (Self_Mutex);
      Reference_To_Servant (Self.X.Channel, Servant (Channel));
      Leave (Self_Mutex);

      CosNotifyChannelAdmin.EventChannel.Impl.Structured_Post
        (Channel, Notification);

   end Structured_Post;

   -------------------
   -- Sequence_Post --
   -------------------

   procedure Sequence_Post
     (Self          : access Object;
      Notifications : CosNotification.EventBatch)
   is
      Channel : CosNotifyChannelAdmin.EventChannel.Impl.Object_Ptr;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("post new sequence of structured data from " &
          "supplieradmin to eventchannel"));

      Enter (Self_Mutex);
      Reference_To_Servant (Self.X.Channel, Servant (Channel));
      Leave (Self_Mutex);

      CosNotifyChannelAdmin.EventChannel.Impl.Sequence_Post
        (Channel, Notifications);

   end Sequence_Post;

end CosNotifyChannelAdmin.SupplierAdmin.Impl;
