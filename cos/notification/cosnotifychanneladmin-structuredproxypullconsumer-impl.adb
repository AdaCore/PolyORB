------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         COSNOTIFYCHANNELADMIN.STRUCTUREDPROXYPULLCONSUMER.IMPL           --
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

with CORBA.Impl;

with CosEventChannelAdmin.Helper;

with CosNotification;
with CosNotification.Helper;
with CosNotifyChannelAdmin.SupplierAdmin.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

with CosNotifyChannelAdmin.StructuredProxyPullConsumer.Skel;
pragma Warnings (Off, CosNotifyChannelAdmin.StructuredProxyPullConsumer.Skel);

package body CosNotifyChannelAdmin.StructuredProxyPullConsumer.Impl is

   use CosNotification;
   use IDL_SEQUENCE_CosNotification_Property;
   use IDL_SEQUENCE_CosNotification_PropertyError;
   use IDL_SEQUENCE_CosNotification_NamedPropertyRange;

   use CORBA;
   use PortableServer;

   package Convert is new
      SupplierAdmin_Forward.Convert (CosNotifyChannelAdmin.SupplierAdmin.Ref);

   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads;

   use PolyORB.CORBA_P.Server_Tools;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("structuredproxypullconsumer");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Structured_Proxy_Pull_Consumer_Record is record
      This           : Object_Ptr;
      Admin          : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      MyId           : CosNotifyChannelAdmin.ProxyID;
      MyType         : CosNotifyChannelAdmin.ProxyType;
      Peer           : CosNotifyComm.StructuredPullSupplier.Ref;
      QoSPropSeq     : CosNotification.QoSProperties;
      Engin_Launched : Boolean := False;
      --  is there a thread launch for the engine
   end record;

   A_S   : Object_Ptr := null;
   --  This variable is used to initialize the threads local variable.
   --  it is used to replace the 'accept' statement.

   Session_Mutex : Mutex_Access;
   Session_Taken : Condition_Access;
   --  Synchornisation of task initialization.

   Peer_Mutex : Mutex_Access;
   --  Protect access on a peer component

   T_Initialized : Boolean := False;

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization is
   begin
      if T_Initialized then
         return;
      end if;
      Create (Session_Mutex);
      Create (Session_Taken);
      Create (Peer_Mutex);

      T_Initialized := True;
   end Ensure_Initialization;

   --------------------------------
   -- Proxy_Pull_Consumer_Engine --
   --------------------------------

   procedure Proxy_Pull_Consumer_Engine;

   procedure Proxy_Pull_Consumer_Engine
   is
      This  : Object_Ptr;
      Peer  : CosNotifyComm.StructuredPullSupplier.Ref;
      Event : CosNotification.StructuredEvent;
      Obj   : CORBA.Impl.Object_Ptr;
   begin
      pragma Debug (O ("Session Thread number "
                       & Image (Current_Task)
                       & " is starting"));
      --  Signal end of thread initialization.

      Ensure_Initialization;

      --  Thread initialization.
      --  A_S is a global variable used to pass an argument to this task
      This := A_S;
      --  This is initialized
      --  we can let Connect_Structured_Pull_Supplier go
      Enter  (Session_Mutex);
      Signal (Session_Taken);
      Leave  (Session_Mutex);

      loop
         --  Session thread main loop.
         Enter (Peer_Mutex);
         Peer := This.X.Peer;
         Leave (Peer_Mutex);

         exit when CosNotifyComm.StructuredPullSupplier.Is_Nil (Peer);

         pragma Debug
         (O ("pull structured event from structuredproxypullconsumer engin"));

         begin
            Event := CosNotifyComm.StructuredPullSupplier.pull_structured_event
                       (Peer);
         exception when others =>
            exit;
         end;

         pragma Debug
         (O ("post structured event from structuredproxypullconsumer " &
             "to admin"));

         Reference_To_Servant (This.X.Admin, Servant (Obj));
         CosNotifyChannelAdmin.SupplierAdmin.Impl.Structured_Post
         (CosNotifyChannelAdmin.SupplierAdmin.Impl.Object_Ptr (Obj), Event);
      end loop;

      This.X.Engin_Launched := False;
   end Proxy_Pull_Consumer_Engine;

   --------------------------------------
   -- Connect_Structured_Pull_Supplier --
   --------------------------------------

   procedure Connect_Structured_Pull_Supplier
     (Self          : access Object;
      Pull_Supplier : CosNotifyComm.StructuredPullSupplier.Ref)
   is
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("connect_structured_pull_supplier in structuredproxypullconsumer"));

      Enter (Session_Mutex);
      if not CosNotifyComm.StructuredPullSupplier.Is_Nil (Self.X.Peer) then
         Leave (Session_Mutex);
         CosEventChannelAdmin.Helper.Raise_AlreadyConnected
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      Self.X.Peer := Pull_Supplier;
      A_S := Self.X.This;

      --  Start engin
      if not Self.X.Engin_Launched then
         Create_Task (Proxy_Pull_Consumer_Engine'Access);
         Self.X.Engin_Launched := True;
         --  thread created
      end if;

      --  wait  A_S initialization in Proxy_Pull_Consumer_Engin
      Wait (Session_Taken, Session_Mutex);
      Leave (Session_Mutex);

   end Connect_Structured_Pull_Supplier;

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
      pragma Debug (O ("suspend_connection in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);
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
      pragma Debug (O ("resume_connection in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);
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
      pragma Debug (O ("get_mytype in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      MyType := Self.X.MyType;
      Leave (Peer_Mutex);

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
      Ensure_Initialization;
      pragma Debug (O ("get_myadmin in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      MyAdmin := Convert.To_Forward (Self.X.Admin);
      Leave (Peer_Mutex);

      return MyAdmin;
   end Get_MyAdmin;

   -------------------------------
   -- Obtain_Subscription_Types --
   -------------------------------

   function Obtain_Subscription_Types
     (Self : access Object;
      Mode : CosNotifyChannelAdmin.ObtainInfoMode)
     return CosNotification.EventTypeSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Mode);
      pragma Warnings (On);  --  WAG:3.14
      MySeq : CosNotification.EventTypeSeq;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("obtain_subscription_types in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);

      return MySeq;
   end Obtain_Subscription_Types;

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
      Ensure_Initialization;
      pragma Debug (O ("validate_event_qos in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);

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
      pragma Debug (O ("get_qos in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      MyQoS := Self.X.QoSPropSeq;
      Leave (Peer_Mutex);

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
      Ensure_Initialization;
      pragma Debug (O ("set_qos in structuredproxypullconsumer"));

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
      Enter (Peer_Mutex);
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
      Leave (Peer_Mutex);

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
      Ensure_Initialization;
      pragma Debug (O ("validate_qos in structuredproxypullconsumer"));

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

      Enter (Peer_Mutex);
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
      Leave (Peer_Mutex);

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
      Ensure_Initialization;
      pragma Debug (O ("add_filter in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);

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
      pragma Debug (O ("remove_filter in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);
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
      pragma Debug (O ("get_filter in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);

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
      pragma Debug (O ("get_all_filters in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);

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
      pragma Debug (O ("remove_all_filters in structuredproxypullconsumer"));

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);
   end Remove_All_Filters;

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
      pragma Debug (O ("offer_change in structuredproxypullconsumer"));

      Ensure_Initialization;

      Enter (Peer_Mutex);
      Leave (Peer_Mutex);

   end Offer_Change;

   -----------------------------------------
   -- Disconnect_Structured_Pull_Consumer --
   -----------------------------------------

   procedure Disconnect_Structured_Pull_Consumer
     (Self : access Object)
   is
      Peer    : CosNotifyComm.StructuredPullSupplier.Ref;
      Nil_Ref : CosNotifyComm.StructuredPullSupplier.Ref;
   begin
      pragma Debug (O ("disconnect structuredproxypullconsumer"));

      Ensure_Initialization;

      Enter (Peer_Mutex);
      Peer        := Self.X.Peer;
      Self.X.Peer := Nil_Ref;
      Leave (Peer_Mutex);

      if not CosNotifyComm.StructuredPullSupplier.Is_Nil (Peer) then
         CosNotifyComm.StructuredPullSupplier.
         disconnect_structured_pull_supplier (Peer);
      end if;
   end Disconnect_Structured_Pull_Consumer;

   ------------
   -- Create --
   ------------

   function Create
     (Admin       : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      Initial_QoS : CosNotification.QoSProperties;
      Ptype       : CosNotifyChannelAdmin.ProxyType;
      Proxy_Id    : CosNotifyChannelAdmin.ProxyID)
     return Object_Ptr
   is
      Consumer : Object_Ptr;
      My_Ref   : StructuredProxyPullConsumer.Ref;
   begin
      pragma Debug (O ("create structuredproxypullconsumer"));

      Consumer              := new Object;
      Consumer.X            := new Structured_Proxy_Pull_Consumer_Record;
      Consumer.X.Admin      := Admin;
      Consumer.X.MyId       := Proxy_Id;
      Consumer.X.MyType     := Ptype;
      Consumer.X.This       := Consumer;
      Consumer.X.QoSPropSeq := Initial_QoS;

      Initiate_Servant (Servant (Consumer), My_Ref);
      return Consumer;
   end Create;

end CosNotifyChannelAdmin.StructuredProxyPullConsumer.Impl;
