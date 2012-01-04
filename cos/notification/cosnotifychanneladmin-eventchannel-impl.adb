------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 COSNOTIFYCHANNELADMIN.EVENTCHANNEL.IMPL                  --
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

with CosNotification;
with CosNotification.Helper;

with CosNotifyChannelAdmin.ConsumerAdmin.Impl;
with CosNotifyChannelAdmin.Helper;
with CosNotifyChannelAdmin.SupplierAdmin.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosNotifyChannelAdmin.EventChannel.Skel;
pragma Warnings (Off, CosNotifyChannelAdmin.EventChannel.Skel);

package body CosNotifyChannelAdmin.EventChannel.Impl is

   use IDL_SEQUENCE_CosNotifyChannelAdmin_AdminID;

   use CosNotification;
   use IDL_SEQUENCE_CosNotification_Property;
   use IDL_SEQUENCE_CosNotification_PropertyError;
   use IDL_SEQUENCE_CosNotification_NamedPropertyRange;

   use CORBA;
   use CORBA.TypeCode;
   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("eventchannel");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package Convert is new EventChannelFactory_Forward.Convert
      (CosNotifyChannelAdmin.EventChannelFactory.Ref);

   package ConsumerAdmins is
      new CORBA.Sequences.Unbounded (CosNotifyChannelAdmin.ConsumerAdmin.Ref);

   package SupplierAdmins is
      new CORBA.Sequences.Unbounded (CosNotifyChannelAdmin.SupplierAdmin.Ref);

   type Event_Channel_Record is record
      This       : Object_Ptr;
      AdmPropSeq : CosNotification.AdminProperties;
      Consumer   : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      Supplier   : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      Consumers  : ConsumerAdmins.Sequence;
      Suppliers  : SupplierAdmins.Sequence;
      CIDSeq     : CosNotifyChannelAdmin.AdminIDSeq;
      SIDSeq     : CosNotifyChannelAdmin.AdminIDSeq;
      Factory    : CosNotifyChannelAdmin.EventChannelFactory.Ref;
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

   -------------------
   -- Get_MyFactory --
   -------------------

   function Get_MyFactory
     (Self : access Object)
     return CosNotifyChannelAdmin.EventChannelFactory_Forward.Ref
   is
      MyFactory : CosNotifyChannelAdmin.EventChannelFactory_Forward.Ref;
   begin
      pragma Debug (O ("get_myfactory in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyFactory := Convert.To_Forward (Self.X.Factory);
      Leave (Self_Mutex);

      return MyFactory;
   end Get_MyFactory;

   --------------------------------
   -- Get_Default_Consumer_Admin --
   --------------------------------

   function Get_Default_Consumer_Admin
     (Self : access Object)
     return CosNotifyChannelAdmin.ConsumerAdmin.Ref
   is
      MyAdmin : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
   begin
      pragma Debug (O ("get_default_consumer_admin in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyAdmin := Self.X.Consumer;
      Leave (Self_Mutex);

      return MyAdmin;
   end Get_Default_Consumer_Admin;

   --------------------------------
   -- Get_Default_Supplier_Admin --
   --------------------------------

   function Get_Default_Supplier_Admin
     (Self : access Object)
     return CosNotifyChannelAdmin.SupplierAdmin.Ref
   is
      MyAdmin : CosNotifyChannelAdmin.SupplierAdmin.Ref;
   begin
      pragma Debug (O ("get_default_supplier_admin in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyAdmin := Self.X.Supplier;
      Leave (Self_Mutex);

      return MyAdmin;
   end Get_Default_Supplier_Admin;

   --------------------------------
   -- Get_Default_Filter_Factory --
   --------------------------------

   function Get_Default_Filter_Factory
     (Self : access Object)
     return CosNotifyFilter.FilterFactory.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyFilter : CosNotifyFilter.FilterFactory.Ref;
   begin
      pragma Debug (O ("get_default_filter_factory in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Get_Default_Filter_Factory;

   -----------------------
   -- New_For_Consumers --
   -----------------------

   procedure New_For_Consumers
     (Self    : access Object;
      Op      : CosNotifyChannelAdmin.InterFilterGroupOperator;
      Id      :    out CosNotifyChannelAdmin.AdminID;
      Returns :    out CosNotifyChannelAdmin.ConsumerAdmin.Ref)
   is
      Consumer : CosNotifyChannelAdmin.ConsumerAdmin.Impl.Object_Ptr;
      MyRef    : CosNotifyChannelAdmin.EventChannel.Ref;
   begin
      pragma Debug (O ("new_for_consumers in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Id := CosNotifyChannelAdmin.AdminID
            (ConsumerAdmins.Length (Self.X.Consumers));
      Append (Self.X.CIDSeq, Id);

      Servant_To_Reference (Servant (Self.X.This), MyRef);
      Consumer := CosNotifyChannelAdmin.ConsumerAdmin.Impl.Create
                  (MyRef, Self.X.QoSPropSeq, Id, Op);
      Servant_To_Reference (Servant (Consumer), Returns);
      ConsumerAdmins.Append (Self.X.Consumers, Returns);
      Leave (Self_Mutex);

   end New_For_Consumers;

   -----------------------
   -- New_For_Suppliers --
   -----------------------

   procedure New_For_Suppliers
     (Self    : access Object;
      Op      : CosNotifyChannelAdmin.InterFilterGroupOperator;
      Id      :    out CosNotifyChannelAdmin.AdminID;
      Returns :    out CosNotifyChannelAdmin.SupplierAdmin.Ref)
   is
      Supplier : CosNotifyChannelAdmin.SupplierAdmin.Impl.Object_Ptr;
      MyRef    : CosNotifyChannelAdmin.EventChannel.Ref;
   begin
      pragma Debug (O ("new_for_suppliers in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Id := CosNotifyChannelAdmin.AdminID
            (SupplierAdmins.Length (Self.X.Suppliers));
      Append (Self.X.SIDSeq, Id);

      Servant_To_Reference (Servant (Self.X.This), MyRef);
      Supplier := CosNotifyChannelAdmin.SupplierAdmin.Impl.Create
                  (MyRef, Self.X.QoSPropSeq, Id, Op);
      Servant_To_Reference (Servant (Supplier), Returns);
      SupplierAdmins.Append (Self.X.Suppliers, Returns);
      Leave (Self_Mutex);

   end New_For_Suppliers;

   -----------------------
   -- Get_ConsumerAdmin --
   -----------------------

   function Get_ConsumerAdmin
     (Self : access Object;
      Id   : CosNotifyChannelAdmin.AdminID)
      return CosNotifyChannelAdmin.ConsumerAdmin.Ref
   is
      MyConsumerAdmin : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      SeqLen          : CosNotifyChannelAdmin.AdminID;
   begin
      pragma Debug (O ("get_consumeradmin in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);

      SeqLen := CosNotifyChannelAdmin.AdminID (Length (Self.X.CIDSeq));
      if Id >= SeqLen then
         Leave (Self_Mutex);
         CosNotifyChannelAdmin.Helper.Raise_AdminNotFound
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      --  AdminID of the ConsumerAdmin will always be 1 less than
      --  the index number of its ConsumerAdmin Sequence as the
      --  first consumeradmin created must have the AdminID of 0
      MyConsumerAdmin := ConsumerAdmins.Get_Element
                         (Self.X.Consumers, Integer (Id + 1));

      Leave (Self_Mutex);

      return MyConsumerAdmin;
   end Get_ConsumerAdmin;

   -----------------------
   -- Get_SupplierAdmin --
   -----------------------

   function Get_SupplierAdmin
     (Self : access Object;
      Id   : CosNotifyChannelAdmin.AdminID)
      return CosNotifyChannelAdmin.SupplierAdmin.Ref
   is
      MySupplierAdmin : CosNotifyChannelAdmin.SupplierAdmin.Ref;
      SeqLen          : CosNotifyChannelAdmin.AdminID;
   begin
      pragma Debug (O ("get_supplieradmin in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);

      SeqLen := CosNotifyChannelAdmin.AdminID (Length (Self.X.SIDSeq));
      if Id >= SeqLen then
         Leave (Self_Mutex);
         CosNotifyChannelAdmin.Helper.Raise_AdminNotFound
           ((CORBA.IDL_Exception_Members with null record));
      end if;

      --  AdminID of the SupplierAdmin will always be 1 less than
      --  the index number of its SupplierAdmin Sequence as the
      --  first supplieradmin created must have the AdminID of 0
      MySupplierAdmin := SupplierAdmins.Get_Element
                         (Self.X.Suppliers, Integer (Id + 1));

      Leave (Self_Mutex);

      return MySupplierAdmin;
   end Get_SupplierAdmin;

   ----------------------------
   -- Get_All_ConsumerAdmins --
   ----------------------------

   function Get_All_ConsumerAdmins
     (Self : access Object)
      return CosNotifyChannelAdmin.AdminIDSeq
   is
      MySeq : CosNotifyChannelAdmin.AdminIDSeq;
   begin
      pragma Debug (O ("get_all_consumeradmins in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MySeq := Self.X.CIDSeq;
      Leave (Self_Mutex);

      return MySeq;
   end Get_All_ConsumerAdmins;

   ----------------------------
   -- Get_All_SupplierAdmins --
   ----------------------------

   function Get_All_SupplierAdmins
     (Self : access Object)
      return CosNotifyChannelAdmin.AdminIDSeq
   is
      MySeq : CosNotifyChannelAdmin.AdminIDSeq;
   begin
      pragma Debug (O ("get_all_supplieradmins in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MySeq := Self.X.SIDSeq;
      Leave (Self_Mutex);

      return MySeq;
   end Get_All_SupplierAdmins;

   -------------
   -- Get_QoS --
   -------------

   function Get_QoS
     (Self : access Object)
      return CosNotification.QoSProperties
   is
      MyProp : CosNotification.QoSProperties;
   begin
      pragma Debug (O ("get_qos in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyProp := Self.X.QoSPropSeq;
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
      Consumers  : CORBA.Long;
      My_Ptr     : EventChannel.Impl.Object_Ptr;
      MyProp     : CosNotification.Property;
      MyError    : CosNotification.PropertyError;
      MyErrCode  : CosNotification.QoSError_code;
      MyRange    : CosNotification.PropertyRange;
      MyErrorSeq : CosNotification.PropertyErrorSeq;
      SeqLen     : Integer;
      Suppliers  : CORBA.Long;
   begin
      pragma Debug (O ("set_qos in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      My_Ptr := Self.X.This;
      Leave (Self_Mutex);

      Consumers := GetTotalConsumers (My_Ptr);
      Suppliers := GetTotalSuppliers (My_Ptr);

      SeqLen := Length (QoS);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (QoS, Index);
         if MyProp.name = "EventReliability" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "ConnectionReliability" then
            if Consumers > 0
              or else Suppliers > 0
            then
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
         if MyProp.name = "EventReliability" then
            Replace_Element (Self.X.QoSPropSeq, 1, MyProp);
         elsif MyProp.name = "ConnectionReliability" then
            Replace_Element (Self.X.QoSPropSeq, 2, MyProp);
         elsif MyProp.name = "Priority" then
            Replace_Element (Self.X.QoSPropSeq, 3, MyProp);
         elsif MyProp.name = "OrderPolicy" then
            Replace_Element (Self.X.QoSPropSeq, 4, MyProp);
         else
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
      My_Ptr       : EventChannel.Impl.Object_Ptr;
      MyProp       : CosNotification.Property;
      MyError      : CosNotification.PropertyError;
      MyErrCode    : CosNotification.QoSError_code;
      MyNamedRange : CosNotification.NamedPropertyRange;
      MyRange      : CosNotification.PropertyRange;
      MyErrorSeq   : CosNotification.PropertyErrorSeq;
      SeqLen       : Integer;
      Suppliers    : CORBA.Long;
   begin
      pragma Debug (O ("validate_qos in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      My_Ptr := Self.X.This;
      Leave (Self_Mutex);

      Consumers := GetTotalConsumers (My_Ptr);
      Suppliers := GetTotalSuppliers (My_Ptr);

      SeqLen := Length (Required_QoS);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Required_QoS, Index);
         if MyProp.name = "EventReliability" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "ConnectionReliability" then
            if Consumers > 0 or Suppliers > 0 then
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
         if MyProp.name = "EventReliability" then
               MyRange      := (From_Any (MyProp.value),
                                To_Any (CORBA.Short (0)));
               MyNamedRange := (MyProp.name, MyRange);
               Append (Available_QoS, MyNamedRange);
         elsif MyProp.name = "ConnectionReliability" then
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

   ---------------
   -- Get_Admin --
   ---------------

   function Get_Admin
     (Self : access Object)
     return CosNotification.AdminProperties
   is
      MyProp : CosNotification.AdminProperties;
   begin
      pragma Debug (O ("get_admin in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyProp := Self.X.AdmPropSeq;
      Leave (Self_Mutex);

      return MyProp;
   end Get_Admin;

   ---------------
   -- Set_Admin --
   ---------------

   procedure Set_Admin
     (Self  : access Object;
      Admin : CosNotification.AdminProperties)
   is
      Consumers  : CORBA.Long;
      My_Ptr     : EventChannel.Impl.Object_Ptr;
      MyProp     : CosNotification.Property;
      MyError    : CosNotification.PropertyError;
      MyErrCode  : CosNotification.QoSError_code;
      MyRange    : CosNotification.PropertyRange;
      MyErrorSeq : CosNotification.PropertyErrorSeq;
      SeqLen     : Integer;
      Suppliers  : CORBA.Long;
   begin
      pragma Debug (O ("set_admin in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      My_Ptr := Self.X.This;
      Leave (Self_Mutex);

      Consumers := GetTotalConsumers (My_Ptr);
      Suppliers := GetTotalSuppliers (My_Ptr);

      SeqLen := Length (Admin);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Admin, Index);
         if MyProp.name = "MaxQueueLength" then
            if CORBA.Long'(From_Any (MyProp.value)) < 0 then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Long (0)), To_Any (CORBA.Long (0)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "MaxConsumers" then
            if CORBA.Long'(From_Any (MyProp.value)) < 0 then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Long (0)), To_Any (CORBA.Long (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            elsif CORBA.Long'(From_Any (MyProp.value)) < Consumers then
               MyErrCode := UNAVAILABLE_VALUE;
               MyRange   := (To_Any (Consumers),
                             To_Any (CORBA.Long (0)));
               MyError   := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "MaxSuppliers" then
            if CORBA.Long'(From_Any (MyProp.value)) < 0 then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Long (0)), To_Any (CORBA.Long (0)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            elsif CORBA.Long'(From_Any (MyProp.value)) < Suppliers then
               MyErrCode := UNAVAILABLE_VALUE;
               MyRange   := (To_Any (Suppliers),
                             To_Any (CORBA.Long (0)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "RejectNewEvents" then
            if not CORBA.Boolean'(From_Any (MyProp.value))
              and then CORBA.Boolean'(From_Any (MyProp.value))
            then
               MyErrCode := BAD_TYPE;
               MyRange   := (To_Any (CORBA.Boolean (True)),
                             To_Any (CORBA.Boolean (False)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         else
            MyErrCode := BAD_PROPERTY;
            MyError := (MyErrCode, MyProp.name, MyRange);
            Append (MyErrorSeq, MyError);
         end if;
      end loop;

      if Length (MyErrorSeq) > 0 then
         CosNotification.Helper.Raise_UnsupportedAdmin
           ((CORBA.IDL_Exception_Members with admin_err => MyErrorSeq));
      end if;

      Enter (Self_Mutex);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Admin, Index);
         if MyProp.name = "MaxQueueLength" then
            Replace_Element (Self.X.AdmPropSeq, 1, MyProp);
         elsif MyProp.name = "MaxConsumers" then
            Replace_Element (Self.X.AdmPropSeq, 2, MyProp);
         elsif MyProp.name = "MaxSuppliers" then
            Replace_Element (Self.X.AdmPropSeq, 3, MyProp);
         else
            Replace_Element (Self.X.AdmPropSeq, 4, MyProp);
         end if;
      end loop;
      Leave (Self_Mutex);

   end Set_Admin;

   -------------------
   -- For_Consumers --
   -------------------

   function For_Consumers
     (Self : access Object)
     return CosEventChannelAdmin.ConsumerAdmin.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyConsumerAdmin : CosEventChannelAdmin.ConsumerAdmin.Ref;
   begin
      pragma Debug (O ("for_consumers in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyConsumerAdmin;
   end For_Consumers;

   -------------------
   -- For_Suppliers --
   -------------------

   function For_Suppliers
     (Self : access Object)
     return CosEventChannelAdmin.SupplierAdmin.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MySupplierAdmin : CosEventChannelAdmin.SupplierAdmin.Ref;
   begin
      pragma Debug (O ("for_suppliers in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySupplierAdmin;
   end For_Suppliers;

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
      pragma Debug (O ("destroy in eventchannel"));

      null;
   end Destroy;

   ------------
   -- Create --
   ------------

   function Create
     (Factory       : CosNotifyChannelAdmin.EventChannelFactory.Ref;
      Initial_QoS   : CosNotification.QoSProperties;
      Initial_Admin : CosNotification.AdminProperties)
      return Object_Ptr
   is
      AdminID     : CosNotifyChannelAdmin.AdminID;
      Channel     : Object_Ptr;
      Consumer    : CosNotifyChannelAdmin.ConsumerAdmin.Impl.Object_Ptr;
      ConsumerRef : CosNotifyChannelAdmin.ConsumerAdmin.Ref;
      My_Ref      : EventChannel.Ref;
      MyError     : CosNotification.PropertyError;
      MyErrCode   : CosNotification.QoSError_code;
      MyErrorSeq  : CosNotification.PropertyErrorSeq;
      MyRange     : CosNotification.PropertyRange;
      MyProp      : CosNotification.Property;
      MyPropName  : CORBA.String;
      SeqLen      : Integer;
      Supplier    : CosNotifyChannelAdmin.SupplierAdmin.Impl.Object_Ptr;
      SupplierRef : CosNotifyChannelAdmin.SupplierAdmin.Ref;
   begin
      pragma Debug (O ("create eventchannel"));

      --  Parse the passed QoS Sequence to check for
      --  valid names and values
      SeqLen := Length (Initial_QoS);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Initial_QoS, Index);
         if MyProp.name = "EventReliability" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "ConnectionReliability" then
            if CORBA.Short'(From_Any (MyProp.value)) /= 0
              and then CORBA.Short'(From_Any (MyProp.value)) /= 1
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (0)),
                             To_Any (CORBA.Short (0)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "Priority" then
            if CORBA.Short'(From_Any (MyProp.value))
              not in -32_767 .. 32_767
            then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Short (-32767)),
                             To_Any (CORBA.Short (32767)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "StartTime" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StopTime" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "Timeout" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError := (MyErrCode, MyProp.name, MyRange);
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
               MyError := (MyErrCode, MyProp.name, MyRange);
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
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "MaximumBatchSize" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "PacingInterval" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StartTimeSupported" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "StopTimeSupported" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         elsif MyProp.name = "MaxEventsPerConsumer" then
               MyErrCode := UNSUPPORTED_PROPERTY;
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
         else
            MyErrCode := BAD_PROPERTY;
            MyError := (MyErrCode, MyProp.name, MyRange);
            Append (MyErrorSeq, MyError);
         end if;
      end loop;

      if Length (MyErrorSeq) > 0 then
         CosNotification.Helper.Raise_UnsupportedQoS
           ((CORBA.IDL_Exception_Members with qos_err => MyErrorSeq));
      end if;

      --  Parse the passed Admin Sequence to check for
      --  valid names and values
      SeqLen := Length (Initial_Admin);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Initial_Admin, Index);
         if MyProp.name = "MaxQueueLength" then
            if CORBA.Long'(From_Any (MyProp.value)) < 0 then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Long (0)), To_Any (CORBA.Long (0)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "MaxConsumers" then
            if CORBA.Long'(From_Any (MyProp.value)) < 0 then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Long (0)), To_Any (CORBA.Long (0)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "MaxSuppliers" then
            if CORBA.Long'(From_Any (MyProp.value)) < 0 then
               MyErrCode := BAD_VALUE;
               MyRange   := (To_Any (CORBA.Long (0)), To_Any (CORBA.Long (0)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         elsif MyProp.name = "RejectNewEvents" then
            if CORBA.Boolean'(From_Any (MyProp.value))
              and then CORBA.Boolean'(From_Any (MyProp.value))
            then
               MyErrCode := BAD_TYPE;
               MyRange   := (To_Any (CORBA.Boolean (True)),
                             To_Any (CORBA.Boolean (False)));
               MyError := (MyErrCode, MyProp.name, MyRange);
               Append (MyErrorSeq, MyError);
            end if;
         else
            MyErrCode := BAD_PROPERTY;
            MyError := (MyErrCode, MyProp.name, MyRange);
            Append (MyErrorSeq, MyError);
         end if;
      end loop;

      if Length (MyErrorSeq) > 0 then
         CosNotification.Helper.Raise_UnsupportedAdmin
           ((CORBA.IDL_Exception_Members with admin_err => MyErrorSeq));
      end if;

      Channel           := new Object;
      Channel.X         := new Event_Channel_Record;
      Channel.X.This    := Channel;
      Channel.X.Factory := Factory;
      Initiate_Servant (Servant (Channel), My_Ref);

      --  Create Default QoS Properties Sequence for the channel
      MyPropName := To_CORBA_String ("EventReliability");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Short (0)));
      Append (Channel.X.QoSPropSeq, MyProp);

      MyPropName := To_CORBA_String ("ConnectionReliability");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Short (0)));
      Append (Channel.X.QoSPropSeq, MyProp);

      MyPropName := To_CORBA_String ("Priority");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Short (0)));
      Append (Channel.X.QoSPropSeq, MyProp);

      MyPropName := To_CORBA_String ("OrderPolicy");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Short (2)));
      Append (Channel.X.QoSPropSeq, MyProp);

      MyPropName := To_CORBA_String ("DiscardPolicy");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Short (2)));
      Append (Channel.X.QoSPropSeq, MyProp);

      --  Replace the values in Channel's QoSProperties Sequence
      --  with those of Passed AdminProperties Sequence
      SeqLen := Length (Initial_QoS);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Initial_QoS, Index);
         if MyProp.name = "EventReliability" then
            Replace_Element (Channel.X.QoSPropSeq, 1, MyProp);
         elsif MyProp.name = "ConnectionReliability" then
            Replace_Element (Channel.X.QoSPropSeq, 2, MyProp);
         elsif MyProp.name = "Priority" then
            Replace_Element (Channel.X.QoSPropSeq, 3, MyProp);
         elsif MyProp.name = "OrderPolicy" then
            Replace_Element (Channel.X.QoSPropSeq, 4, MyProp);
         else
            Replace_Element (Channel.X.QoSPropSeq, 5, MyProp);
         end if;
      end loop;

      --  AdminID of the default consumeradmin object created by
      --  eventchannel must be 0 but in Ada Sequence Indexing starts
      --  from 1 so every time we need to append the ID number
      --  into AdminIDSeq we do it before the appending the actual
      --  Admin Ref into its respective Sequence
      AdminID := CosNotifyChannelAdmin.AdminID
                 (ConsumerAdmins.Length (Channel.X.Consumers));
      Append (Channel.X.CIDSeq, AdminID);

      --  create a default consumeradmin and append its reference
      --  to ConsumerAdmins sequence
      Consumer := CosNotifyChannelAdmin.ConsumerAdmin.Impl.Create
                  (My_Ref, Channel.X.QoSPropSeq, AdminID);
      Servant_To_Reference (Servant (Consumer), ConsumerRef);
      Channel.X.Consumer := ConsumerRef;
      ConsumerAdmins.Append (Channel.X.Consumers, Channel.X.Consumer);

      --  AdminID of the default supplieradmin object created by
      --  eventchannel must be 0 but in Ada Sequence Indexing starts
      --  from 1 so every time we need to append the ID number
      --  into AdminIDSeq we do it before the appending the actual
      --  Admin Ref into its respective Sequence
      AdminID := CosNotifyChannelAdmin.AdminID
                 (SupplierAdmins.Length (Channel.X.Suppliers));
      Append (Channel.X.SIDSeq, AdminID);

      --  create a default supplieradmin and append its reference
      --  to SupplierAdmins sequence
      Supplier := CosNotifyChannelAdmin.SupplierAdmin.Impl.Create
                  (My_Ref, Channel.X.QoSPropSeq, AdminID);
      Servant_To_Reference (Servant (Supplier), SupplierRef);
      Channel.X.Supplier := SupplierRef;
      SupplierAdmins.Append (Channel.X.Suppliers, Channel.X.Supplier);

      --  Create Default Admin Properties Sequence for the channel
      MyPropName := To_CORBA_String ("MaxQueueLength");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Long (0)));
      Append (Channel.X.AdmPropSeq, MyProp);

      MyPropName := To_CORBA_String ("MaxConsumers");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Long (0)));
      Append (Channel.X.AdmPropSeq, MyProp);

      MyPropName := To_CORBA_String ("MaxSuppliers");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Long (0)));
      Append (Channel.X.AdmPropSeq, MyProp);

      MyPropName := To_CORBA_String ("RejectNewEvents");
      MyProp     := (CosNotification.PropertyName (MyPropName),
                     To_Any (CORBA.Boolean (False)));
      Append (Channel.X.AdmPropSeq, MyProp);

      --  Replace the values in Channel's AdminProperties Sequence
      --  with those of Passed AdminProperties Sequence
      SeqLen := Length (Initial_Admin);
      for Index in 1 .. SeqLen loop
         MyProp := Get_Element (Initial_Admin, Index);
         if MyProp.name = "MaxQueueLength" then
            Replace_Element (Channel.X.AdmPropSeq, 1, MyProp);
         elsif MyProp.name = "MaxConsumers" then
            Replace_Element (Channel.X.AdmPropSeq, 2, MyProp);
         elsif MyProp.name = "MaxSuppliers" then
            Replace_Element (Channel.X.AdmPropSeq, 3, MyProp);
         else
            Replace_Element (Channel.X.AdmPropSeq, 4, MyProp);
         end if;
      end loop;

      return Channel;
   end Create;

   -----------------------
   -- GetTotalConsumers --
   -----------------------

   function GetTotalConsumers
     (Self : access Object)
     return CORBA.Long
   is
      Res     : CORBA.Long;
   begin
      pragma Debug (O ("gettotalconsumers in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      declare
         CTotal : CORBA.Long := 0;
         SAdmins : constant SupplierAdmins.Element_Array
            := SupplierAdmins.To_Element_Array (Self.X.Suppliers);
         Supplier : CosNotifyChannelAdmin.SupplierAdmin.Impl.Object_Ptr;
      begin
         Leave (Self_Mutex);
         for J in SAdmins'Range loop
            Reference_To_Servant (SAdmins (J), Servant (Supplier));
            CTotal   := CTotal +
                      CosNotifyChannelAdmin.SupplierAdmin.Impl.
                      GetTotalConsumers (Supplier);
         end loop;
         Res := CTotal;
      end;

      return Res;
   end GetTotalConsumers;

   -----------------------
   -- GetTotalSuppliers --
   -----------------------

   function GetTotalSuppliers
     (Self : access Object)
     return CORBA.Long
   is
      Res     : CORBA.Long;
   begin
      pragma Debug (O ("gettotalsuppliers in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      declare
         STotal  : CORBA.Long := 0;
         CAdmins : constant ConsumerAdmins.Element_Array
            := ConsumerAdmins.To_Element_Array (Self.X.Consumers);
         Consumer : CosNotifyChannelAdmin.ConsumerAdmin.Impl.Object_Ptr;
      begin
         Leave (Self_Mutex);
         for J in CAdmins'Range loop
            Reference_To_Servant (CAdmins (J), Servant (Consumer));
            STotal   := STotal +
                      CosNotifyChannelAdmin.ConsumerAdmin.Impl.
                      GetTotalSuppliers (Consumer);
         end loop;
         Res := STotal;
      end;

      return Res;
   end GetTotalSuppliers;

   ----------
   -- Post --
   ----------

   procedure Post
     (Self : access Object;
      Data : CORBA.Any)
   is
      Consumer    : CosNotifyChannelAdmin.ConsumerAdmin.Impl.Object_Ptr;
   begin
      pragma Debug (O ("post new data from eventchannel to consumeradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      declare
         Consumers : constant ConsumerAdmins.Element_Array
           := ConsumerAdmins.To_Element_Array (Self.X.Consumers);
      begin
         Leave (Self_Mutex);

         pragma Debug
           (O ("post new data from eventchannel to consumeradmins"));

         for J in Consumers'Range loop
            Reference_To_Servant (Consumers (J), Servant (Consumer));
            CosNotifyChannelAdmin.ConsumerAdmin.Impl.Post
              (Consumer, Data);
         end loop;
      end;

   end Post;

   ---------------------
   -- Structured_Post --
   ---------------------

   procedure Structured_Post
     (Self         : access Object;
      Notification : CosNotification.StructuredEvent)
   is
      Consumer    : CosNotifyChannelAdmin.ConsumerAdmin.Impl.Object_Ptr;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("post structured event from eventchannel to consumeradmin"));

      Enter (Self_Mutex);
      declare
         Consumers : constant ConsumerAdmins.Element_Array
           := ConsumerAdmins.To_Element_Array (Self.X.Consumers);
      begin
         Leave (Self_Mutex);

         pragma Debug
           (O ("post new structured from eventchannel to consumeradmins"));

         for J in Consumers'Range loop
            Reference_To_Servant (Consumers (J), Servant (Consumer));
            CosNotifyChannelAdmin.ConsumerAdmin.Impl.Structured_Post
              (Consumer, Notification);
         end loop;
      end;

   end Structured_Post;

   -------------------
   -- Sequence_Post --
   -------------------

   procedure Sequence_Post
     (Self          : access Object;
      Notifications : CosNotification.EventBatch)
   is
      Consumer    : CosNotifyChannelAdmin.ConsumerAdmin.Impl.Object_Ptr;
   begin
      Ensure_Initialization;
      pragma Debug
      (O ("post sequence of structured event from " &
          "eventchannel to consumeradmin"));

      Enter (Self_Mutex);
      declare
         Consumers : constant ConsumerAdmins.Element_Array
           := ConsumerAdmins.To_Element_Array (Self.X.Consumers);
      begin
         Leave (Self_Mutex);

         pragma Debug
           (O ("post new sequence of structured from " &
               "eventchannel to consumeradmins"));

         for J in Consumers'Range loop
            Reference_To_Servant (Consumers (J), Servant (Consumer));
            CosNotifyChannelAdmin.ConsumerAdmin.Impl.Sequence_Post
              (Consumer, Notifications);
         end loop;
      end;

   end Sequence_Post;

   -----------------------
   -- TestConsumerLimit --
   -----------------------

   function TestConsumerLimit
     (Self : access Object)
     return CORBA.Boolean
   is
      MyProp   : CosNotification.Property;
      My_Ptr   : EventChannel.Impl.Object_Ptr;
      MaxCons  : CORBA.Long;
      PresCons : CORBA.Long;
      Res      : CORBA.Boolean;
   begin
      pragma Debug (O ("testconsumerlimit in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyProp  := Get_Element (Self.X.AdmPropSeq, 2);
      Leave (Self_Mutex);
      MaxCons := CORBA.Long'(From_Any (MyProp.value));

      if MaxCons = 0 then
         Res := True;
      else
         Enter (Self_Mutex);
         My_Ptr := Self.X.This;
         Leave (Self_Mutex);
         PresCons := GetTotalConsumers (My_Ptr);
         if PresCons < MaxCons then
            Res := True;
         else
            Res := False;
         end if;
      end if;

      return Res;
   end TestConsumerLimit;

   -----------------------
   -- TestSupplierLimit --
   -----------------------

   function TestSupplierLimit
     (Self : access Object)
     return CORBA.Boolean
   is
      MyProp   : CosNotification.Property;
      My_Ptr   : EventChannel.Impl.Object_Ptr;
      MaxSups  : CORBA.Long;
      PresSups : CORBA.Long;
      Res      : CORBA.Boolean;
   begin
      pragma Debug (O ("testsupplierlimit in eventchannel"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      MyProp  := Get_Element (Self.X.AdmPropSeq, 3);
      Leave (Self_Mutex);
      MaxSups := CORBA.Long'(From_Any (MyProp.value));

      if MaxSups = 0 then
         Res := True;
      else
         Enter (Self_Mutex);
         My_Ptr := Self.X.This;
         Leave (Self_Mutex);
         PresSups := GetTotalSuppliers (My_Ptr);
         if PresSups < MaxSups then
            Res := True;
         else
            Res := False;
         end if;
      end if;

      return Res;
   end TestSupplierLimit;

end CosNotifyChannelAdmin.EventChannel.Impl;
