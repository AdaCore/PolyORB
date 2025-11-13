------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       R T C O R B A . H E L P E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2017, Free Software Foundation, Inc.          --
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

-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC version 2.3.0w.
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with RTCORBA.ProtocolProperties.Helper;
with IOP.Helper;
with CORBA.Object.Helper;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.CORBA_Helper);
with PolyORB.Utils.Unchecked_Deallocation;
with PolyORB.Types;

package body RTCORBA.Helper is

   function From_Any (Item : CORBA.Any) return RTCORBA.NativePriority
   is
   begin
      return RTCORBA.NativePriority (CORBA.Short'(CORBA.From_Any (Item)));
   end From_Any;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return RTCORBA.NativePriority is
   begin
      return RTCORBA.NativePriority (CORBA.Short'(CORBA.From_Any (C)));
   end From_Any;

   function To_Any
     (Item : RTCORBA.NativePriority) return CORBA.Any
   is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_NativePriority);
      return Result;
   end To_Any;

   function From_Any (Item : CORBA.Any) return RTCORBA.Priority
   is
   begin
      return RTCORBA.Priority (CORBA.Short'(CORBA.From_Any (Item)));
   end From_Any;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return RTCORBA.Priority is
   begin
      return RTCORBA.Priority (CORBA.Short'(CORBA.From_Any (C)));
   end From_Any;

   function To_Any
     (Item : RTCORBA.Priority) return CORBA.Any
   is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_Priority);
      return Result;
   end To_Any;

   function From_Any (Item : CORBA.Any) return RTCORBA.ThreadpoolId
   is
   begin
      return RTCORBA.ThreadpoolId (CORBA.Unsigned_Long'(CORBA.From_Any (Item)));
   end From_Any;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return RTCORBA.ThreadpoolId is
   begin
      return RTCORBA.ThreadpoolId (CORBA.Unsigned_Long'(CORBA.From_Any (C)));
   end From_Any;

   function To_Any
     (Item : RTCORBA.ThreadpoolId) return CORBA.Any
   is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Unsigned_Long (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_ThreadpoolId);
      return Result;
   end To_Any;

   type Ptr_ThreadpoolLane is access all RTCORBA.ThreadpoolLane;
   type Content_ThreadpoolLane is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_ThreadpoolLane;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_ThreadpoolLane;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   function Get_Aggregate_Count
     (ACC : Content_ThreadpoolLane) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_ThreadpoolLane;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_ThreadpoolLane;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_ThreadpoolLane);

   function Get_Aggregate_Element
     (ACC   : not null access Content_ThreadpoolLane;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC);
   begin
      Mech.all := PolyORB.Any.By_Reference;
      case Index is
         when 0 =>
            return CORBA.Wrap (CORBA.Short (ACC.V.lane_priority)'Unrestricted_Access);
         when 1 =>
            return CORBA.Wrap (ACC.V.static_threads'Unrestricted_Access);
         when 2 =>
            return CORBA.Wrap (ACC.V.dynamic_threads'Unrestricted_Access);
         when others =>
            raise Constraint_Error;
      end case;
   end Get_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_ThreadpoolLane) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 3;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_ThreadpoolLane;
      Count : PolyORB.Types.Unsigned_Long)
   is
      use type PolyORB.Types.Unsigned_Long;
      pragma Unreferenced (ACC);
   begin
      if Count /= 3 then
         raise Program_Error;
      end if;
   end Set_Aggregate_Count;

   function Clone
     (ACC  : Content_ThreadpoolLane;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_ThreadpoolLane then
            return null;
         end if;
         Target := Into;
         Content_ThreadpoolLane (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_ThreadpoolLane;
         Content_ThreadpoolLane (Target.all).V := new RTCORBA.ThreadpoolLane'(ACC.V.all);
      end if;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_ThreadpoolLane)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => RTCORBA.ThreadpoolLane, Name => Ptr_ThreadpoolLane);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access RTCORBA.ThreadpoolLane) return PolyORB.Any.Content'Class is
   begin
      return Content_ThreadpoolLane'(PolyORB.Any.Aggregate_Content with V => Ptr_ThreadpoolLane (X));
   end Wrap;

   function From_Any (Item : CORBA.Any) return RTCORBA.ThreadpoolLane is
      Index : CORBA.Any;
      Result_lane_priority : RTCORBA.Priority;
      Result_static_threads : CORBA.Unsigned_Long;
      Result_dynamic_threads : CORBA.Unsigned_Long;
   begin
      Index := CORBA.Internals.Get_Aggregate_Element (Item,
                                            RTCORBA.Helper.TC_Priority,
                                            CORBA.Unsigned_Long ( 0));
      Result_lane_priority := RTCORBA.Helper.From_Any (Index);
      Index := CORBA.Internals.Get_Aggregate_Element (Item,
                                            CORBA.TC_Unsigned_Long,
                                            CORBA.Unsigned_Long ( 1));
      Result_static_threads := CORBA.From_Any (Index);
      Index := CORBA.Internals.Get_Aggregate_Element (Item,
                                            CORBA.TC_Unsigned_Long,
                                            CORBA.Unsigned_Long ( 2));
      Result_dynamic_threads := CORBA.From_Any (Index);
      return
         (lane_priority => Result_lane_priority,
          static_threads => Result_static_threads,
          dynamic_threads => Result_dynamic_threads);
   end From_Any;

   function To_Any
     (Item : RTCORBA.ThreadpoolLane) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_ThreadpoolLane);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result, RTCORBA.Helper.To_Any (Item.lane_priority));
      CORBA.Internals.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.static_threads));
      CORBA.Internals.Add_Aggregate_Element
         (Result, CORBA.To_Any (Item.dynamic_threads));
      return Result;
   end To_Any;

   function IDL_SEQUENCE_RTCORBA_ThreadpoolLane_Element_Wrap (X : access RTCORBA.ThreadpoolLane) return PolyORB.Any.Content'Class is
   begin
      return RTCORBA.Helper.Wrap (X.all'Unrestricted_Access);
   end IDL_SEQUENCE_RTCORBA_ThreadpoolLane_Element_Wrap;

   package IDL_SEQUENCE_RTCORBA_ThreadpoolLane_Helper is new IDL_SEQUENCE_RTCORBA_ThreadpoolLane.CORBA_Helper
     (Element_To_Any   => RTCORBA.Helper.To_Any,
      Element_From_Any => RTCORBA.Helper.From_Any,
      Element_Wrap     => IDL_SEQUENCE_RTCORBA_ThreadpoolLane_Element_Wrap);

   function From_Any (Item : CORBA.Any) return RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane.Sequence
     renames IDL_SEQUENCE_RTCORBA_ThreadpoolLane_Helper.From_Any;

   function To_Any
     (Item : RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane.Sequence) return CORBA.Any
     renames IDL_SEQUENCE_RTCORBA_ThreadpoolLane_Helper.To_Any;

   function Wrap (X : access RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane.Sequence) return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_RTCORBA_ThreadpoolLane_Helper.Wrap;

   function From_Any (Item : CORBA.Any) return RTCORBA.ThreadpoolLanes
   is
   begin
      return RTCORBA.ThreadpoolLanes (RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane.Sequence'(RTCORBA.Helper.From_Any (Item)));
   end From_Any;

   function To_Any
     (Item : RTCORBA.ThreadpoolLanes) return CORBA.Any
   is
      Result : CORBA.Any := RTCORBA.Helper.To_Any (RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane.Sequence (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_ThreadpoolLanes);
      return Result;
   end To_Any;

   type Ptr_PriorityModel is access all RTCORBA.PriorityModel;
   type Content_PriorityModel is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_PriorityModel;
      Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_PriorityModel;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   procedure Set_Aggregate_Element
     (ACC    : in out Content_PriorityModel;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);
   function Get_Aggregate_Count
     (ACC : Content_PriorityModel) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_PriorityModel;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_PriorityModel;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_PriorityModel);

   function Get_Aggregate_Element
     (ACC   : not null access Content_PriorityModel;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC, Index);
      pragma Suppress (All_Checks);
   begin
      ACC.Repr_Cache := RTCORBA.PriorityModel'Pos (ACC.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (ACC.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Content_PriorityModel;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use type PolyORB.Types.Unsigned_Long;
      pragma Assert (Index = 0);
   begin
      ACC.V.all := RTCORBA.PriorityModel'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_PriorityModel) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 1;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_PriorityModel;
      Count : PolyORB.Types.Unsigned_Long)
   is
      use type PolyORB.Types.Unsigned_Long;
      pragma Unreferenced (ACC);
   begin
      if Count /= 1 then
         raise Program_Error;
      end if;
   end Set_Aggregate_Count;

   function Clone
     (ACC  : Content_PriorityModel;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_PriorityModel then
            return null;
         end if;
         Target := Into;
         Content_PriorityModel (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_PriorityModel;
         Content_PriorityModel (Target.all).V := new RTCORBA.PriorityModel'(ACC.V.all);
      end if;
      Content_PriorityModel (Target.all).Repr_Cache:= ACC.Repr_Cache;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_PriorityModel)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => RTCORBA.PriorityModel, Name => Ptr_PriorityModel);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access RTCORBA.PriorityModel) return PolyORB.Any.Content'Class is
   begin
      return Content_PriorityModel'(PolyORB.Any.Aggregate_Content with V => Ptr_PriorityModel (X),
        Repr_Cache => 0);
   end Wrap;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return RTCORBA.PriorityModel is
      ACC : PolyORB.Any.Aggregate_Content'Class renames PolyORB.Any.Aggregate_Content'Class (PolyORB.Any.Get_Value (C).all);
      El_M  : aliased PolyORB.Any.Mechanism := PolyORB.Any.By_Value;
      El_CC : aliased PolyORB.Any.Content'Class :=
        PolyORB.Any.Get_Aggregate_Element (ACC'Access,
                                           PolyORB.Any.TC_Unsigned_Long,
                                           0, El_M'Access);
      El_C : PolyORB.Any.Any_Container;
   begin
      PolyORB.Any.Set_Type (El_C, PolyORB.Any.TC_Unsigned_Long);
      PolyORB.Any.Set_Value (El_C, El_CC'Unchecked_Access);
      return PriorityModel'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (El_C)));
   end From_Any;

   function From_Any (Item : CORBA.Any) return RTCORBA.PriorityModel is
   begin
      return From_Any (CORBA.Get_Container (Item).all);
   end From_Any;

   function To_Any
     (Item : RTCORBA.PriorityModel) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_PriorityModel);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (PriorityModel'Pos (Item))));
      return Result;
   end To_Any;

   function Unchecked_To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.ProtocolProperties_Forward.Ref
   is
      Result : RTCORBA.ProtocolProperties_Forward.Ref;
   begin
      ProtocolProperties_Forward.Set (Result,
           CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.ProtocolProperties_Forward.Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, "IDL:omg.org/RTCORBA/ProtocolProperties:1.0") then
         return Unchecked_To_Ref (The_Ref);
      end if;
      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Ref;

   type Ptr_Protocol is access all RTCORBA.Protocol;
   type Content_Protocol is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_Protocol;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_Protocol;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   function Get_Aggregate_Count
     (ACC : Content_Protocol) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_Protocol;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_Protocol;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_Protocol);

   function Get_Aggregate_Element
     (ACC   : not null access Content_Protocol;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC);
   begin
      Mech.all := PolyORB.Any.By_Reference;
      case Index is
         when 0 =>
            return CORBA.Wrap (CORBA.Unsigned_Long (ACC.V.protocol_type)'Unrestricted_Access);
         when 1 =>
            return CORBA.Object.Helper.Wrap (CORBA.Object.Ref (ACC.V.orb_protocol_properties)'Unrestricted_Access);
         when 2 =>
            return CORBA.Object.Helper.Wrap (CORBA.Object.Ref (ACC.V.transport_protocol_properties)'Unrestricted_Access);
         when others =>
            raise Constraint_Error;
      end case;
   end Get_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_Protocol) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 3;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_Protocol;
      Count : PolyORB.Types.Unsigned_Long)
   is
      use type PolyORB.Types.Unsigned_Long;
      pragma Unreferenced (ACC);
   begin
      if Count /= 3 then
         raise Program_Error;
      end if;
   end Set_Aggregate_Count;

   function Clone
     (ACC  : Content_Protocol;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_Protocol then
            return null;
         end if;
         Target := Into;
         Content_Protocol (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_Protocol;
         Content_Protocol (Target.all).V := new RTCORBA.Protocol'(ACC.V.all);
      end if;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_Protocol)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => RTCORBA.Protocol, Name => Ptr_Protocol);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access RTCORBA.Protocol) return PolyORB.Any.Content'Class is
   begin
      return Content_Protocol'(PolyORB.Any.Aggregate_Content with V => Ptr_Protocol (X));
   end Wrap;

   type Ptr_PriorityBand is access all RTCORBA.PriorityBand;
   type Content_PriorityBand is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_PriorityBand;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_PriorityBand;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   function Get_Aggregate_Count
     (ACC : Content_PriorityBand) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_PriorityBand;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_PriorityBand;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_PriorityBand);

   function Get_Aggregate_Element
     (ACC   : not null access Content_PriorityBand;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      pragma Unreferenced (TC);
   begin
      Mech.all := PolyORB.Any.By_Reference;
      case Index is
         when 0 =>
            return CORBA.Wrap (CORBA.Short (ACC.V.low)'Unrestricted_Access);
         when 1 =>
            return CORBA.Wrap (CORBA.Short (ACC.V.high)'Unrestricted_Access);
         when others =>
            raise Constraint_Error;
      end case;
   end Get_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_PriorityBand) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 2;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_PriorityBand;
      Count : PolyORB.Types.Unsigned_Long)
   is
      use type PolyORB.Types.Unsigned_Long;
      pragma Unreferenced (ACC);
   begin
      if Count /= 2 then
         raise Program_Error;
      end if;
   end Set_Aggregate_Count;

   function Clone
     (ACC  : Content_PriorityBand;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_PriorityBand then
            return null;
         end if;
         Target := Into;
         Content_PriorityBand (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_PriorityBand;
         Content_PriorityBand (Target.all).V := new RTCORBA.PriorityBand'(ACC.V.all);
      end if;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_PriorityBand)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => RTCORBA.PriorityBand, Name => Ptr_PriorityBand);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access RTCORBA.PriorityBand) return PolyORB.Any.Content'Class is
   begin
      return Content_PriorityBand'(PolyORB.Any.Aggregate_Content with V => Ptr_PriorityBand (X));
   end Wrap;

   function From_Any (Item : CORBA.Any) return RTCORBA.PriorityBand is
      Index : CORBA.Any;
      Result_low : RTCORBA.Priority;
      Result_high : RTCORBA.Priority;
   begin
      Index := CORBA.Internals.Get_Aggregate_Element (Item,
                                            RTCORBA.Helper.TC_Priority,
                                            CORBA.Unsigned_Long ( 0));
      Result_low := RTCORBA.Helper.From_Any (Index);
      Index := CORBA.Internals.Get_Aggregate_Element (Item,
                                            RTCORBA.Helper.TC_Priority,
                                            CORBA.Unsigned_Long ( 1));
      Result_high := RTCORBA.Helper.From_Any (Index);
      return
         (low => Result_low,
          high => Result_high);
   end From_Any;

   function To_Any
     (Item : RTCORBA.PriorityBand) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_PriorityBand);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result, RTCORBA.Helper.To_Any (Item.low));
      CORBA.Internals.Add_Aggregate_Element
         (Result, RTCORBA.Helper.To_Any (Item.high));
      return Result;
   end To_Any;

   function IDL_SEQUENCE_RTCORBA_PriorityBand_Element_Wrap (X : access RTCORBA.PriorityBand) return PolyORB.Any.Content'Class is
   begin
      return RTCORBA.Helper.Wrap (X.all'Unrestricted_Access);
   end IDL_SEQUENCE_RTCORBA_PriorityBand_Element_Wrap;

   package IDL_SEQUENCE_RTCORBA_PriorityBand_Helper is new IDL_SEQUENCE_RTCORBA_PriorityBand.CORBA_Helper
     (Element_To_Any   => RTCORBA.Helper.To_Any,
      Element_From_Any => RTCORBA.Helper.From_Any,
      Element_Wrap     => IDL_SEQUENCE_RTCORBA_PriorityBand_Element_Wrap);

   function From_Any (Item : CORBA.Any) return RTCORBA.IDL_SEQUENCE_RTCORBA_PriorityBand.Sequence
     renames IDL_SEQUENCE_RTCORBA_PriorityBand_Helper.From_Any;

   function To_Any
     (Item : RTCORBA.IDL_SEQUENCE_RTCORBA_PriorityBand.Sequence) return CORBA.Any
     renames IDL_SEQUENCE_RTCORBA_PriorityBand_Helper.To_Any;

   function Wrap (X : access RTCORBA.IDL_SEQUENCE_RTCORBA_PriorityBand.Sequence) return PolyORB.Any.Content'Class
     renames IDL_SEQUENCE_RTCORBA_PriorityBand_Helper.Wrap;

   function From_Any (Item : CORBA.Any) return RTCORBA.PriorityBands
   is
   begin
      return RTCORBA.PriorityBands (RTCORBA.IDL_SEQUENCE_RTCORBA_PriorityBand.Sequence'(RTCORBA.Helper.From_Any (Item)));
   end From_Any;

   function To_Any
     (Item : RTCORBA.PriorityBands) return CORBA.Any
   is
      Result : CORBA.Any := RTCORBA.Helper.To_Any (RTCORBA.IDL_SEQUENCE_RTCORBA_PriorityBand.Sequence (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_PriorityBands);
      return Result;
   end To_Any;

   procedure Deferred_Initialization is
   begin

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("NativePriority");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/NativePriority:1.0");
      begin
         TC_NativePriority := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => CORBA.TC_Short);
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("Priority");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/Priority:1.0");
      begin
         TC_Priority := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => CORBA.TC_Short);
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ThreadpoolId");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/ThreadpoolId:1.0");
      begin
         TC_ThreadpoolId := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => CORBA.TC_Unsigned_Long);
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ThreadpoolLane");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/ThreadpoolLane:1.0");
         Arg_Name_lane_priority : constant CORBA.String := CORBA.To_CORBA_String ("lane_priority");
         Arg_Name_static_threads : constant CORBA.String := CORBA.To_CORBA_String ("static_threads");
         Arg_Name_dynamic_threads : constant CORBA.String := CORBA.To_CORBA_String ("dynamic_threads");
      begin
         TC_ThreadpoolLane :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Struct);
         CORBA.Internals.Add_Parameter (TC_ThreadpoolLane, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_ThreadpoolLane, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_ThreadpoolLane, CORBA.To_Any (RTCORBA.Helper.TC_Priority));
         CORBA.Internals.Add_Parameter (TC_ThreadpoolLane, CORBA.To_Any (Arg_Name_lane_priority));
         CORBA.Internals.Add_Parameter (TC_ThreadpoolLane, CORBA.To_Any (CORBA.TC_Unsigned_Long));
         CORBA.Internals.Add_Parameter (TC_ThreadpoolLane, CORBA.To_Any (Arg_Name_static_threads));
         CORBA.Internals.Add_Parameter (TC_ThreadpoolLane, CORBA.To_Any (CORBA.TC_Unsigned_Long));
         CORBA.Internals.Add_Parameter (TC_ThreadpoolLane, CORBA.To_Any (Arg_Name_dynamic_threads));
      end;

      TC_IDL_SEQUENCE_RTCORBA_ThreadpoolLane :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (RTCORBA.Helper.TC_ThreadpoolLane, 0);
      IDL_SEQUENCE_RTCORBA_ThreadpoolLane_Helper.Initialize
        (Element_TC  => RTCORBA.Helper.TC_ThreadpoolLane,
         Sequence_TC => TC_IDL_SEQUENCE_RTCORBA_ThreadpoolLane);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ThreadpoolLanes");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/ThreadpoolLanes:1.0");
      begin
         TC_ThreadpoolLanes := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => RTCORBA.Helper.TC_IDL_SEQUENCE_RTCORBA_ThreadpoolLane);
      end;
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("PriorityModel");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/PriorityModel:1.0");
         CLIENT_PROPAGATED_Name : constant CORBA.String := CORBA.To_CORBA_String ("CLIENT_PROPAGATED");
         SERVER_DECLARED_Name : constant CORBA.String := CORBA.To_CORBA_String ("SERVER_DECLARED");
      begin
         TC_PriorityModel :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Enum);
         CORBA.Internals.Add_Parameter (TC_PriorityModel, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_PriorityModel, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_PriorityModel, CORBA.To_Any (CLIENT_PROPAGATED_Name));
         CORBA.Internals.Add_Parameter (TC_PriorityModel, CORBA.To_Any (SERVER_DECLARED_Name));
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ProtocolProperties");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/ProtocolProperties:1.0");
      begin
         TC_ProtocolProperties :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Object);
         CORBA.Internals.Add_Parameter (TC_ProtocolProperties, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_ProtocolProperties, CORBA.To_Any (Id));
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("Protocol");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/Protocol:1.0");
         Arg_Name_protocol_type : constant CORBA.String := CORBA.To_CORBA_String ("protocol_type");
         Arg_Name_orb_protocol_properties : constant CORBA.String := CORBA.To_CORBA_String ("orb_protocol_properties");
         Arg_Name_transport_protocol_properties : constant CORBA.String := CORBA.To_CORBA_String ("transport_protocol_properties");
      begin
         TC_Protocol :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Struct);
         CORBA.Internals.Add_Parameter (TC_Protocol, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_Protocol, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_Protocol, CORBA.To_Any (IOP.Helper.TC_ProfileId));
         CORBA.Internals.Add_Parameter (TC_Protocol, CORBA.To_Any (Arg_Name_protocol_type));
         CORBA.Internals.Add_Parameter (TC_Protocol, CORBA.To_Any (RTCORBA.ProtocolProperties.Helper.TC_ProtocolProperties));
         CORBA.Internals.Add_Parameter (TC_Protocol, CORBA.To_Any (Arg_Name_orb_protocol_properties));
         CORBA.Internals.Add_Parameter (TC_Protocol, CORBA.To_Any (RTCORBA.ProtocolProperties.Helper.TC_ProtocolProperties));
         CORBA.Internals.Add_Parameter (TC_Protocol, CORBA.To_Any (Arg_Name_transport_protocol_properties));
      end;

      TC_IDL_SEQUENCE_RTCORBA_Protocol :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (RTCORBA.Helper.TC_Protocol, 0);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ProtocolList");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/ProtocolList:1.0");
      begin
         TC_ProtocolList := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => RTCORBA.Helper.TC_IDL_SEQUENCE_RTCORBA_Protocol);
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("PriorityBand");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/PriorityBand:1.0");
         Arg_Name_low : constant CORBA.String := CORBA.To_CORBA_String ("low");
         Arg_Name_high : constant CORBA.String := CORBA.To_CORBA_String ("high");
      begin
         TC_PriorityBand :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Struct);
         CORBA.Internals.Add_Parameter (TC_PriorityBand, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_PriorityBand, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_PriorityBand, CORBA.To_Any (RTCORBA.Helper.TC_Priority));
         CORBA.Internals.Add_Parameter (TC_PriorityBand, CORBA.To_Any (Arg_Name_low));
         CORBA.Internals.Add_Parameter (TC_PriorityBand, CORBA.To_Any (RTCORBA.Helper.TC_Priority));
         CORBA.Internals.Add_Parameter (TC_PriorityBand, CORBA.To_Any (Arg_Name_high));
      end;

      TC_IDL_SEQUENCE_RTCORBA_PriorityBand :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (RTCORBA.Helper.TC_PriorityBand, 0);
      IDL_SEQUENCE_RTCORBA_PriorityBand_Helper.Initialize
        (Element_TC  => RTCORBA.Helper.TC_PriorityBand,
         Sequence_TC => TC_IDL_SEQUENCE_RTCORBA_PriorityBand);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("PriorityBands");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/PriorityBands:1.0");
      begin
         TC_PriorityBands := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => RTCORBA.Helper.TC_IDL_SEQUENCE_RTCORBA_PriorityBand);
      end;

   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"RTCORBA.Helper",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   =>
                  +"any"
          ,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;

end RTCORBA.Helper;
