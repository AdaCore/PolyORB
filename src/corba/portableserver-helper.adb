------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O R T A B L E S E R V E R . H E L P E R                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2017, Free Software Foundation, Inc.          --
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
with PolyORB.Utils.Unchecked_Deallocation;
with PolyORB.Types;
with PolyORB.Exceptions;
with PolyORB.Std;
with CORBA.Object.Helper;
with CORBA.IDL_SEQUENCES.Helper;
with PortableServer.POA.Helper;

package body PortableServer.Helper is

   function Unchecked_To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableServer.POA_Forward.Ref
   is
      Result : PortableServer.POA_Forward.Ref;
   begin
      POA_Forward.Set (Result,
           CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableServer.POA_Forward.Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, "IDL:omg.org/PortableServer/POA:1.0") then
         return Unchecked_To_Ref (The_Ref);
      end if;
      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Ref;

   function From_Any (Item : CORBA.Any) return PortableServer.ObjectId
   is
   begin
      return PortableServer.ObjectId (CORBA.IDL_SEQUENCES.OctetSeq'(CORBA.IDL_SEQUENCES.Helper.From_Any (Item)));
   end From_Any;

   function To_Any
     (Item : PortableServer.ObjectId) return CORBA.Any
   is
      Result : CORBA.Any := CORBA.IDL_SEQUENCES.Helper.To_Any (CORBA.IDL_SEQUENCES.OctetSeq (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_ObjectId);
      return Result;
   end To_Any;

   function From_Any (Item : CORBA.Any) return PortableServer.ForwardRequest_Members is
      Index : CORBA.Any;
      Result_forward_reference : CORBA.Object.Ref;
   begin
      Index := CORBA.Internals.Get_Aggregate_Element (Item,
                                            CORBA.Object.Helper.TC_Object,
                                            CORBA.Unsigned_Long ( 0));
      Result_forward_reference := CORBA.Object.Helper.From_Any (Index);
      return
         (forward_reference => Result_forward_reference);
   end From_Any;

   function To_Any
     (Item : PortableServer.ForwardRequest_Members) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_ForwardRequest);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result, CORBA.Object.Helper.To_Any (Item.forward_reference));
      return Result;
   end To_Any;

   procedure Raise_ForwardRequest_From_Any
     (Item    : PolyORB.Any.Any;
      Message : PolyORB.Std.String);
   pragma No_Return (Raise_ForwardRequest_From_Any);

   procedure Raise_ForwardRequest_From_Any
     (Item    : PolyORB.Any.Any;
      Message : PolyORB.Std.String)
   is
      Members : constant ForwardRequest_Members := From_Any (CORBA.Any (Item));
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (ForwardRequest'Identity,
         Members,
         Message);
   end Raise_ForwardRequest_From_Any;

   procedure Raise_ForwardRequest
     (Members : ForwardRequest_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (ForwardRequest'Identity,
         Members);
   end Raise_ForwardRequest;

   type Ptr_ThreadPolicyValue is access all PortableServer.ThreadPolicyValue;
   type Content_ThreadPolicyValue is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_ThreadPolicyValue;
      Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_ThreadPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class;
   procedure Set_Aggregate_Element
     (ACC    : in out Content_ThreadPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);
   function Get_Aggregate_Count
     (ACC : Content_ThreadPolicyValue) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_ThreadPolicyValue;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_ThreadPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_ThreadPolicyValue);

   function Get_Aggregate_Element
     (ACC   : not null access Content_ThreadPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC, Index);
      pragma Suppress (All_Checks);
   begin
      ACC.Repr_Cache := PortableServer.ThreadPolicyValue'Pos (ACC.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (ACC.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Content_ThreadPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use type PolyORB.Types.Unsigned_Long;
      pragma Assert (Index = 0);
   begin
      ACC.V.all := PortableServer.ThreadPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_ThreadPolicyValue) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 1;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_ThreadPolicyValue;
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
     (ACC  : Content_ThreadPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_ThreadPolicyValue then
            return null;
         end if;
         Target := Into;
         Content_ThreadPolicyValue (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_ThreadPolicyValue;
         Content_ThreadPolicyValue (Target.all).V := new PortableServer.ThreadPolicyValue'(ACC.V.all);
      end if;
      Content_ThreadPolicyValue (Target.all).Repr_Cache:= ACC.Repr_Cache;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_ThreadPolicyValue)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => PortableServer.ThreadPolicyValue, Name => Ptr_ThreadPolicyValue);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access PortableServer.ThreadPolicyValue) return PolyORB.Any.Content'Class is
   begin
      return Content_ThreadPolicyValue'(PolyORB.Any.Aggregate_Content with V => Ptr_ThreadPolicyValue (X),
        Repr_Cache => 0);
   end Wrap;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.ThreadPolicyValue is
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
      return ThreadPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (El_C)));
   end From_Any;

   function From_Any (Item : CORBA.Any) return PortableServer.ThreadPolicyValue is
   begin
      return From_Any (CORBA.Get_Container (Item).all);
   end From_Any;

   function To_Any
     (Item : PortableServer.ThreadPolicyValue) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_ThreadPolicyValue);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (ThreadPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   type Ptr_LifespanPolicyValue is access all PortableServer.LifespanPolicyValue;
   type Content_LifespanPolicyValue is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_LifespanPolicyValue;
      Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_LifespanPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class;
   procedure Set_Aggregate_Element
     (ACC    : in out Content_LifespanPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);
   function Get_Aggregate_Count
     (ACC : Content_LifespanPolicyValue) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_LifespanPolicyValue;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_LifespanPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_LifespanPolicyValue);

   function Get_Aggregate_Element
     (ACC   : not null access Content_LifespanPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC, Index);
      pragma Suppress (All_Checks);
   begin
      ACC.Repr_Cache := PortableServer.LifespanPolicyValue'Pos (ACC.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (ACC.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Content_LifespanPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use type PolyORB.Types.Unsigned_Long;
      pragma Assert (Index = 0);
   begin
      ACC.V.all := PortableServer.LifespanPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_LifespanPolicyValue) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 1;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_LifespanPolicyValue;
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
     (ACC  : Content_LifespanPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_LifespanPolicyValue then
            return null;
         end if;
         Target := Into;
         Content_LifespanPolicyValue (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_LifespanPolicyValue;
         Content_LifespanPolicyValue (Target.all).V := new PortableServer.LifespanPolicyValue'(ACC.V.all);
      end if;
      Content_LifespanPolicyValue (Target.all).Repr_Cache:= ACC.Repr_Cache;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_LifespanPolicyValue)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => PortableServer.LifespanPolicyValue, Name => Ptr_LifespanPolicyValue);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access PortableServer.LifespanPolicyValue) return PolyORB.Any.Content'Class is
   begin
      return Content_LifespanPolicyValue'(PolyORB.Any.Aggregate_Content with V => Ptr_LifespanPolicyValue (X),
        Repr_Cache => 0);
   end Wrap;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.LifespanPolicyValue is
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
      return LifespanPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (El_C)));
   end From_Any;

   function From_Any (Item : CORBA.Any) return PortableServer.LifespanPolicyValue is
   begin
      return From_Any (CORBA.Get_Container (Item).all);
   end From_Any;

   function To_Any
     (Item : PortableServer.LifespanPolicyValue) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_LifespanPolicyValue);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (LifespanPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   type Ptr_IdUniquenessPolicyValue is access all PortableServer.IdUniquenessPolicyValue;
   type Content_IdUniquenessPolicyValue is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_IdUniquenessPolicyValue;
      Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_IdUniquenessPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   procedure Set_Aggregate_Element
     (ACC    : in out Content_IdUniquenessPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);
   function Get_Aggregate_Count
     (ACC : Content_IdUniquenessPolicyValue) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_IdUniquenessPolicyValue;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_IdUniquenessPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_IdUniquenessPolicyValue);

   function Get_Aggregate_Element
     (ACC   : not null access Content_IdUniquenessPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC, Index);
      pragma Suppress (All_Checks);
   begin
      ACC.Repr_Cache := PortableServer.IdUniquenessPolicyValue'Pos (ACC.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (ACC.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Content_IdUniquenessPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use type PolyORB.Types.Unsigned_Long;
      pragma Assert (Index = 0);
   begin
      ACC.V.all := PortableServer.IdUniquenessPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_IdUniquenessPolicyValue) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 1;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_IdUniquenessPolicyValue;
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
     (ACC  : Content_IdUniquenessPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_IdUniquenessPolicyValue then
            return null;
         end if;
         Target := Into;
         Content_IdUniquenessPolicyValue (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_IdUniquenessPolicyValue;
         Content_IdUniquenessPolicyValue (Target.all).V := new PortableServer.IdUniquenessPolicyValue'(ACC.V.all);
      end if;
      Content_IdUniquenessPolicyValue (Target.all).Repr_Cache:= ACC.Repr_Cache;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_IdUniquenessPolicyValue)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => PortableServer.IdUniquenessPolicyValue, Name => Ptr_IdUniquenessPolicyValue);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access PortableServer.IdUniquenessPolicyValue) return PolyORB.Any.Content'Class is
   begin
      return Content_IdUniquenessPolicyValue'(PolyORB.Any.Aggregate_Content with V => Ptr_IdUniquenessPolicyValue (X),
        Repr_Cache => 0);
   end Wrap;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.IdUniquenessPolicyValue is
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
      return IdUniquenessPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (El_C)));
   end From_Any;

   function From_Any (Item : CORBA.Any) return PortableServer.IdUniquenessPolicyValue is
   begin
      return From_Any (CORBA.Get_Container (Item).all);
   end From_Any;

   function To_Any
     (Item : PortableServer.IdUniquenessPolicyValue) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_IdUniquenessPolicyValue);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (IdUniquenessPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   type Ptr_IdAssignmentPolicyValue is access all PortableServer.IdAssignmentPolicyValue;
   type Content_IdAssignmentPolicyValue is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_IdAssignmentPolicyValue;
      Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_IdAssignmentPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   procedure Set_Aggregate_Element
     (ACC    : in out Content_IdAssignmentPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);
   function Get_Aggregate_Count
     (ACC : Content_IdAssignmentPolicyValue) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_IdAssignmentPolicyValue;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_IdAssignmentPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_IdAssignmentPolicyValue);

   function Get_Aggregate_Element
     (ACC   : not null access Content_IdAssignmentPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC, Index);
      pragma Suppress (All_Checks);
   begin
      ACC.Repr_Cache := PortableServer.IdAssignmentPolicyValue'Pos (ACC.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (ACC.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Content_IdAssignmentPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use type PolyORB.Types.Unsigned_Long;
      pragma Assert (Index = 0);
   begin
      ACC.V.all := PortableServer.IdAssignmentPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_IdAssignmentPolicyValue) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 1;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_IdAssignmentPolicyValue;
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
     (ACC  : Content_IdAssignmentPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_IdAssignmentPolicyValue then
            return null;
         end if;
         Target := Into;
         Content_IdAssignmentPolicyValue (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_IdAssignmentPolicyValue;
         Content_IdAssignmentPolicyValue (Target.all).V := new PortableServer.IdAssignmentPolicyValue'(ACC.V.all);
      end if;
      Content_IdAssignmentPolicyValue (Target.all).Repr_Cache:= ACC.Repr_Cache;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_IdAssignmentPolicyValue)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => PortableServer.IdAssignmentPolicyValue, Name => Ptr_IdAssignmentPolicyValue);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access PortableServer.IdAssignmentPolicyValue) return PolyORB.Any.Content'Class is
   begin
      return Content_IdAssignmentPolicyValue'(PolyORB.Any.Aggregate_Content with V => Ptr_IdAssignmentPolicyValue (X),
        Repr_Cache => 0);
   end Wrap;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.IdAssignmentPolicyValue is
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
      return IdAssignmentPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (El_C)));
   end From_Any;

   function From_Any (Item : CORBA.Any) return PortableServer.IdAssignmentPolicyValue is
   begin
      return From_Any (CORBA.Get_Container (Item).all);
   end From_Any;

   function To_Any
     (Item : PortableServer.IdAssignmentPolicyValue) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_IdAssignmentPolicyValue);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (IdAssignmentPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   type Ptr_ImplicitActivationPolicyValue is access all PortableServer.ImplicitActivationPolicyValue;
   type Content_ImplicitActivationPolicyValue is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_ImplicitActivationPolicyValue;
      Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_ImplicitActivationPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   procedure Set_Aggregate_Element
     (ACC    : in out Content_ImplicitActivationPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);
   function Get_Aggregate_Count
     (ACC : Content_ImplicitActivationPolicyValue) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_ImplicitActivationPolicyValue;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_ImplicitActivationPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_ImplicitActivationPolicyValue);

   function Get_Aggregate_Element
     (ACC   : not null access Content_ImplicitActivationPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC, Index);
      pragma Suppress (All_Checks);
   begin
      ACC.Repr_Cache := PortableServer.ImplicitActivationPolicyValue'Pos (ACC.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (ACC.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Content_ImplicitActivationPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use type PolyORB.Types.Unsigned_Long;
      pragma Assert (Index = 0);
   begin
      ACC.V.all := PortableServer.ImplicitActivationPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_ImplicitActivationPolicyValue) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 1;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_ImplicitActivationPolicyValue;
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
     (ACC  : Content_ImplicitActivationPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_ImplicitActivationPolicyValue then
            return null;
         end if;
         Target := Into;
         Content_ImplicitActivationPolicyValue (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_ImplicitActivationPolicyValue;
         Content_ImplicitActivationPolicyValue (Target.all).V := new PortableServer.ImplicitActivationPolicyValue'(ACC.V.all);
      end if;
      Content_ImplicitActivationPolicyValue (Target.all).Repr_Cache:= ACC.Repr_Cache;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_ImplicitActivationPolicyValue)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => PortableServer.ImplicitActivationPolicyValue, Name => Ptr_ImplicitActivationPolicyValue);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access PortableServer.ImplicitActivationPolicyValue) return PolyORB.Any.Content'Class is
   begin
      return Content_ImplicitActivationPolicyValue'(PolyORB.Any.Aggregate_Content with V => Ptr_ImplicitActivationPolicyValue (X),
        Repr_Cache => 0);
   end Wrap;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.ImplicitActivationPolicyValue is
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
      return ImplicitActivationPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (El_C)));
   end From_Any;

   function From_Any (Item : CORBA.Any) return PortableServer.ImplicitActivationPolicyValue is
   begin
      return From_Any (CORBA.Get_Container (Item).all);
   end From_Any;

   function To_Any
     (Item : PortableServer.ImplicitActivationPolicyValue) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_ImplicitActivationPolicyValue);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (ImplicitActivationPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   type Ptr_ServantRetentionPolicyValue is access all PortableServer.ServantRetentionPolicyValue;
   type Content_ServantRetentionPolicyValue is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_ServantRetentionPolicyValue;
      Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_ServantRetentionPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   procedure Set_Aggregate_Element
     (ACC    : in out Content_ServantRetentionPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);
   function Get_Aggregate_Count
     (ACC : Content_ServantRetentionPolicyValue) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_ServantRetentionPolicyValue;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_ServantRetentionPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_ServantRetentionPolicyValue);

   function Get_Aggregate_Element
     (ACC   : not null access Content_ServantRetentionPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC, Index);
      pragma Suppress (All_Checks);
   begin
      ACC.Repr_Cache := PortableServer.ServantRetentionPolicyValue'Pos (ACC.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (ACC.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Content_ServantRetentionPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use type PolyORB.Types.Unsigned_Long;
      pragma Assert (Index = 0);
   begin
      ACC.V.all := PortableServer.ServantRetentionPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_ServantRetentionPolicyValue) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 1;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_ServantRetentionPolicyValue;
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
     (ACC  : Content_ServantRetentionPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_ServantRetentionPolicyValue then
            return null;
         end if;
         Target := Into;
         Content_ServantRetentionPolicyValue (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_ServantRetentionPolicyValue;
         Content_ServantRetentionPolicyValue (Target.all).V := new PortableServer.ServantRetentionPolicyValue'(ACC.V.all);
      end if;
      Content_ServantRetentionPolicyValue (Target.all).Repr_Cache:= ACC.Repr_Cache;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_ServantRetentionPolicyValue)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => PortableServer.ServantRetentionPolicyValue, Name => Ptr_ServantRetentionPolicyValue);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access PortableServer.ServantRetentionPolicyValue) return PolyORB.Any.Content'Class is
   begin
      return Content_ServantRetentionPolicyValue'(PolyORB.Any.Aggregate_Content with V => Ptr_ServantRetentionPolicyValue (X),
        Repr_Cache => 0);
   end Wrap;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.ServantRetentionPolicyValue is
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
      return ServantRetentionPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (El_C)));
   end From_Any;

   function From_Any (Item : CORBA.Any) return PortableServer.ServantRetentionPolicyValue is
   begin
      return From_Any (CORBA.Get_Container (Item).all);
   end From_Any;

   function To_Any
     (Item : PortableServer.ServantRetentionPolicyValue) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_ServantRetentionPolicyValue);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (ServantRetentionPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   type Ptr_RequestProcessingPolicyValue is access all PortableServer.RequestProcessingPolicyValue;
   type Content_RequestProcessingPolicyValue is
     new PolyORB.Any.Aggregate_Content with
   record
      V : Ptr_RequestProcessingPolicyValue;
      Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Get_Aggregate_Element
     (ACC   : not null access Content_RequestProcessingPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class;
   procedure Set_Aggregate_Element
     (ACC    : in out Content_RequestProcessingPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);
   function Get_Aggregate_Count
     (ACC : Content_RequestProcessingPolicyValue) return PolyORB.Types.Unsigned_Long;
   procedure Set_Aggregate_Count
     (ACC : in out Content_RequestProcessingPolicyValue;
      Count : PolyORB.Types.Unsigned_Long);
   function Clone
     (ACC  : Content_RequestProcessingPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value
     (ACC : in out Content_RequestProcessingPolicyValue);

   function Get_Aggregate_Element
     (ACC   : not null access Content_RequestProcessingPolicyValue;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC, Index);
      pragma Suppress (All_Checks);
   begin
      ACC.Repr_Cache := PortableServer.RequestProcessingPolicyValue'Pos (ACC.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (ACC.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

   procedure Set_Aggregate_Element
     (ACC    : in out Content_RequestProcessingPolicyValue;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use type PolyORB.Types.Unsigned_Long;
      pragma Assert (Index = 0);
   begin
      ACC.V.all := PortableServer.RequestProcessingPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

   function Get_Aggregate_Count
     (ACC : Content_RequestProcessingPolicyValue) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return 1;
   end Get_Aggregate_Count;

   procedure Set_Aggregate_Count
     (ACC : in out Content_RequestProcessingPolicyValue;
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
     (ACC  : Content_RequestProcessingPolicyValue;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Content_RequestProcessingPolicyValue then
            return null;
         end if;
         Target := Into;
         Content_RequestProcessingPolicyValue (Target.all).V.all := ACC.V.all;
      else
         Target := new Content_RequestProcessingPolicyValue;
         Content_RequestProcessingPolicyValue (Target.all).V := new PortableServer.RequestProcessingPolicyValue'(ACC.V.all);
      end if;
      Content_RequestProcessingPolicyValue (Target.all).Repr_Cache:= ACC.Repr_Cache;
      return Target;
   end Clone;

   procedure Finalize_Value
     (ACC : in out Content_RequestProcessingPolicyValue)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => PortableServer.RequestProcessingPolicyValue, Name => Ptr_RequestProcessingPolicyValue);
   begin
      Free (ACC.V);
   end Finalize_Value;

   function Wrap (X : access PortableServer.RequestProcessingPolicyValue) return PolyORB.Any.Content'Class is
   begin
      return Content_RequestProcessingPolicyValue'(PolyORB.Any.Aggregate_Content with V => Ptr_RequestProcessingPolicyValue (X),
        Repr_Cache => 0);
   end Wrap;

   function From_Any (C : PolyORB.Any.Any_Container'Class) return PortableServer.RequestProcessingPolicyValue is
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
      return RequestProcessingPolicyValue'Val (PolyORB.Types.Unsigned_Long'(PolyORB.Any.From_Any (El_C)));
   end From_Any;

   function From_Any (Item : CORBA.Any) return PortableServer.RequestProcessingPolicyValue is
   begin
      return From_Any (CORBA.Get_Container (Item).all);
   end From_Any;

   function To_Any
     (Item : PortableServer.RequestProcessingPolicyValue) return CORBA.Any is
      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_RequestProcessingPolicyValue);
   begin
      CORBA.Internals.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (RequestProcessingPolicyValue'Pos (Item))));
      return Result;
   end To_Any;

   procedure Deferred_Initialization is
   begin

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("POA");
         Id   : constant CORBA.String := CORBA.To_CORBA_String
                  ("IDL:omg.org/PortableServer/POA:1.0");
      begin
         TC_POA :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Object);
         CORBA.Internals.Add_Parameter (TC_POA, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_POA, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_POA);
      end;

      TC_IDL_SEQUENCE_PortableServer_POA_Forward :=
        CORBA.TypeCode.Internals.Build_Sequence_TC
          (PortableServer.POA.Helper.TC_POA, 0);
      CORBA.TypeCode.Internals.Disable_Ref_Counting
        (TC_IDL_SEQUENCE_PortableServer_POA_Forward);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("POAList");
         Id   : constant CORBA.String :=
           CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/POAList:1.0");
      begin
         TC_POAList := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => PortableServer.Helper.TC_IDL_SEQUENCE_PortableServer_POA_Forward);
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_POAList);
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ObjectId");
         Id   : constant CORBA.String :=
           CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/ObjectId:1.0");
      begin
         TC_ObjectId := CORBA.TypeCode.Internals.Build_Alias_TC
           (Name => Name, Id => Id, Parent => CORBA.IDL_SEQUENCES.Helper.TC_OctetSeq);
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_ObjectId);
      end;

      declare
         Name : constant CORBA.String :=
           CORBA.To_CORBA_String ("ForwardRequest");
         Id   : constant CORBA.String :=
           CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/ForwardRequest:1.0");
         Arg_Name_forward_reference : constant CORBA.String := CORBA.To_CORBA_String ("forward_reference");
      begin
         TC_ForwardRequest :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Except);
         CORBA.Internals.Add_Parameter (TC_ForwardRequest, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_ForwardRequest, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_ForwardRequest, CORBA.To_Any (CORBA.Object.Helper.TC_Object));
         CORBA.Internals.Add_Parameter (TC_ForwardRequest, CORBA.To_Any (Arg_Name_forward_reference));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_ForwardRequest);
      end;
      PolyORB.Exceptions.Register_Exception
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_ForwardRequest),
         Raise_ForwardRequest_From_Any'Access);
      declare
         Name : constant CORBA.String :=
           CORBA.To_CORBA_String ("ThreadPolicyValue");
         Id   : constant CORBA.String :=
           CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/ThreadPolicyValue:1.0");

         ORB_CTRL_MODEL_Name : constant CORBA.String := CORBA.To_CORBA_String ("ORB_CTRL_MODEL");
         SINGLE_THREAD_MODEL_Name : constant CORBA.String := CORBA.To_CORBA_String ("SINGLE_THREAD_MODEL");
         MAIN_THREAD_MODEL_Name : constant CORBA.String := CORBA.To_CORBA_String ("MAIN_THREAD_MODEL");
      begin
         TC_ThreadPolicyValue :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Enum);
         CORBA.Internals.Add_Parameter (TC_ThreadPolicyValue, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_ThreadPolicyValue, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_ThreadPolicyValue, CORBA.To_Any (ORB_CTRL_MODEL_Name));
         CORBA.Internals.Add_Parameter (TC_ThreadPolicyValue, CORBA.To_Any (SINGLE_THREAD_MODEL_Name));
         CORBA.Internals.Add_Parameter (TC_ThreadPolicyValue, CORBA.To_Any (MAIN_THREAD_MODEL_Name));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_ThreadPolicyValue);
      end;
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("LifespanPolicyValue");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/LifespanPolicyValue:1.0");
         TRANSIENT_Name : constant CORBA.String := CORBA.To_CORBA_String ("TRANSIENT");
         PERSISTENT_Name : constant CORBA.String := CORBA.To_CORBA_String ("PERSISTENT");
      begin
         TC_LifespanPolicyValue :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Enum);
         CORBA.Internals.Add_Parameter (TC_LifespanPolicyValue, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_LifespanPolicyValue, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_LifespanPolicyValue, CORBA.To_Any (TRANSIENT_Name));
         CORBA.Internals.Add_Parameter (TC_LifespanPolicyValue, CORBA.To_Any (PERSISTENT_Name));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_LifespanPolicyValue);
      end;
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("IdUniquenessPolicyValue");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/IdUniquenessPolicyValue:1.0");
         UNIQUE_ID_Name : constant CORBA.String := CORBA.To_CORBA_String ("UNIQUE_ID");
         MULTIPLE_ID_Name : constant CORBA.String := CORBA.To_CORBA_String ("MULTIPLE_ID");
      begin
         TC_IdUniquenessPolicyValue :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Enum);
         CORBA.Internals.Add_Parameter (TC_IdUniquenessPolicyValue, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_IdUniquenessPolicyValue, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_IdUniquenessPolicyValue, CORBA.To_Any (UNIQUE_ID_Name));
         CORBA.Internals.Add_Parameter (TC_IdUniquenessPolicyValue, CORBA.To_Any (MULTIPLE_ID_Name));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_IdUniquenessPolicyValue);
      end;
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("IdAssignmentPolicyValue");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/IdAssignmentPolicyValue:1.0");
         USER_ID_Name : constant CORBA.String := CORBA.To_CORBA_String ("USER_ID");
         SYSTEM_ID_Name : constant CORBA.String := CORBA.To_CORBA_String ("SYSTEM_ID");
      begin
         TC_IdAssignmentPolicyValue :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Enum);
         CORBA.Internals.Add_Parameter (TC_IdAssignmentPolicyValue, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_IdAssignmentPolicyValue, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_IdAssignmentPolicyValue, CORBA.To_Any (USER_ID_Name));
         CORBA.Internals.Add_Parameter (TC_IdAssignmentPolicyValue, CORBA.To_Any (SYSTEM_ID_Name));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_IdAssignmentPolicyValue);
      end;
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ImplicitActivationPolicyValue");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/ImplicitActivationPolicyValue:1.0");
         IMPLICIT_ACTIVATION_Name : constant CORBA.String := CORBA.To_CORBA_String ("IMPLICIT_ACTIVATION");
         NO_IMPLICIT_ACTIVATION_Name : constant CORBA.String := CORBA.To_CORBA_String ("NO_IMPLICIT_ACTIVATION");
      begin
         TC_ImplicitActivationPolicyValue :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Enum);
         CORBA.Internals.Add_Parameter (TC_ImplicitActivationPolicyValue, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_ImplicitActivationPolicyValue, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_ImplicitActivationPolicyValue, CORBA.To_Any (IMPLICIT_ACTIVATION_Name));
         CORBA.Internals.Add_Parameter (TC_ImplicitActivationPolicyValue, CORBA.To_Any (NO_IMPLICIT_ACTIVATION_Name));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_ImplicitActivationPolicyValue);
      end;
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ServantRetentionPolicyValue");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/ServantRetentionPolicyValue:1.0");
         RETAIN_Name : constant CORBA.String := CORBA.To_CORBA_String ("RETAIN");
         NON_RETAIN_Name : constant CORBA.String := CORBA.To_CORBA_String ("NON_RETAIN");
      begin
         TC_ServantRetentionPolicyValue :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Enum);
         CORBA.Internals.Add_Parameter (TC_ServantRetentionPolicyValue, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_ServantRetentionPolicyValue, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_ServantRetentionPolicyValue, CORBA.To_Any (RETAIN_Name));
         CORBA.Internals.Add_Parameter (TC_ServantRetentionPolicyValue, CORBA.To_Any (NON_RETAIN_Name));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_ServantRetentionPolicyValue);
      end;
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("RequestProcessingPolicyValue");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/PortableServer/RequestProcessingPolicyValue:1.0");
         USE_ACTIVE_OBJECT_MAP_ONLY_Name : constant CORBA.String := CORBA.To_CORBA_String ("USE_ACTIVE_OBJECT_MAP_ONLY");
         USE_DEFAULT_SERVANT_Name : constant CORBA.String := CORBA.To_CORBA_String ("USE_DEFAULT_SERVANT");
         USE_SERVANT_MANAGER_Name : constant CORBA.String := CORBA.To_CORBA_String ("USE_SERVANT_MANAGER");
      begin
         TC_RequestProcessingPolicyValue :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TCF_Enum);
         CORBA.Internals.Add_Parameter (TC_RequestProcessingPolicyValue, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_RequestProcessingPolicyValue, CORBA.To_Any (Id));
         CORBA.Internals.Add_Parameter (TC_RequestProcessingPolicyValue, CORBA.To_Any (USE_ACTIVE_OBJECT_MAP_ONLY_Name));
         CORBA.Internals.Add_Parameter (TC_RequestProcessingPolicyValue, CORBA.To_Any (USE_DEFAULT_SERVANT_Name));
         CORBA.Internals.Add_Parameter (TC_RequestProcessingPolicyValue, CORBA.To_Any (USE_SERVANT_MANAGER_Name));
         CORBA.TypeCode.Internals.Disable_Ref_Counting (TC_RequestProcessingPolicyValue);
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
         (Name      => +"PortableServer.Helper",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   =>
                  +"any"
                  & "PortableServer.POA.Helper"
                  & "CORBA.IDL_SEQUENCES.Helper"
                  & "corba.object"
                  & "exceptions"
          ,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;

end PortableServer.Helper;
