------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     M E S S A G I N G . H E L P E R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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

with CORBA.Helper;
with PolyORB.Initialization;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
with PolyORB.Utils.Strings;

package body Messaging.Helper is

   package IDL_Sequence_Octet_Helper is new IDL_Sequence_Octet.CORBA_Helper
     (Element_To_Any   => CORBA.To_Any,
      Element_From_Any => CORBA.From_Any);

   package IDL_Sequence_Messaging_PolicyValue_Helper is
     new IDL_Sequence_Messaging_PolicyValue.CORBA_Helper
          (Element_To_Any => To_Any, Element_From_Any => From_Any);

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any)
     return IDL_Sequence_Messaging_PolicyValue.Sequence
       renames IDL_Sequence_Messaging_PolicyValue_Helper.From_Any;

   function From_Any (Item : in CORBA.Any) return IDL_Sequence_Octet.Sequence
     renames IDL_Sequence_Octet_Helper.From_Any;

   function From_Any (Item : in CORBA.Any) return Ordering is
      Result : constant CORBA.Unsigned_Short := CORBA.From_Any (Item);
   begin
      return Messaging.Ordering (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return PolicyValue is
      Index         : CORBA.Any;
      Result_PType  : CORBA.PolicyType;
      Result_PValue : IDL_Sequence_Octet.Sequence;
   begin
      Index :=
        CORBA.Get_Aggregate_Element
         (Item,
          CORBA.Helper.TC_PolicyType,
          CORBA.Unsigned_Long (0));
      Result_PType := CORBA.Helper.From_Any (Index);
      Index :=
        CORBA.Get_Aggregate_Element
         (Item,
          TC_IDL_Sequence_Octet,
          CORBA.Unsigned_Long (1));
      Result_PValue := From_Any (Index);
      return (PType => Result_PType, PValue => Result_PValue);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return PolicyValueSeq is
      Result : constant IDL_Sequence_Messaging_PolicyValue.Sequence
        := From_Any (Item);
   begin
      return PolicyValueSeq (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return Priority is
      Result : constant CORBA.Short := CORBA.From_Any (Item);
   begin
      return Messaging.Priority (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return PriorityRange is
      Index      : CORBA.Any;
      Result_Min : Priority;
      Result_Max : Priority;
   begin
      Index :=
        CORBA.Get_Aggregate_Element
         (Item,
          TC_Priority,
          CORBA.Unsigned_Long (0));
      Result_Min := From_Any (Index);
      Index :=
        CORBA.Get_Aggregate_Element
         (Item,
          TC_Priority,
          CORBA.Unsigned_Long (1));
      Result_Max := From_Any (Index);

      return (Min => Result_Min, Max => Result_Max);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return RebindMode is
      Result : constant CORBA.Short := CORBA.From_Any (Item);
   begin
      return RebindMode (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return RoutingType is
      Result : constant CORBA.Short := CORBA.From_Any (Item);
   begin
      return Messaging.RoutingType (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return RoutingTypeRange is
      Index      : CORBA.Any;
      Result_Min : RoutingType;
      Result_Max : RoutingType;
   begin
      Index :=
        CORBA.Get_Aggregate_Element
         (Item,
          TC_RoutingType,
          CORBA.Unsigned_Long (0));
      Result_Min := From_Any (Index);
      Index :=
        CORBA.Get_Aggregate_Element
         (Item,
          TC_RoutingType,
          CORBA.Unsigned_Long (1));
      Result_Max := From_Any (Index);
      return (Min => Result_Min, Max => Result_Max);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return SyncScope is
      Result : constant CORBA.Short := CORBA.From_Any (Item);
   begin
      return Messaging.SyncScope (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : in IDL_Sequence_Messaging_PolicyValue.Sequence)
     return CORBA.Any
       renames IDL_Sequence_Messaging_PolicyValue_Helper.To_Any;

   function To_Any (Item : in IDL_Sequence_Octet.Sequence) return CORBA.Any
     renames IDL_Sequence_Octet_Helper.To_Any;

   function To_Any (Item : in Ordering) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Unsigned_Short (Item));
   begin
      CORBA.Set_Type (Result, TC_Ordering);
      return Result;
   end To_Any;

   function To_Any (Item : in PolicyValue) return CORBA.Any is
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_PolicyValue);
   begin
      CORBA.Add_Aggregate_Element (Result, CORBA.Helper.To_Any (Item.PType));
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.PValue));
      return Result;
   end To_Any;

   function To_Any (Item : in PolicyValueSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_Sequence_Messaging_PolicyValue.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_PolicyValueSeq);
      return Result;
   end To_Any;

   function To_Any (Item : in Priority) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Set_Type (Result, TC_Priority);
      return Result;
   end To_Any;

   function To_Any (Item : in PriorityRange) return CORBA.Any is
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_PriorityRange);
   begin
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Min));
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Max));
      return Result;
   end To_Any;

   function To_Any (Item : in RebindMode) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Set_Type (Result, TC_RebindMode);
      return Result;
   end To_Any;

   function To_Any (Item : in RoutingType) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Set_Type (Result, TC_RoutingType);
      return Result;
   end To_Any;

   function To_Any (Item : in RoutingTypeRange) return CORBA.Any is
      Result : CORBA.Any
        := CORBA.Get_Empty_Any_Aggregate (TC_RoutingTypeRange);
   begin
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Min));
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Max));
      return Result;
   end To_Any;

   function To_Any (Item : in SyncScope) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Set_Type (Result, TC_SyncScope);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
   begin
      TC_IDL_Sequence_Octet :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Octet, 0);
      IDL_Sequence_Octet_Helper.Initialize
        (Element_TC  => CORBA.TC_Octet,
         Sequence_TC => TC_IDL_Sequence_Octet);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("RebindMode");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (RebindMode_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RebindMode, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RebindMode, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RebindMode, CORBA.To_Any (CORBA.TC_Short));
      end;

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("SyncScope");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (SyncScope_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_SyncScope, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_SyncScope, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_SyncScope, CORBA.To_Any (CORBA.TC_Short));
      end;

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("RoutingType");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (RoutingType_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingType, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingType, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingType, CORBA.To_Any (CORBA.TC_Short));
      end;

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("Priority");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (Priority_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_Priority, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_Priority, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_Priority, CORBA.To_Any (CORBA.TC_Short));
      end;

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("Ordering");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (Ordering_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_Ordering, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_Ordering, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_Ordering, CORBA.To_Any (CORBA.TC_Unsigned_Short));
      end;

      declare
         Name         : CORBA.String
           := CORBA.To_CORBA_String ("PriorityRange");
         Id           : CORBA.String
           := CORBA.To_CORBA_String (PriorityRange_Repository_Id);
         Arg_Name_Min : CORBA.String := CORBA.To_CORBA_String ("min");
         Arg_Name_Max : CORBA.String := CORBA.To_CORBA_String ("max");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PriorityRange, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PriorityRange, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PriorityRange, CORBA.To_Any (TC_Priority));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PriorityRange, CORBA.To_Any (Arg_Name_Min));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PriorityRange, CORBA.To_Any (TC_Priority));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PriorityRange, CORBA.To_Any (Arg_Name_Max));
      end;

      declare
         Name         : CORBA.String
           := CORBA.To_CORBA_String ("RoutingTypeRange");
         Id           : CORBA.String
           := CORBA.To_CORBA_String (RoutingTypeRange_Repository_Id);
         Arg_Name_Min : CORBA.String := CORBA.To_CORBA_String ("min");
         Arg_Name_Max : CORBA.String := CORBA.To_CORBA_String ("max");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingTypeRange, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingTypeRange, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingTypeRange, CORBA.To_Any (TC_RoutingType));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingTypeRange, CORBA.To_Any (Arg_Name_Min));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingTypeRange, CORBA.To_Any (TC_RoutingType));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_RoutingTypeRange, CORBA.To_Any (Arg_Name_Max));
      end;

      declare
         Name            : CORBA.String
           := CORBA.To_CORBA_String ("PolicyValue");
         Id              : CORBA.String
           := CORBA.To_CORBA_String (PolicyValue_Repository_Id);
         Arg_Name_PType  : CORBA.String := CORBA.To_CORBA_String ("ptype");
         Arg_Name_PValue : CORBA.String := CORBA.To_CORBA_String ("pvalue");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValue, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValue, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValue, CORBA.To_Any (CORBA.Helper.TC_PolicyType));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValue, CORBA.To_Any (Arg_Name_PType));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValue, CORBA.To_Any (TC_IDL_Sequence_Octet));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValue, CORBA.To_Any (Arg_Name_PValue));
      end;

      TC_IDL_Sequence_Messaging_PolicyValue :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (TC_PolicyValue, 0);
      IDL_Sequence_Messaging_PolicyValue_Helper.Initialize
        (Element_TC  => TC_PolicyValue,
         Sequence_TC => TC_IDL_Sequence_Messaging_PolicyValue);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("PolicyValueSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (PolicyValueSeq_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValueSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValueSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_PolicyValueSeq,
           CORBA.To_Any (TC_IDL_Sequence_Messaging_PolicyValue));
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
         (Name      => +"messaging.helper",
          Conflicts => Empty,
          Depends   => +"corba.helper",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end Messaging.Helper;
