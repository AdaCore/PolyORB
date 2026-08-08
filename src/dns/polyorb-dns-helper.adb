------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . D N S . H E L P E R                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2013, Free Software Foundation, Inc.          --
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

with PolyORB.Log;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body PolyORB.DNS.Helper is
   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("polyorb.dns.helper");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;

   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;

   package body Internals is

      --------------
      -- From_Any --
      --------------

      function From_Any
        (C : PolyORB.Any.Any_Container'Class)
        return RR_Type
      is
      begin
         return RR_Type'Val
           (PolyORB.Types.Unsigned_Long'
              (PolyORB.Any.Get_Aggregate_Element
                 (C,
                  0)));
      end From_Any;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      overriding function Get_Aggregate_Element
        (Acc   : not null access Content_RR_Type;
         Tc    : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech  : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
      is
         use type PolyORB.Types.Unsigned_Long;
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         pragma Unreferenced (Tc, Index);
      begin
         Acc.Repr_Cache :=
           RR_Type'Pos
              (Acc.V.all);
         Mech.all :=
           PolyORB.Any.By_Value;
         return PolyORB.Any.Wrap
           (Acc.Repr_Cache'Unrestricted_Access);
      end Get_Aggregate_Element;

      ---------------------------
      -- Set_Aggregate_Element --
      ---------------------------

      overriding procedure Set_Aggregate_Element
        (Acc    : in out Content_RR_Type;
         Tc     : PolyORB.Any.TypeCode.Object_Ptr;
         Index  : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class)
      is
         use type PolyORB.Types.Unsigned_Long;
         pragma Assert ((Index = 0));
         pragma Unreferenced (Tc);
      begin
         Acc.V.all :=
           RR_Type'Val
              (PolyORB.Types.Unsigned_Long'
                 (PolyORB.Any.From_Any
                    (From_C)));
      end Set_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      overriding function Get_Aggregate_Count
        (Acc : Content_RR_Type) return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 1;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      overriding procedure Set_Aggregate_Count
        (Acc   : in out Content_RR_Type;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      overriding function Unchecked_Get_V
        (Acc : not null access Content_RR_Type)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_RR_Type,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      overriding function Clone
        (Acc  : Content_RR_Type;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
      begin
         if Into /= null then
            if Into.all not in Content_RR_Type then
               return null;
            end if;
            Target :=
              Into;
            Content_RR_Type
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_RR_Type;
            Content_RR_Type
              (Target.all).V :=
              new RR_Type'
                 (Acc.V.all);
         end if;
         Content_RR_Type
           (Target.all).Repr_Cache :=
           Acc.Repr_Cache;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      overriding procedure Finalize_Value (Acc : in out Content_RR_Type) is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => RR_Type,

            Name   => Ptr_RR_Type);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access RR_Type) return PolyORB.Any.Content'Class
      is
      begin
         return Content_RR_Type'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_RR_Type
              (X),
            Repr_Cache => 0);
      end Wrap;

      RR_Type_Initialized : PolyORB.Types.Boolean := False;

      ------------------------
      -- Initialize_RR_Type --
      ------------------------

      procedure Initialize_RR_Type is
         Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("RR_Type");
         Id : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("IDL:DNS/RR_Type:1.0");
         A_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("A");
         NS_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("NS");
         SOA_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("SOA");
         CNAME_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("CNAME");
         PTR_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("PTR");
         TXT_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("TXT");
         SRV_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("SRV");
      begin
         if not RR_Type_Initialized then
            RR_Type_Initialized :=
              True;
            TC_RR_Type :=
              PolyORB.Any.TypeCode.TCF_Enum;

            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (Id));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (A_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (NS_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (SOA_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (CNAME_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (PTR_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (TXT_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Type,
               PolyORB.Any.To_Any
                 (SRV_Name));
            Any.TypeCode.Disable_Ref_Counting
             (Any.TypeCode.Object_Of (TC_RR_Type).all);

         end if;
      end Initialize_RR_Type;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_SRV_Data;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
      is
         use type PolyORB.Types.Unsigned_Long;
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         pragma Unreferenced (Tc);
      begin
         Mech.all :=
           PolyORB.Any.By_Reference;
         case Index is
            when 0 =>
               return PolyORB.Any.Wrap
                 (Acc.V.priority'Unrestricted_Access);
            when 1 =>
               return PolyORB.Any.Wrap
                 (Acc.V.weight'Unrestricted_Access);
            when 2 =>
               return PolyORB.Any.Wrap
                 (Acc.V.port'Unrestricted_Access);
            when 3 =>
               return PolyORB.Any.Wrap
                 (Acc.V.target'Unrestricted_Access);
            pragma Warnings (Off);
            when others =>
               raise Constraint_Error;
            pragma Warnings (On);

         end case;
      end Get_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      overriding function Get_Aggregate_Count
        (Acc : Content_SRV_Data)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 4;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_SRV_Data;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      overriding function Unchecked_Get_V
        (Acc : not null access Content_SRV_Data)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_SRV_Data,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      overriding function Clone
        (Acc : Content_SRV_Data;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
      begin
         if Into /= null then
            if Into.all not in Content_SRV_Data then
               return null;
            end if;
            Target :=
              Into;
            Content_SRV_Data
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_SRV_Data;
            Content_SRV_Data
              (Target.all).V :=
              new SRV_Data'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      overriding procedure Finalize_Value
        (Acc : in out Content_SRV_Data)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => SRV_Data,

            Name   => Ptr_SRV_Data);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access SRV_Data)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_SRV_Data'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_SRV_Data
              (X));
      end Wrap;

      SRV_Data_Initialized : PolyORB.Types.Boolean := False;

      -------------------------
      -- Initialize_SRV_Data --
      -------------------------

      procedure Initialize_SRV_Data is
         Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("SRV_Data");
         Id : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("IDL:DNS/SRV_Data:1.0");
         Argument_Name_priority : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("priority");
         Argument_Name_weight : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("weight");
         Argument_Name_port : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("port");
         Argument_Name_target : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("target");
      begin
         if not SRV_Data_Initialized then
            SRV_Data_Initialized :=
              True;
            Helper.TC_SRV_Data := PolyORB.Any.TypeCode.TCF_Struct;
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (Id));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (PolyORB.Any.TC_Unsigned_Short));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (Argument_Name_priority));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (PolyORB.Any.TC_Unsigned_Short));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (Argument_Name_weight));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (PolyORB.Any.TC_Unsigned_Short));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (Argument_Name_port));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (PolyORB.Any.TC_String));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_SRV_Data,
               PolyORB.Any.To_Any
                 (Argument_Name_target));
            Any.TypeCode.Disable_Ref_Counting
             (Any.TypeCode.Object_Of (TC_SRV_Data).all);

         end if;
      end Initialize_SRV_Data;

      IDL_SEQUENCE_4_octet_Initialized : PolyORB.Types.Boolean :=
        False;
      ---------------------------------------
      -- IDL_SEQUENCE_4_octet_Element_Wrap --
      ---------------------------------------

      function IDL_SEQUENCE_4_octet_Element_Wrap
        (X : access PolyORB.Types.Octet)
        return PolyORB.Any.Content'Class
      is
      begin
         return PolyORB.Any.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_4_octet_Element_Wrap;

      function Wrap
        (X : access IDL_SEQUENCE_4_octet.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_4_octet_Helper.Wrap;
      -------------------------------------
      -- Initialize_IDL_SEQUENCE_4_octet --
      -------------------------------------

      procedure Initialize_IDL_SEQUENCE_4_octet is
      begin
         if not IDL_SEQUENCE_4_octet_Initialized then
            IDL_SEQUENCE_4_octet_Initialized :=
              True;
            Helper.TC_IDL_SEQUENCE_4_octet :=
              PolyORB.Any.TypeCode.Build_Sequence_TC
                 (PolyORB.Any.TC_Octet,
                  4);
            Any.TypeCode.Disable_Ref_Counting
              (Any.TypeCode.Object_Of (TC_IDL_SEQUENCE_4_octet).all);
            IDL_SEQUENCE_4_octet_Helper.Initialize
              (Element_TC => PolyORB.Any.TC_Octet,
               Sequence_TC => Helper.TC_IDL_SEQUENCE_4_octet);
         end if;
      end Initialize_IDL_SEQUENCE_4_octet;

      IDL_AT_Sequence_4_octet_Initialized : PolyORB.Types.Boolean := False;

      ----------------------------------------
      -- Initialize_IDL_AT_Sequence_4_octet --
      ----------------------------------------

      procedure Initialize_IDL_AT_Sequence_4_octet is
         Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("IDL_AT_Sequence_4_octet");
         Id : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("IDL:DNS/IDL_AT_Sequence_4_octet:1.0");
      begin
         if not IDL_AT_Sequence_4_octet_Initialized then
            IDL_AT_Sequence_4_octet_Initialized :=
              True;
            Helper.Internals.Initialize_IDL_SEQUENCE_4_octet;
            TC_IDL_AT_Sequence_4_octet := PolyORB.Any.TypeCode.TCF_Alias;
            Any.TypeCode.Add_Parameter
              (TC_IDL_AT_Sequence_4_octet, Any.To_Any (Name));
            Any.TypeCode.Add_Parameter (TC_IDL_AT_Sequence_4_octet,
                                        Any.To_Any (Id));
            Any.TypeCode.Add_Parameter
              (TC_IDL_AT_Sequence_4_octet, Any.To_Any
                 (TC_IDL_SEQUENCE_4_octet));
            Any.TypeCode.Disable_Ref_Counting
              (Any.TypeCode.Object_Of (TC_IDL_AT_Sequence_4_octet).all);
         end if;
      end Initialize_IDL_AT_Sequence_4_octet;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_RR_Data;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
      is
         use type PolyORB.Types.Unsigned_Long;
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         pragma Unreferenced (Tc);
      begin
         if Index = 0 then
            Mech.all :=
              PolyORB.Any.By_Value;
            Acc.Switch_Cache :=
              Acc.V.Switch;
            return Helper.Internals.Wrap
              (Acc.Switch_Cache'Unrestricted_Access);
         else
            pragma Assert ((Index
               = 1));
            Mech.all :=
              PolyORB.Any.By_Reference;
            case Acc.V.Switch is
               when SRV =>
                  return Helper.Internals.Wrap
                    (Acc.V.srv_data'Unrestricted_Access);
               when A =>
                  return Helper.Internals.Wrap
                    (IDL_SEQUENCE_4_octet.Sequence
                       (Acc.V.a_address)'Unrestricted_Access);
               pragma Warnings (Off);
               when others =>
                  return PolyORB.Any.Wrap
                    (Acc.V.rr_answer'Unrestricted_Access);
               pragma Warnings (On);

            end case;
         end if;
      end Get_Aggregate_Element;

      ---------------------------
      -- Set_Aggregate_Element --
      ---------------------------

      overriding procedure Set_Aggregate_Element
        (Acc : in out Content_RR_Data;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class)
      is
         use type PolyORB.Types.Unsigned_Long;
         pragma Assert ((Index
            = 0));
         New_Switch : constant RR_Type :=
           Helper.Internals.From_Any
              (From_C);
         New_Union : RR_Data
           (Switch => New_Switch);
         --  Use default initialization
         pragma Warnings (Off, New_Union);
         pragma Suppress (Discriminant_Check);
         pragma Unreferenced (Tc);
      begin
         Acc.V.all :=
           New_Union;
      end Set_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      overriding function Get_Aggregate_Count
        (Acc : Content_RR_Data)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 2;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_RR_Data;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      overriding function Unchecked_Get_V
        (Acc : not null access Content_RR_Data)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_RR_Data,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      overriding function Clone
        (Acc : Content_RR_Data;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
         pragma Suppress (Discriminant_Check);
      begin
         if Into /= null then
            if Into.all not in Content_RR_Data then
               return null;
            end if;
            Target :=
              Into;
            Content_RR_Data
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_RR_Data;
            Content_RR_Data
              (Target.all).V :=
              new RR_Data'
                 (Acc.V.all);
         end if;
         Content_RR_Data
           (Target.all).Switch_Cache :=
           Acc.Switch_Cache;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      overriding procedure Finalize_Value
        (Acc : in out Content_RR_Data)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => RR_Data,

            Name   => Ptr_RR_Data);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access RR_Data)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_RR_Data'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_RR_Data
              (X),
            Switch_Cache => X.Switch);
      end Wrap;

      RR_Data_Initialized : PolyORB.Types.Boolean := False;

      ------------------------
      -- Initialize_RR_Data --
      ------------------------

      procedure Initialize_RR_Data is
         Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("RR_Data");
         Id : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("IDL:DNS/RR_Data:1.0");
         Argument_Name_srv_data : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("srv_data");
         Argument_Name_a_address : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("a_address");
         Argument_Name_rr_answer : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("rr_answer");
      begin
         if not RR_Data_Initialized then
            RR_Data_Initialized :=
              True;
            Helper.TC_RR_Data := PolyORB.Any.TypeCode.TCF_Union;
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (Id));
            Helper.Internals.Initialize_RR_Type;
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (Helper.TC_RR_Type));
            Helper.Internals.Initialize_SRV_Data;
            Helper.Internals.Initialize_IDL_AT_Sequence_4_octet;
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (PolyORB.Types.Long
                    (2)));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               Helper.To_Any
                 (RR_Type'
                    (SRV)));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (Helper.TC_SRV_Data));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (Argument_Name_srv_data));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               Helper.To_Any
                 (RR_Type'
                    (A)));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (Helper.TC_IDL_AT_Sequence_4_octet));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (Argument_Name_a_address));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               Helper.To_Any
                 (RR_Type'First));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (PolyORB.Any.TC_String));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR_Data,
               PolyORB.Any.To_Any
                 (Argument_Name_rr_answer));
            Any.TypeCode.Disable_Ref_Counting
              (Any.TypeCode.Object_Of (TC_RR_Data).all);
         end if;
      end Initialize_RR_Data;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_RR;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
      is
         use type PolyORB.Types.Unsigned_Long;
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         pragma Unreferenced (Tc);
      begin
         Mech.all :=
           PolyORB.Any.By_Reference;
         case Index is
            when 0 =>
               return PolyORB.Any.Wrap
                 (Acc.V.rr_name'Unrestricted_Access);
            when 1 =>
               return Helper.Internals.Wrap
                 (Acc.V.rr_type'Unrestricted_Access);
            when 2 =>
               return PolyORB.Any.Wrap
                 (Acc.V.TTL'Unrestricted_Access);
            when 3 =>
               return PolyORB.Any.Wrap
                 (Acc.V.data_length'Unrestricted_Access);
            when 4 =>
               return Helper.Internals.Wrap
                 (Acc.V.rr_data'Unrestricted_Access);
            pragma Warnings (Off);
            when others =>
               raise Constraint_Error;
            pragma Warnings (On);

         end case;
      end Get_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      overriding function Get_Aggregate_Count
        (Acc : Content_RR)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 5;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_RR;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      overriding function Unchecked_Get_V
        (Acc : not null access Content_RR)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_RR,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      overriding function Clone
        (Acc : Content_RR;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
      begin
         if Into /= null then
            if Into.all not in Content_RR then
               return null;
            end if;
            Target :=
              Into;
            Content_RR
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_RR;
            Content_RR
              (Target.all).V :=
              new RR'
                 (Acc.V.all);
         end if;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      overriding procedure Finalize_Value
        (Acc : in out Content_RR)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => RR,

            Name   => Ptr_RR);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access RR)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_RR'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_RR
              (X));
      end Wrap;

      RR_Initialized : PolyORB.Types.Boolean := False;

      -------------------
      -- Initialize_RR --
      -------------------

      procedure Initialize_RR is
         Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("RR");
         Id : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("IDL:DNS/RR:1.0");
         Argument_Name_rr_name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("rr_name");
         Argument_Name_rr_type : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("rr_type");
         Argument_Name_TTL : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("TTL");
         Argument_Name_data_length : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("data_length");
         Argument_Name_rr_data : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("rr_data");
      begin
         if not RR_Initialized then
            RR_Initialized :=
              True;
            Helper.TC_RR := PolyORB.Any.TypeCode.TCF_Struct;
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Id));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (PolyORB.Any.TC_String));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Argument_Name_rr_name));
            Helper.Internals.Initialize_RR_Type;
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Helper.TC_RR_Type));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Argument_Name_rr_type));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (PolyORB.Any.TC_Unsigned_Long));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Argument_Name_TTL));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (PolyORB.Any.TC_Unsigned_Short));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Argument_Name_data_length));
            Helper.Internals.Initialize_RR_Data;
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Helper.TC_RR_Data));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_RR,
               PolyORB.Any.To_Any
                 (Argument_Name_rr_data));
            Any.TypeCode.Disable_Ref_Counting
              (Any.TypeCode.Object_Of (TC_RR).all);
         end if;
      end Initialize_RR;

      --------------
      -- From_Any --
      --------------

      function From_Any
        (C : PolyORB.Any.Any_Container'Class)
        return Rcode
      is
      begin
         return Rcode'Val
           (PolyORB.Types.Unsigned_Long'
              (PolyORB.Any.Get_Aggregate_Element
                 (C,
                  0)));
      end From_Any;

      ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_Rcode;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class
      is
         use type PolyORB.Types.Unsigned_Long;
         use type PolyORB.Any.Mechanism;
         pragma Suppress (Validity_Check);
         pragma Unreferenced (Tc, Index);
      begin
         Acc.Repr_Cache :=
           Rcode'Pos
              (Acc.V.all);
         Mech.all :=
           PolyORB.Any.By_Value;
         return PolyORB.Any.Wrap
           (Acc.Repr_Cache'Unrestricted_Access);
      end Get_Aggregate_Element;

      ---------------------------
      -- Set_Aggregate_Element --
      ---------------------------

      overriding procedure Set_Aggregate_Element
        (Acc : in out Content_Rcode;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class)
      is
         use type PolyORB.Types.Unsigned_Long;
         pragma Assert ((Index
            = 0));
         pragma Unreferenced (Tc);
      begin
         Acc.V.all :=
           Rcode'Val
              (PolyORB.Types.Unsigned_Long'
                 (PolyORB.Any.From_Any
                    (From_C)));
      end Set_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

      overriding function Get_Aggregate_Count
        (Acc : Content_Rcode)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
      begin
         return 1;
      end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_Rcode;
         Count : PolyORB.Types.Unsigned_Long)
      is
      begin
         null;
      end Set_Aggregate_Count;

      ---------------------
      -- Unchecked_Get_V --
      ---------------------

      overriding function Unchecked_Get_V
        (Acc : not null access Content_Rcode)
        return PolyORB.Types.Address
      is
         function To_Address
           is new Ada.Unchecked_Conversion
              (Ptr_Rcode,
               PolyORB.Types.Address);

      begin
         return To_Address
           (Acc.V);
      end Unchecked_Get_V;

      -----------
      -- Clone --
      -----------

      overriding function Clone
        (Acc : Content_Rcode;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr
      is
         use type PolyORB.Any.Content_Ptr;
         Target : PolyORB.Any.Content_Ptr;
      begin
         if Into /= null then
            if Into.all not in Content_Rcode then
               return null;
            end if;
            Target :=
              Into;
            Content_Rcode
              (Target.all).V.all :=
              Acc.V.all;
         else
            Target :=
              new Content_Rcode;
            Content_Rcode
              (Target.all).V :=
              new Rcode'
                 (Acc.V.all);
         end if;
         Content_Rcode
           (Target.all).Repr_Cache :=
           Acc.Repr_Cache;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

      overriding procedure Finalize_Value
        (Acc : in out Content_Rcode)
      is
         procedure Free
           is new PolyORB.Utils.Unchecked_Deallocation.Free

           (Object => Rcode,

            Name   => Ptr_Rcode);

      begin
         Free
           (Acc.V);
      end Finalize_Value;

      ----------
      -- Wrap --
      ----------

      function Wrap
        (X : access Rcode)
        return PolyORB.Any.Content'Class
      is
      begin
         return Content_Rcode'
           (PolyORB.Any.Aggregate_Content with
            V => Ptr_Rcode
              (X),
            Repr_Cache => 0);
      end Wrap;

      Rcode_Initialized : PolyORB.Types.Boolean := False;

      ----------------------
      -- Initialize_Rcode --
      ----------------------

      procedure Initialize_Rcode is
         Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("Rcode");
         Id : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("IDL:DNS/Rcode:1.0");
         No_Error_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("No_Error");
         Format_Error_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("Format_Error");
         Server_Failure_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("Server_Failure");
         Name_Error_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("Name_Error");
         Not_Implemented_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("Not_Implemented");
         Refused_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("Refused");
         YX_Domain_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("YX_Domain");
         YX_RRSet_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("YX_RRSet");
         NX_RRSet_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("NX_RRSet");
         Not_Auth_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("Not_Auth");
         Not_Zone_Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("Not_Zone");
      begin
         if not Rcode_Initialized then
            Rcode_Initialized :=
              True;
            Helper.TC_Rcode := PolyORB.Any.TypeCode.TCF_Enum;
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Id));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (No_Error_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Format_Error_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Server_Failure_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Name_Error_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Not_Implemented_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Refused_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (YX_Domain_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (YX_RRSet_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (NX_RRSet_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Not_Auth_Name));
            PolyORB.Any.TypeCode.Add_Parameter
              (TC_Rcode,
               PolyORB.Any.To_Any
                 (Not_Zone_Name));
            Any.TypeCode.Disable_Ref_Counting
              (Any.TypeCode.Object_Of (TC_Rcode).all);
         end if;
      end Initialize_Rcode;

      --------------------------------------
      -- IDL_SEQUENCE_DNS_RR_Element_Wrap --
      --------------------------------------

      function IDL_SEQUENCE_DNS_RR_Element_Wrap
        (X : access RR)
        return PolyORB.Any.Content'Class
      is
      begin
         return Helper.Internals.Wrap
           (X.all'Unrestricted_Access);
      end IDL_SEQUENCE_DNS_RR_Element_Wrap;

      function Wrap
        (X : access IDL_SEQUENCE_DNS_RR.Sequence)
        return PolyORB.Any.Content'Class
        renames IDL_SEQUENCE_DNS_RR_Helper.Wrap;

      IDL_SEQUENCE_DNS_RR_Initialized : PolyORB.Types.Boolean := False;

      ------------------------------------
      -- Initialize_IDL_SEQUENCE_DNS_RR --
      ------------------------------------

      procedure Initialize_IDL_SEQUENCE_DNS_RR is
      begin
         if not IDL_SEQUENCE_DNS_RR_Initialized then
            IDL_SEQUENCE_DNS_RR_Initialized :=
              True;
            Helper.Internals.Initialize_RR;
            Helper.TC_IDL_SEQUENCE_DNS_RR :=
              PolyORB.Any.TypeCode.Build_Sequence_TC
                 (Helper.TC_RR,
                  0);
            Any.TypeCode.Disable_Ref_Counting
              (Any.TypeCode.Object_Of (TC_IDL_SEQUENCE_DNS_RR).all);
            IDL_SEQUENCE_DNS_RR_Helper.Initialize
              (Element_TC => Helper.TC_RR,
               Sequence_TC => Helper.TC_IDL_SEQUENCE_DNS_RR);
         end if;
      end Initialize_IDL_SEQUENCE_DNS_RR;

      rrSequence_Initialized : PolyORB.Types.Boolean := False;

      ---------------------------
      -- Initialize_rrSequence --
      ---------------------------

      procedure Initialize_rrSequence is
         Name : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("rrSequence");
         Id : constant PolyORB.Types.String :=
           PolyORB.Types.To_PolyORB_String
              ("IDL:DNS/rrSequence:1.0");
      begin
         if not rrSequence_Initialized then
            rrSequence_Initialized :=
              True;
            Helper.Internals.Initialize_IDL_SEQUENCE_DNS_RR;
            TC_rrSequence := PolyORB.Any.TypeCode.TCF_Alias;
            Any.TypeCode.Add_Parameter
              (TC_rrSequence, Any.To_Any (Name));
            Any.TypeCode.Add_Parameter (TC_rrSequence, Any.To_Any (Id));
            Any.TypeCode.Add_Parameter (TC_rrSequence, Any.To_Any
                                  (TC_IDL_SEQUENCE_DNS_RR));
            Any.TypeCode.Disable_Ref_Counting
              (Any.TypeCode.Object_Of (TC_rrSequence).all);
         end if;
      end Initialize_rrSequence;

   end Internals;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return RR_Type
   is
   begin
      return Helper.Internals.From_Any
        (PolyORB.Any.Get_Container
           (Item).all);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : RR_Type)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        PolyORB.Any.Get_Empty_Any_Aggregate
           (TC_RR_Type);
   begin
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (PolyORB.Types.Unsigned_Long
              (RR_Type'Pos
                 (Item))));
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return SRV_Data
   is
   begin
      return (priority => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TC_Unsigned_Short,
            0)),
      weight => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TC_Unsigned_Short,
            1)),
      port => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TC_Unsigned_Short,
            2)),
      target => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TC_String,
            3)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : SRV_Data)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        PolyORB.Any.Get_Empty_Any_Aggregate
           (TC_SRV_Data);
   begin
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.priority));
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.weight));
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.port));
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.target));
      return Result;
   end To_Any;

   function From_Any
     (Item : PolyORB.Any.Any)
     return IDL_SEQUENCE_4_octet.Sequence
     renames Helper.Internals.IDL_SEQUENCE_4_octet_Helper.From_Any;

   function To_Any
     (Item : IDL_SEQUENCE_4_octet.Sequence)
     return PolyORB.Any.Any
     renames Helper.Internals.IDL_SEQUENCE_4_octet_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return IDL_AT_Sequence_4_octet
   is
      Result : constant IDL_SEQUENCE_4_octet.Sequence :=
        Helper.From_Any
           (Item);
   begin
      return IDL_AT_Sequence_4_octet
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : IDL_AT_Sequence_4_octet)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        To_Any
           (IDL_SEQUENCE_4_octet.Sequence
              (Item));
   begin
      PolyORB.Any.Set_Type
        (Result,
         TC_IDL_AT_Sequence_4_octet);
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return RR_Data
   is
      Label_Any : constant PolyORB.Any.Any :=
        PolyORB.Any.Get_Aggregate_Element
           (Item,
            Helper.TC_RR_Type,
            PolyORB.Types.Unsigned_Long
              (0));
      Label : constant RR_Type :=
        Helper.From_Any
           (Label_Any);
      Result : RR_Data
        (Label);
      Index : PolyORB.Any.Any;
   begin
      case Label is
         when SRV =>
            Index :=
              PolyORB.Any.Get_Aggregate_Element
                 (Item,
                  Helper.TC_SRV_Data,
                  PolyORB.Types.Unsigned_Long
                    (1));
            Result.srv_data :=
              Helper.From_Any
                 (Index);
         when A =>
            Index :=
              PolyORB.Any.Get_Aggregate_Element
                 (Item,
                  Helper.TC_IDL_AT_Sequence_4_octet,
                  PolyORB.Types.Unsigned_Long
                    (1));
            Result.a_address :=
              Helper.From_Any
                 (Index);
         pragma Warnings (Off);
         when others =>
            Index :=
              PolyORB.Any.Get_Aggregate_Element
                 (Item,
                  PolyORB.Any.TC_String,
                  PolyORB.Types.Unsigned_Long
                    (1));
            Result.rr_answer :=
              PolyORB.Any.From_Any
                 (Index);
         pragma Warnings (On);

      end case;
      return Result;
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : RR_Data)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        PolyORB.Any.Get_Empty_Any_Aggregate
           (Helper.TC_RR_Data);
   begin
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         Helper.To_Any
           (Item.Switch));
      case Item.Switch is
         when SRV =>
            PolyORB.Any.Add_Aggregate_Element
              (Result,
               Helper.To_Any
                 (Item.srv_data));
         when A =>
            PolyORB.Any.Add_Aggregate_Element
              (Result,
               Helper.To_Any
                 (Item.a_address));
         pragma Warnings (Off);
         when others =>
            PolyORB.Any.Add_Aggregate_Element
              (Result,
               PolyORB.Any.To_Any
                 (Item.rr_answer));
         pragma Warnings (On);

      end case;
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return RR
   is
   begin
      return (rr_name => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TC_String,
            0)),
      rr_type => Helper.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            Helper.TC_RR_Type,
            1)),
      TTL => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TC_Unsigned_Long,
            2)),
      data_length => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TC_Unsigned_Short,
            3)),
      rr_data => Helper.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            Helper.TC_RR_Data,
            4)));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : RR)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        PolyORB.Any.Get_Empty_Any_Aggregate
           (TC_RR);
   begin
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.rr_name));
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         Helper.To_Any
           (Item.rr_type));
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.TTL));
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.data_length));
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         Helper.To_Any
           (Item.rr_data));
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return Rcode
   is
   begin
      return Helper.Internals.From_Any
        (PolyORB.Any.Get_Container
           (Item).all);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : Rcode)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        PolyORB.Any.Get_Empty_Any_Aggregate
           (TC_Rcode);
   begin
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (PolyORB.Types.Unsigned_Long
              (Rcode'Pos
                 (Item))));
      return Result;
   end To_Any;

   function From_Any
     (Item : PolyORB.Any.Any)
     return IDL_SEQUENCE_DNS_RR.Sequence
     renames Helper.Internals.IDL_SEQUENCE_DNS_RR_Helper.From_Any;

   function To_Any
     (Item : IDL_SEQUENCE_DNS_RR.Sequence)
     return PolyORB.Any.Any
     renames Helper.Internals.IDL_SEQUENCE_DNS_RR_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return rrSequence
   is
      Result : constant IDL_SEQUENCE_DNS_RR.Sequence :=
        Helper.From_Any
           (Item);
   begin
      return rrSequence
        (Result);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : rrSequence)
     return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        Helper.To_Any
           (IDL_SEQUENCE_DNS_RR.Sequence
              (Item));
   begin
      PolyORB.Any.Set_Type
        (Result,
         TC_rrSequence);
      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------
   procedure Deferred_Initialization;
   procedure Deferred_Initialization is
   begin
      DNS.Helper.Internals.Initialize_RR_Type;
      DNS.Helper.Internals.Initialize_SRV_Data;
      DNS.Helper.Internals.Initialize_IDL_SEQUENCE_4_octet;
      DNS.Helper.Internals.Initialize_IDL_AT_Sequence_4_octet;
      DNS.Helper.Internals.Initialize_RR_Data;
      DNS.Helper.Internals.Initialize_RR;
      DNS.Helper.Internals.Initialize_Rcode;
      DNS.Helper.Internals.Initialize_IDL_SEQUENCE_DNS_RR;
      DNS.Helper.Internals.Initialize_rrSequence;
   end Deferred_Initialization;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;
begin
   pragma Debug (C, O ("Registering Module DNS-Helper"));
   Register_Module
     (Module_Info'
      (Name      => +"dns.helper",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      =>  Deferred_Initialization'Access,
       Shutdown  => null));

end PolyORB.DNS.Helper;
