------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.DSA_P.NAME_SERVICE.MDNS.HELPER                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Types;
with PolyORB.Sequences.Unbounded;
with PolyORB.Sequences.Unbounded.Helper;
with PolyORB.Sequences.Bounded;
with PolyORB.Sequences.Bounded.Helper;

pragma Elaborate_All (PolyORB.Sequences.Unbounded.Helper,
                           PolyORB.Sequences.Bounded.Helper);
with PolyORB.Any;

package PolyORB.DSA_P.Name_Service.mDNS.Helper is

   --  Rcode type definition
   type Rcode is
     (No_Error,
      Format_Error,
      Server_Failure,
      Name_Error,
      Not_Implemented,
      Refused,
      YX_Domain,
      YX_RRSet,
      NX_RRSet,
      Not_Auth,
      Not_Zone);

   --  Rcode constant code values ,
   --  defined by IANA, ref: [RFC 5395] [RFC 1035]
   No_Error_Code : constant Types.Unsigned_Short := 0;
   Format_Error_Code : constant Types.Unsigned_Short := 1;
   Server_Failure_Code : constant Types.Unsigned_Short := 2;
   Name_Error_Code : constant Types.Unsigned_Short := 3;
   Not_Implemented_Code : constant Types.Unsigned_Short := 4;
   Refused_Code : constant Types.Unsigned_Short := 5;
   YX_Domain_Code : constant Types.Unsigned_Short := 6;
   YX_RRSet_Code : constant Types.Unsigned_Short := 7;
   NX_RRSet_Code : constant Types.Unsigned_Short := 8;
   Not_Auth_Code : constant Types.Unsigned_Short := 9;
   Not_Zone_Code : constant Types.Unsigned_Short := 10;

   type Opcode_Type is
     (Query,
      IQuery,
      Status
     );
   --  Opcode operation name definition
   Query_Name : constant Standard.String := "Query";
   IQuery_Name : constant Standard.String := "IQuery";
   Status_Name : constant Standard.String := "Status";

   type RR_Type is
     (A,
      NS,
      SOA,
      CNAME,
      PTR,
      TXT,
      SRV);

   --  Resource Record (RR) TYPEs constant code values
   --  ,defined by IANA, ref: [RFC 5395] [RFC 1035]

   A_Code : constant Types.Unsigned_Short := 1;
   NS_Code : constant Types.Unsigned_Short := 2;
   SOA_Code : constant Types.Unsigned_Short := 6;
   CNAME_Code : constant Types.Unsigned_Short := 5;
   PTR_Code : constant Types.Unsigned_Short := 12;
   TXT_Code : constant Types.Unsigned_Short := 16;
   SRV_Code : constant Types.Unsigned_Short := 33;

   Default_Class_Code : constant Types.Unsigned_Short := 1;

   Arg_Name_Auth : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("authoritative");
   Arg_Name_Question : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("question");
   Arg_Name_Answer : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("answer");
   Arg_Name_Au : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("authority");
   Arg_Name_Add : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("additional");

   type SRV_Data is
     record
         priority : PolyORB.Types.Unsigned_Short;
         weight : PolyORB.Types.Unsigned_Short;
         port : PolyORB.Types.Unsigned_Short;
         target : PolyORB.Types.String;
   end record;

   package IDL_SEQUENCE_4_octet is
     new PolyORB.Sequences.Bounded
        (PolyORB.Types.Octet,
         4);

   type IDL_AT_Sequence_4_octet is
     new IDL_SEQUENCE_4_octet.Sequence;
   type RR_Data
     (Switch : RR_Type := RR_Type'First)
   is
     record
         case Switch is
            when SRV =>
               srv_data :
                PolyORB.DSA_P.Name_Service.mDNS.Helper.SRV_Data;
            when A =>
               a_address : IDL_AT_Sequence_4_octet;
            when others =>
               rr_answer : PolyORB.Types.String;
         end case;
   end record;
   type RR is
     record
         rr_name : PolyORB.Types.String;
         rr_type :
         PolyORB.DSA_P.Name_Service.mDNS.Helper.RR_Type;
         TTL : PolyORB.Types.Unsigned_Long;
         data_length : PolyORB.Types.Unsigned_Short;
         rr_data :
         PolyORB.DSA_P.Name_Service.mDNS.Helper.RR_Data;
   end record;

   package IDL_SEQUENCE_DNS_RR is
     new PolyORB.Sequences.Unbounded
        (RR);

   type rrSequence is
     new IDL_SEQUENCE_DNS_RR.Sequence;

   TC_RR_Type : PolyORB.Any.TypeCode.Local_Ref;
   function From_Any
     (Item : PolyORB.Any.Any)
     return RR_Type;

   function To_Any
     (Item : RR_Type)
     return PolyORB.Any.Any;

   TC_SRV_Data : PolyORB.Any.TypeCode.Local_Ref;

   function From_Any
     (Item : PolyORB.Any.Any)
     return SRV_Data;

   function To_Any
     (Item : SRV_Data)
     return PolyORB.Any.Any;

   TC_IDL_SEQUENCE_4_octet : PolyORB.Any.TypeCode.Local_Ref;

   function From_Any
     (Item : PolyORB.Any.Any)
     return IDL_SEQUENCE_4_octet.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_4_octet.Sequence)
     return PolyORB.Any.Any;

   TC_IDL_AT_Sequence_4_octet : PolyORB.Any.TypeCode.Local_Ref;

   function From_Any
     (Item : PolyORB.Any.Any)
     return IDL_AT_Sequence_4_octet;

   function To_Any
     (Item : IDL_AT_Sequence_4_octet)
     return PolyORB.Any.Any;

   TC_RR_Data : PolyORB.Any.TypeCode.Local_Ref;

   function From_Any
     (Item : PolyORB.Any.Any)
     return RR_Data;

   function To_Any
     (Item : RR_Data)
     return PolyORB.Any.Any;

   TC_RR : PolyORB.Any.TypeCode.Local_Ref;

   function From_Any
     (Item : PolyORB.Any.Any)
     return RR;

   function To_Any
     (Item : RR)
     return PolyORB.Any.Any;

   TC_Rcode : PolyORB.Any.TypeCode.Local_Ref;

   function From_Any
     (Item : PolyORB.Any.Any)
     return Rcode;

   function To_Any
     (Item : Rcode)
     return PolyORB.Any.Any;

   TC_IDL_SEQUENCE_DNS_RR : PolyORB.Any.TypeCode.Local_Ref;

   function From_Any
     (Item : PolyORB.Any.Any)
     return IDL_SEQUENCE_DNS_RR.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_DNS_RR.Sequence)
     return PolyORB.Any.Any;

   TC_rrSequence : PolyORB.Any.TypeCode.Local_Ref;

   function From_Any
     (Item : PolyORB.Any.Any)
     return rrSequence;

   function To_Any
     (Item : rrSequence)
     return PolyORB.Any.Any;

   package Internals is

      function From_Any
        (C : PolyORB.Any.Any_Container'Class)
        return RR_Type;

      type Ptr_RR_Type is
        access all RR_Type;

      type Content_RR_Type is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_RR_Type;
            Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
      end record;

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_RR_Type;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      overriding procedure Set_Aggregate_Element
        (Acc : in out Content_RR_Type;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class);

      overriding function Get_Aggregate_Count
        (Acc : Content_RR_Type)
        return PolyORB.Types.Unsigned_Long;

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_RR_Type;
         Count : PolyORB.Types.Unsigned_Long);

      overriding function Unchecked_Get_V
        (Acc : not null access Content_RR_Type)
        return PolyORB.Types.Address;

      overriding function Clone
        (Acc : Content_RR_Type;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      overriding procedure Finalize_Value
        (Acc : in out Content_RR_Type);

      function Wrap
        (X : access RR_Type)
        return PolyORB.Any.Content'Class;

      procedure Initialize_RR_Type;

      type Ptr_SRV_Data is
        access all SRV_Data;

      type Content_SRV_Data is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_SRV_Data;
      end record;

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_SRV_Data;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      overriding function Get_Aggregate_Count
        (Acc : Content_SRV_Data)
        return PolyORB.Types.Unsigned_Long;

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_SRV_Data;
         Count : PolyORB.Types.Unsigned_Long);

      overriding function Unchecked_Get_V
        (Acc : not null access Content_SRV_Data)
        return PolyORB.Types.Address;

      overriding function Clone
        (Acc : Content_SRV_Data;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      overriding procedure Finalize_Value
        (Acc : in out Content_SRV_Data);

      function Wrap
        (X : access SRV_Data)
        return PolyORB.Any.Content'Class;

      procedure Initialize_SRV_Data;

      function IDL_SEQUENCE_4_octet_Element_Wrap
        (X : access PolyORB.Types.Octet)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access IDL_SEQUENCE_4_octet.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_4_octet_Helper is
        new IDL_SEQUENCE_4_octet.Helper
           (Element_From_Any => PolyORB.Any.From_Any,
            Element_To_Any => PolyORB.Any.To_Any,
            Element_Wrap =>
            Helper.Internals.IDL_SEQUENCE_4_octet_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_4_octet;

      procedure Initialize_IDL_AT_Sequence_4_octet;

      type Ptr_RR_Data is
        access all RR_Data;

      type Content_RR_Data is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_RR_Data;
            Switch_Cache : aliased RR_Type;
      end record;

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_RR_Data;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      overriding procedure Set_Aggregate_Element
        (Acc : in out Content_RR_Data;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class);

      overriding function Get_Aggregate_Count
        (Acc : Content_RR_Data)
        return PolyORB.Types.Unsigned_Long;

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_RR_Data;
         Count : PolyORB.Types.Unsigned_Long);

      overriding function Unchecked_Get_V
        (Acc : not null access Content_RR_Data)
        return PolyORB.Types.Address;

      overriding function Clone
        (Acc : Content_RR_Data;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      overriding procedure Finalize_Value
        (Acc : in out Content_RR_Data);

      function Wrap
        (X : access RR_Data)
        return PolyORB.Any.Content'Class;

      procedure Initialize_RR_Data;

      type Ptr_RR is
        access all RR;

      type Content_RR is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_RR;
      end record;

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_RR;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      overriding function Get_Aggregate_Count
        (Acc : Content_RR)
        return PolyORB.Types.Unsigned_Long;

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_RR;
         Count : PolyORB.Types.Unsigned_Long);

      overriding function Unchecked_Get_V
        (Acc : not null access Content_RR)
        return PolyORB.Types.Address;

      overriding function Clone
        (Acc : Content_RR;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      overriding procedure Finalize_Value
        (Acc : in out Content_RR);

      function Wrap
        (X : access RR)
        return PolyORB.Any.Content'Class;

      procedure Initialize_RR;

      function From_Any
        (C : PolyORB.Any.Any_Container'Class)
        return Rcode;

      type Ptr_Rcode is
        access all Rcode;

      type Content_Rcode is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Rcode;
            Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
      end record;

      overriding function Get_Aggregate_Element
        (Acc : not null access Content_Rcode;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      overriding procedure Set_Aggregate_Element
        (Acc : in out Content_Rcode;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class);

      overriding function Get_Aggregate_Count
        (Acc : Content_Rcode)
        return PolyORB.Types.Unsigned_Long;

      overriding procedure Set_Aggregate_Count
        (Acc : in out Content_Rcode;
         Count : PolyORB.Types.Unsigned_Long);

      overriding function Unchecked_Get_V
        (Acc : not null access Content_Rcode)
        return PolyORB.Types.Address;

      overriding function Clone
        (Acc : Content_Rcode;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      overriding procedure Finalize_Value
        (Acc : in out Content_Rcode);

      function Wrap
        (X : access Rcode)
        return PolyORB.Any.Content'Class;

      procedure Initialize_Rcode;

      function IDL_SEQUENCE_DNS_RR_Element_Wrap
        (X : access RR)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access IDL_SEQUENCE_DNS_RR.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_DNS_RR_Helper is
        new IDL_SEQUENCE_DNS_RR.Helper
           (Element_From_Any => Helper.From_Any,
            Element_To_Any => Helper.To_Any,
            Element_Wrap => Helper.Internals.IDL_SEQUENCE_DNS_RR_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_DNS_RR;

      procedure Initialize_rrSequence;

   end Internals;

end PolyORB.DSA_P.Name_Service.mDNS.Helper;
