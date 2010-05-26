------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . D N S . H E L P E R                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2010, Free Software Foundation, Inc.             --
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

with PolyORB.Types;
with PolyORB.Sequences.Unbounded;
with PolyORB.Sequences.Unbounded.Helper;

pragma Elaborate_All (PolyORB.Sequences.Unbounded.Helper);
with PolyORB.Any;

package PolyORB.DNS.Helper is

   type Rcode_Type is
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

   type Opcode_Type is
     (Query,
      IQuery,
      Status
     );

   type RR_Type is
     (A,
      NS,
      SOA,
      CNAME,
      PTR,
      TXT,
      SRV);

   type RR is record
      rr_name : PolyORB.Types.String;
      rr_type :  PolyORB.DNS.Helper.RR_Type;
   end record;

   package SEQUENCE_RR is
     new PolyORB.Sequences.Unbounded
       (RR);

   type rrSequence is
     new SEQUENCE_RR.Sequence;

   TC_RR_Type : PolyORB.Any.TypeCode.Local_Ref;
   TC_rrSequence : PolyORB.Any.TypeCode.Local_Ref;
   TC_RR : PolyORB.Any.TypeCode.Local_Ref;
   TC_SEQUENCE_RR : PolyORB.Any.TypeCode.Local_Ref;
   procedure Initialize_RR;
   function From_Any
     (C : PolyORB.Any.Any_Container'Class) return RR_Type;
   function From_Any
     (Item : PolyORB.Any.Any)
      return RR_Type;

   function To_Any
     (Item : RR_Type)
      return PolyORB.Any.Any;

   function From_Any
     (Item : PolyORB.Any.Any)
     return RR;

   function To_Any
     (Item : RR)
      return PolyORB.Any.Any;

   function From_Any
     (Item : PolyORB.Any.Any)
     return SEQUENCE_RR.Sequence;

   function To_Any
     (Item : SEQUENCE_RR.Sequence)
     return PolyORB.Any.Any;

   function From_Any
     (Item : PolyORB.Any.Any)
     return rrSequence;

   function To_Any
     (Item : rrSequence)
      return PolyORB.Any.Any;

   function SEQUENCE_RR_Element_Wrap
        (X : access RR)
        return PolyORB.Any.Content'Class;

   function Wrap
        (X : access SEQUENCE_RR.Sequence)
        return PolyORB.Any.Content'Class;

   package SEQUENCE_RR_Helper is
        new SEQUENCE_RR.Helper
       (Element_From_Any => PolyORB.DNS.Helper.From_Any,
        Element_To_Any => PolyORB.DNS.Helper.To_Any,
        Element_Wrap => SEQUENCE_RR_Element_Wrap);

   --  Utilities for the RR_Type type
   type Ptr_RR_Type is access all RR_Type;
   type Content_RR_Type is
        new PolyORB.Any.Aggregate_Content with record
         V : Ptr_RR_Type;
         Repr_Cache : aliased PolyORB.Types.Unsigned_Long;
   end record;

   function Wrap
        (X : access RR_Type)
        return PolyORB.Any.Content'Class;
   function Get_Aggregate_Element
        (Acc : not null access Content_RR_Type;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

   procedure Set_Aggregate_Element
        (Acc : in out Content_RR_Type;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class);

   function Get_Aggregate_Count
    (Acc : Content_RR_Type)
        return PolyORB.Types.Unsigned_Long;

   procedure Set_Aggregate_Count
        (Acc : in out Content_RR_Type;
         Count : PolyORB.Types.Unsigned_Long);
   function Clone
        (Acc : Content_RR_Type;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;
   procedure Finalize_Value (Acc : in out Content_RR_Type);

   --  Utilities for the RR type
   type Ptr_RR is
        access all RR;
   type Content_RR is new PolyORB.Any.Aggregate_Content with record
      V : Ptr_RR;
   end record;

   function Clone
     (Acc : Content_RR;
      Into : PolyORB.Any.Content_Ptr := null)
      return PolyORB.Any.Content_Ptr;

   procedure Finalize_Value
        (Acc : in out Content_RR);

   function Get_Aggregate_Count
      (Acc : Content_RR)
       return PolyORB.Types.Unsigned_Long;

   procedure Set_Aggregate_Count
      (Acc : in out Content_RR;
       Count : PolyORB.Types.Unsigned_Long);

   function Get_Aggregate_Element
     (Acc : not null access Content_RR;
      Tc : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class;
   function Wrap (X : access RR)
                  return PolyORB.Any.Content'Class;

end PolyORB.DNS.Helper;
