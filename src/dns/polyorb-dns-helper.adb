------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . D N S . H E L P E R                    --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Log;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body PolyORB.DNS.Helper is
   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("polyorb.dns.helper");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;

   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;
   procedure Initialize is
   begin
      Initialize_Rcode;
      Initialize_RR_Type;
      Initialize_RR;
      Initialize_rrSequence;
   end Initialize;

   procedure Initialize_RR_Type is
         Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("RR_Type");
         Id : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("IDL:DNS/RR_Type:1.0");
         A_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String  ("A");
         NS_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("NS");
         SOA_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("SOA");
         CNAME_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("CNAME");
         PTR_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("PTR");
         TXT_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("TXT");
         SRV_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("SRV");
   begin
      TC_RR_Type := PolyORB.Any.TypeCode.TC_Enum;
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (Name));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (Id));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (A_Name));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (NS_Name));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (SOA_Name));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (CNAME_Name));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (PTR_Name));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (TXT_Name));
      Any.TypeCode.Add_Parameter
        (TC_RR_Type, Any.To_Any (SRV_Name));
      Any.TypeCode.Disable_Reference_Counting
        (Any.TypeCode.Object_Of (TC_RR_Type).all);
   end Initialize_RR_Type;

   --------------
   -- From_Any --
   --------------
   function From_Any
     (C : PolyORB.Any.Any_Container'Class) return RR_Type
   is
   begin
      return RR_Type'Val
        (PolyORB.Types.Unsigned_Long'
           (PolyORB.Any.Get_Aggregate_Element (C, 0)));
   end From_Any;
   function From_Any
     (Item : PolyORB.Any.Any)
     return RR_Type
   is
   begin
      return From_Any
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

   function Wrap
        (X : access RR_Type)
        return PolyORB.Any.Content'Class
   is
   begin
      return Content_RR_Type'(PolyORB.Any.Aggregate_Content with
            V => Ptr_RR_Type (X), Repr_Cache => 0);
   end Wrap;
   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
        (Acc : not null access Content_RR_Type;
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
      Acc.Repr_Cache := RR_Type'Pos (Acc.V.all);
      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap (Acc.Repr_Cache'Unrestricted_Access);
   end Get_Aggregate_Element;

      ---------------------------
      -- Set_Aggregate_Element --
      ---------------------------

   procedure Set_Aggregate_Element
        (Acc : in out Content_RR_Type;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         From_C : in out PolyORB.Any.Any_Container'Class)
      is
         use type PolyORB.Types.Unsigned_Long;
         pragma Assert ((Index = 0));
         pragma Unreferenced (Tc);
   begin
      Acc.V.all := RR_Type'Val (PolyORB.Types.Unsigned_Long'
                 (PolyORB.Any.From_Any (From_C)));
   end Set_Aggregate_Element;

      -------------------------
      -- Get_Aggregate_Count --
      -------------------------

   function Get_Aggregate_Count
        (Acc : Content_RR_Type)
        return PolyORB.Types.Unsigned_Long
      is
         pragma Unreferenced (Acc);
   begin
      return 1;
   end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

   procedure Set_Aggregate_Count
        (Acc : in out Content_RR_Type;
         Count : PolyORB.Types.Unsigned_Long)
   is
   begin
      null;
   end Set_Aggregate_Count;

      -----------
      -- Clone --
      -----------

      function Clone
        (Acc : Content_RR_Type;
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
            Target := Into;
            Content_RR_Type
              (Target.all).V.all := Acc.V.all;
         else
            Target := new Content_RR_Type;
            Content_RR_Type (Target.all).V := new RR_Type'(Acc.V.all);
         end if;
         Content_RR_Type (Target.all).Repr_Cache := Acc.Repr_Cache;
         return Target;
      end Clone;

      --------------------
      -- Finalize_Value --
      --------------------

   procedure Finalize_Value
     (Acc : in out Content_RR_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
              (RR_Type, Ptr_RR_Type);
   begin
      Free (Acc.V);
   end Finalize_Value;

   --  Utilities for the RR type
   procedure Initialize_RR is
   begin
      TC_RR := PolyORB.Any.TypeCode.TC_Struct;
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any ("RR"));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any ("IDL:DNS/RR:1.0"));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any (Any.TypeCode.TC_String));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any ("rr_name"));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any (TC_RR_Type));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any ("rr_type"));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any (Any.TypeCode.TC_String));
      Any.TypeCode.Add_Parameter (TC_RR, Any.To_Any ("rr_answer"));

      Any.TypeCode.Disable_Reference_Counting
        (Any.TypeCode.Object_Of (TC_RR).all);
   end Initialize_RR;
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
            PolyORB.Any.TypeCode.TC_String,
            0)),
      rr_type => From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            TC_RR_Type,
                    1)),
      rr_answer => PolyORB.Any.From_Any
        (PolyORB.Any.Get_Aggregate_Element
           (Item,
            PolyORB.Any.TypeCode.TC_String,
            2)));
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
        (Result, To_Any (Item.rr_type));
      PolyORB.Any.Add_Aggregate_Element
        (Result,
         PolyORB.Any.To_Any
           (Item.rr_answer));
      return Result;
   end To_Any;

         -----------
      -- Clone --
      -----------

   function Clone
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
         Target := Into;
         Content_RR (Target.all).V.all := Acc.V.all;
      else
         Target := new Content_RR;
         Content_RR (Target.all).V := new RR'(Acc.V.all);
      end if;
      return Target;
   end Clone;
   procedure Finalize_Value
        (Acc : in out Content_RR)
   is
      procedure Free is new Ada.Unchecked_Deallocation (RR, Ptr_RR);
   begin
         Free (Acc.V);
   end Finalize_Value;

         ---------------------------
      -- Get_Aggregate_Element --
      ---------------------------

   function Get_Aggregate_Element
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
      Mech.all := PolyORB.Any.By_Reference;
      case Index is
         when 0 =>
            return PolyORB.Any.Wrap (Acc.V.rr_name'Unrestricted_Access);
         when 1 =>
            return Wrap (Acc.V.rr_type'Unrestricted_Access);
         when 2 =>
            return PolyORB.Any.Wrap (Acc.V.rr_answer'Unrestricted_Access);
         pragma Warnings (Off);
         when others =>
            raise Constraint_Error;
         pragma Warnings (On);
      end case;
   end Get_Aggregate_Element;

   function Get_Aggregate_Count
      (Acc : Content_RR)
      return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (Acc);
   begin
      return 2;
   end Get_Aggregate_Count;

      -------------------------
      -- Set_Aggregate_Count --
      -------------------------

   procedure Set_Aggregate_Count
        (Acc : in out Content_RR;
         Count : PolyORB.Types.Unsigned_Long)
   is
   begin
         null;
   end Set_Aggregate_Count;

   function Wrap (X : access RR)
        return PolyORB.Any.Content'Class
   is
   begin
      return Content_RR'(PolyORB.Any.Aggregate_Content with
            V => Ptr_RR (X));
   end Wrap;
   --  Utilities for the rrSequence type
   procedure Initialize_rrSequence is
   begin
      TC_SEQUENCE_RR := Any.TypeCode.Build_Sequence_TC
                 (TC_RR, 0);
      Any.TypeCode.Disable_Reference_Counting
        (Any.TypeCode.Object_Of (TC_SEQUENCE_RR).all);

      TC_rrSequence := PolyORB.Any.TypeCode.TC_Alias;
      Any.TypeCode.Add_Parameter (TC_rrSequence, Any.To_Any ("rrSequence"));
      Any.TypeCode.Add_Parameter (TC_rrSequence,
                                  Any.To_Any ("IDL:DNS/rrSequence:1.0"));
      Any.TypeCode.Add_Parameter (TC_rrSequence, Any.To_Any
                                  (TC_SEQUENCE_RR));
      Any.TypeCode.Disable_Reference_Counting
        (Any.TypeCode.Object_Of (TC_rrSequence).all);
      SEQUENCE_RR_Helper.Initialize
              (Element_TC => TC_RR,
               Sequence_TC => TC_rrSequence);
   end Initialize_rrSequence;
   function SEQUENCE_RR_Element_Wrap
        (X : access RR)
         return PolyORB.Any.Content'Class
   is
   begin
      return Wrap (X.all'Unrestricted_Access);
   end SEQUENCE_RR_Element_Wrap;

   function Wrap
        (X : access SEQUENCE_RR.Sequence)
         return PolyORB.Any.Content'Class
     renames SEQUENCE_RR_Helper.Wrap;

   function From_Any
     (Item : PolyORB.Any.Any)
     return SEQUENCE_RR.Sequence renames
      SEQUENCE_RR_Helper.From_Any;

   function To_Any
     (Item : SEQUENCE_RR.Sequence)
      return PolyORB.Any.Any renames
       SEQUENCE_RR_Helper.To_Any;

   function From_Any
     (Item : PolyORB.Any.Any)
      return rrSequence
   is
      Result : constant SEQUENCE_RR.Sequence := From_Any (Item);
   begin
      return rrSequence (Result);
   end From_Any;

   function To_Any
     (Item : rrSequence)
      return PolyORB.Any.Any
   is
      Result : PolyORB.Any.Any :=
        To_Any (SEQUENCE_RR.Sequence (Item));
   begin
      PolyORB.Any.Set_Type (Result, TC_rrSequence);
      return Result;
   end To_Any;

   --  Utilities for the Rcode type
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
   function Get_Aggregate_Element
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
   procedure Set_Aggregate_Element
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
   function Get_Aggregate_Count
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
   procedure Set_Aggregate_Count
     (Acc : in out Content_Rcode;
      Count : PolyORB.Types.Unsigned_Long)
   is
   begin
      null;
   end Set_Aggregate_Count;

   ---------------------
   -- Unchecked_Get_V --
   ---------------------
   function Unchecked_Get_V
     (Acc : not null access Content_Rcode)
     return PolyORB.Types.Address
   is
      function To_Address
        is new Ada.Unchecked_Conversion
           (Ptr_Rcode,
            PolyORB.Types.Address);
   begin
      return To_Address (Acc.V);
   end Unchecked_Get_V;

   -----------
   -- Clone --
   -----------
   function Clone
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

         Target := Into;
         Content_Rcode
           (Target.all).V.all :=
           Acc.V.all;
      else
         Target :=
           new Content_Rcode;
         Content_Rcode
           (Target.all).V := new Rcode'
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
   procedure Finalize_Value
     (Acc : in out Content_Rcode)
   is
      procedure Free
        is new Ada.Unchecked_Deallocation
           (Rcode,
            Ptr_Rcode);
   begin
      Free (Acc.V);
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

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : PolyORB.Any.Any)
     return Rcode
   is
   begin
      return From_Any
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

   ----------------------
   -- Initialize_Rcode --
   ----------------------

   procedure Initialize_Rcode is
         Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("Rcode");
         Id : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("IDL:DNS/Rcode:1.0");
         No_Error_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String  ("No_Error");
         Format_Error_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("Format_Error");
         Server_Failure_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("Server_Failure");
         Name_Error_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("Name_Error");
         Not_Implemented_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("Not_Implemented");
         Refused_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("Refused");
         YX_Domain_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("YX_Domain");
         YX_RRSet_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("YX_RRSet");
         NX_RRSet_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("NX_RRSet");
         Not_Auth_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("Not_Auth");
         Not_Zone_Name : constant PolyORB.Types.String :=
            PolyORB.Types.To_PolyORB_String ("Not_Zone");
   begin
      TC_Rcode := PolyORB.Any.TypeCode.TC_Enum;
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Id));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (No_Error_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Format_Error_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Server_Failure_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Name_Error_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Not_Implemented_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Refused_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (YX_Domain_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (YX_RRSet_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (NX_RRSet_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Not_Auth_Name));
      Any.TypeCode.Add_Parameter
        (TC_Rcode, Any.To_Any (Not_Zone_Name));
      Any.TypeCode.Disable_Reference_Counting
        (Any.TypeCode.Object_Of (TC_Rcode).all);
   end Initialize_Rcode;

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
       Init      =>  Initialize'Access,
       Shutdown  => null));

end PolyORB.DNS.Helper;
