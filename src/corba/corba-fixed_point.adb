------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C O R B A . F I X E D _ P O I N T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;

package body CORBA.Fixed_Point is

   use PolyORB.Log;

   -----------
   -- Debug --
   -----------

   package L is new PolyORB.Log.Facility_Log ("corba.fixed_point");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   package CDR_Fixed_F is
     new PolyORB.Representations.CDR.Common.Fixed_Point (F);

   TC_Cache : TypeCode.Object;

   function TC return TypeCode.Object;
   --  Return typecode for this fixed point type

   --------
   -- TC --
   --------

   function TC return TypeCode.Object is
   begin
      if CORBA.TypeCode.Internals.Is_Nil (TC_Cache) then
         TC_Cache := CORBA.TypeCode.Internals.To_CORBA_Object
           (PolyORB.Any.TypeCode.Build_Complex_TC (Tk_Fixed,
            (PolyORB.Any.To_Any (PolyORB.Types.Unsigned_Short (F'Digits)),
             PolyORB.Any.To_Any (PolyORB.Types.Short (F'Scale)))));
      end if;
      return TC_Cache;
   end TC;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : F) return CORBA.Any is
      Result : Any := CORBA.Internals.Get_Empty_Any_Aggregate (TC);
      Octets : constant Ada.Streams.Stream_Element_Array :=
                 CDR_Fixed_F.Fixed_To_Octets (Item);
   begin
      for I in Octets'Range loop
         CORBA.Internals.Add_Aggregate_Element
           (Result, CORBA.To_Any (CORBA.Octet (Octets (I))));
      end loop;
      return Result;
   end To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any) return F is
      use type PolyORB.Any.TCKind;

   begin
      pragma Debug (C, O ("From_Any (Fixed) : enter"));
      if PolyORB.Any.TypeCode.Kind (Internals.Get_Unwound_Type (Item))
        /= PolyORB.Any.Tk_Fixed
      then
         pragma Debug
           (C, O ("From_Any (Fixed) : Bad_TypeCode, type is " &
               PolyORB.Any.TCKind'Image
                (PolyORB.Any.TypeCode.Kind
                 (Internals.Get_Unwound_Type (Item)))));
         raise Bad_TypeCode;
      end if;

      declare
         use Ada.Streams;

         Nb : constant CORBA.Unsigned_Long :=
           CORBA.Internals.Get_Aggregate_Count (Item);
         Octets : Stream_Element_Array (1 .. Stream_Element_Offset (Nb)) :=
           (others => 0);
      begin
         for J in Octets'Range loop
            pragma Debug (C, O ("From_Any (Fixed) : yet another octet"));
            Octets (J) :=
              Stream_Element
               (PolyORB.Types.Octet'(PolyORB.Any.Get_Aggregate_Element
                (PolyORB.Any.Any (Item),
                 PolyORB.Types.Unsigned_Long (J - 1))));
         end loop;
         pragma Debug (C, O ("From_Any (Fixed) : return"));
         return CDR_Fixed_F.Octets_To_Fixed (Octets);

      exception when CORBA.Marshal =>
         pragma Debug (C, O ("From_Any (Fixed) : exception catched" &
                          "while returning"));
         raise CORBA.Bad_TypeCode;
      end;
   end From_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access F) return PolyORB.Any.Content'Class is
   begin
      return Fixed_Content'
        (PolyORB.Any.Aggregate_Content with
           V          => X.all'Unrestricted_Access,
           Repr_Cache => (others => 0));
   end Wrap;

   -----------
   -- Clone --
   -----------

   function Clone
     (ACC  : Fixed_Content;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      use type PolyORB.Any.Content_Ptr;
      Target : PolyORB.Any.Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Fixed_Content then
            return null;
         end if;
         Target := Into;
      else
         Target := new Fixed_Content;
         Fixed_Content (Target.all).V := new F;
      end if;

      Fixed_Content (Target.all).V.all := ACC.V.all;
      Fixed_Content (Target.all).Repr_Cache := ACC.Repr_Cache;
      return Target;
   end Clone;

   --------------------
   -- Finalize_Value --
   --------------------

   procedure Finalize_Value (ACC : in out Fixed_Content) is
      procedure Free is new Ada.Unchecked_Deallocation (F, F_Ptr);
   begin
      Free (ACC.V);
   end Finalize_Value;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (ACC   : not null access Fixed_Content;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class
   is
      pragma Unreferenced (TC);
      use Ada.Streams;
      use type PolyORB.Any.Mechanism;
   begin

      --  If getting first element, and accessing for read, prime cache

      if Mech.all = PolyORB.Any.By_Value
        and then Stream_Element_Offset (Index) = ACC.Repr_Cache'First
      then
         ACC.Repr_Cache := CDR_Fixed_F.Fixed_To_Octets (ACC.V.all);
      end if;

      Mech.all := PolyORB.Any.By_Value;
      return PolyORB.Any.Wrap
        (PolyORB.Types.Octet
          (ACC.Repr_Cache
           (Stream_Element_Offset (Index)))'Unrestricted_Access);
   end Get_Aggregate_Element;

   ---------------------------
   -- Set_Aggregate_Element --
   ---------------------------

   procedure Set_Aggregate_Element
     (ACC    : in out Fixed_Content;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : PolyORB.Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class)
   is
      pragma Unreferenced (TC);
      use Ada.Streams;
   begin
      ACC.Repr_Cache (Stream_Element_Offset (Index)) :=
        Stream_Element
          (PolyORB.Types.Octet'(PolyORB.Any.From_Any (From_C)));

      --  If setting last element, update actual fixed value

      if Stream_Element_Offset (Index) = ACC.Repr_Cache'Last then
         ACC.V.all := CDR_Fixed_F.Octets_To_Fixed (ACC.Repr_Cache);
      end if;
   end Set_Aggregate_Element;

   -------------------------
   -- Get_Aggregate_Count --
   -------------------------

   function Get_Aggregate_Count
     (ACC : Fixed_Content) return PolyORB.Types.Unsigned_Long
   is
      pragma Unreferenced (ACC);
   begin
      return Fixed_Content_Count;
   end Get_Aggregate_Count;

   -------------------------
   -- Set_Aggregate_Count --
   -------------------------

   procedure Set_Aggregate_Count
     (ACC   : in out Fixed_Content;
      Count : PolyORB.Types.Unsigned_Long)
   is
      pragma Unreferenced (ACC);
   begin
      if Count /= Fixed_Content_Count then
         raise Constraint_Error;
      end if;
   end Set_Aggregate_Count;

end CORBA.Fixed_Point;
