------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . S E Q U E N C E S . H E L P E R              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2007, Free Software Foundation, Inc.          --
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

--  Any conversion subprograms for sequences (both bounded and unbounded)

with Ada.Unchecked_Deallocation;

package body PolyORB.Sequences.Helper is

   use PolyORB.Any;
   use PolyORB.Types;

   --  Global data

   Initialized : Boolean := False;
   Sequence_TC, Element_TC : PolyORB.Any.TypeCode.Local_Ref;

   -----------
   -- Clone --
   -----------

   function Clone
     (ACC  : Sequence_Content;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
      Target : Content_Ptr;
   begin
      if Into /= null then
         if Into.all not in Sequence_Content then
            return null;
         end if;
         Target := Into;
      else
         Target := new Sequence_Content;
         Sequence_Content (Target.all).V := new Sequence;
      end if;

      Sequence_Content (Target.all).V.all := ACC.V.all;
      Sequence_Content (Target.all).Length_Cache := ACC.Length_Cache;
      return Target;
   end Clone;

   --------------------
   -- Finalize_Value --
   --------------------

   procedure Finalize_Value
     (ACC : in out Sequence_Content)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Sequence, Sequence_Ptr);
   begin
      Free (ACC.V);
   end Finalize_Value;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any.Any) return Sequence
   is
      pragma Suppress (Discriminant_Check);

      Len : constant Integer :=
              Integer (Types.Unsigned_Long'(Get_Aggregate_Element (Item, 0)));

      Result : aliased Sequence := New_Sequence (Len);

   begin
      pragma Assert (Initialized);
      for J in 1 .. Len loop
         Unchecked_Element_Of (Result'Access, J).all :=
           Element_From_Any
             (Get_Aggregate_Element
              (Item, Element_TC, Types.Unsigned_Long (J)));
      end loop;
      return Result;
   end From_Any;

   -------------------------
   -- Get_Aggregate_Count --
   -------------------------

   function Get_Aggregate_Count
     (ACC : Sequence_Content) return PolyORB.Types.Unsigned_Long is
   begin
      return PolyORB.Types.Unsigned_Long
        (Length (ACC.V.all) + 1);
   end Get_Aggregate_Count;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   function Get_Aggregate_Element
     (ACC   : access Sequence_Content;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : access PolyORB.Any.Mechanism) return PolyORB.Any.Content'Class
   is
      use type PolyORB.Types.Unsigned_Long;
      use type PolyORB.Any.Mechanism;
      pragma Unreferenced (TC);
   begin
      if Index = 0 then
         Mech.all := PolyORB.Any.By_Value;
         ACC.Length_Cache := Types.Unsigned_Long (Length (ACC.V.all));
         return PolyORB.Any.Wrap (ACC.Length_Cache'Unrestricted_Access);
      else
         Mech.all := PolyORB.Any.By_Reference;
         return Element_Wrap
                  (Unchecked_Element_Of (ACC.V, Standard.Positive (Index)));
      end if;
   end Get_Aggregate_Element;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Element_TC, Sequence_TC : PolyORB.Any.TypeCode.Local_Ref)
   is
   begin
      Helper.Element_TC  := Element_TC;
      Helper.Sequence_TC := Sequence_TC;
      Initialized := True;
   end Initialize;

   -------------------------
   -- Set_Aggregate_Count --
   -------------------------

   procedure Set_Aggregate_Count
     (ACC   : in out Sequence_Content;
      Count : PolyORB.Types.Unsigned_Long)
   is
      use type PolyORB.Types.Unsigned_Long;
   begin
      Set_Length (ACC.V.all, Length => Integer (Count - 1));
   end Set_Aggregate_Count;

   ---------------------------
   -- Set_Aggregate_Element --
   ---------------------------

   procedure Set_Aggregate_Element
     (ACC    : in out Sequence_Content;
      TC     : TypeCode.Object_Ptr;
      Index  : Types.Unsigned_Long;
      From_C : in out Any_Container'Class)
   is
      pragma Unreferenced (TC);
   begin

      --  For a sequence aggregate, only index 0 (the length item) is by value

      pragma Assert (Index = 0);

      --  Check consistency and discard value

      pragma Assert (PolyORB.Types.Unsigned_Long (Length (ACC.V.all))
                     = PolyORB.Any.From_Any (From_C));

      null;
   end Set_Aggregate_Element;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Sequence) return Any.Any is
      pragma Assert (Initialized);
      Result : Any.Any := Get_Empty_Any_Aggregate (Sequence_TC);

   begin
      Add_Aggregate_Element
        (Result, To_Any (Types.Unsigned_Long (Length (Item))));

      for J in 1 .. Length (Item) loop
         Add_Aggregate_Element (Result,
           Element_To_Any
             (Unchecked_Element_Of (Item'Unrestricted_Access, J).all));
      end loop;
      return Result;
   end To_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access Sequence) return Any.Content'Class is
   begin
      return Sequence_Content'(Any.Aggregate_Content with
                               V => Sequence_Ptr (X), Length_Cache => 0);
   end Wrap;

end PolyORB.Sequences.Helper;
