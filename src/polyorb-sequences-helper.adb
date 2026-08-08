------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . S E Q U E N C E S . H E L P E R              --
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

pragma Ada_2012;

--  Any conversion subprograms for sequences (both bounded and unbounded)

with PolyORB.Utils.Unchecked_Deallocation;

package body PolyORB.Sequences.Helper is

   use PolyORB.Any;
   use PolyORB.Types;

   --  Global data

   Initialized : Boolean := False;
   Sequence_TC, Element_TC : PolyORB.Any.TypeCode.Local_Ref;

   -----------
   -- Clone --
   -----------

   overriding function Clone
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

   overriding procedure Finalize_Value
     (ACC : in out Sequence_Content)
   is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Sequence,
         Name => Sequence_Ptr);
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

   overriding function Get_Aggregate_Count
     (ACC : Sequence_Content) return PolyORB.Types.Unsigned_Long is
   begin
      return PolyORB.Types.Unsigned_Long
        (Length (ACC.V.all) + 1);
   end Get_Aggregate_Count;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   overriding function Get_Aggregate_Element
     (ACC   : not null access Sequence_Content;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class
   is
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

   overriding procedure Set_Aggregate_Count
     (ACC   : in out Sequence_Content;
      Count : PolyORB.Types.Unsigned_Long) is
   begin
      Set_Length (ACC.V.all, Length => Integer (Count - 1));
   end Set_Aggregate_Count;

   ---------------------------
   -- Set_Aggregate_Element --
   ---------------------------

   overriding procedure Set_Aggregate_Element
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
      Result : Any.Any;
   begin
      Set_Type (Result, Sequence_TC);
      Set_Value
        (Get_Container (Result).all,
         new Sequence_Content'
           (Any.Aggregate_Content with
              V            => new Sequence'(Item),
              Length_Cache => Unsigned_Long (Length (Item))),
         Foreign => False);
      return Result;
   end To_Any;

   ---------------------
   -- Unchecked_Get_V --
   ---------------------

   overriding function Unchecked_Get_V
     (ACC : not null access Sequence_Content) return System.Address
   is
   begin
      if ACC.V = null or else Length (ACC.V.all) = 0 then
         return System.Null_Address;
      end if;
      return Unchecked_Element_Of (ACC.V, 1).all'Address;
   end Unchecked_Get_V;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access Sequence) return Any.Content'Class is
   begin
      return Sequence_Content'(Any.Aggregate_Content with
                               V => Sequence_Ptr (X), Length_Cache => 0);
   end Wrap;

end PolyORB.Sequences.Helper;
