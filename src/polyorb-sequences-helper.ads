------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . S E Q U E N C E S . H E L P E R              --
--                                                                          --
--                                 S p e c                                  --
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

with System;

with PolyORB.Any;
with PolyORB.Types;

generic
   type Element is private;
   type Element_Ptr is access all Element;
   type Sequence is private;

   with function Length (Seq : Sequence) return Natural;
   --  Return Seq's current length

   with function New_Sequence (Length : Natural) return Sequence;
   --  Create a new sequence of the given Length

   with procedure Set_Length (Source : in out Sequence; Length : Natural);

   with function Unchecked_Element_Of
     (Source : not null access Sequence;
      Index  : Positive) return Element_Ptr;
   --  Access to the Index'th (1-based) element in Seq

   with function Element_From_Any (Item : PolyORB.Any.Any) return Element;
   with function Element_To_Any   (Item : Element) return PolyORB.Any.Any;
   pragma Unreferenced (Element_To_Any);
   with function Element_Wrap (X : access Element)
     return PolyORB.Any.Content'Class;

package PolyORB.Sequences.Helper is

   function From_Any (Item : PolyORB.Any.Any) return Sequence;
   function To_Any   (Item : Sequence) return PolyORB.Any.Any;
   function Wrap (X : access Sequence) return PolyORB.Any.Content'Class;

   procedure Initialize
     (Element_TC, Sequence_TC : PolyORB.Any.TypeCode.Local_Ref);

private

   --  Aggregate container

   type Sequence_Ptr is access all Sequence;
   type Sequence_Content is new Any.Aggregate_Content with record
      V : Sequence_Ptr;
      Length_Cache : PolyORB.Types.Unsigned_Long;
   end record;

   --  Aggregate container primitives

   overriding function Get_Aggregate_Element
     (ACC   : not null access Sequence_Content;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class;

   overriding procedure Set_Aggregate_Element
     (ACC    : in out Sequence_Content;
      TC     : PolyORB.Any.TypeCode.Object_Ptr;
      Index  : Types.Unsigned_Long;
      From_C : in out PolyORB.Any.Any_Container'Class);

   overriding function Get_Aggregate_Count
     (ACC : Sequence_Content) return PolyORB.Types.Unsigned_Long;

   overriding procedure Set_Aggregate_Count
     (ACC : in out Sequence_Content;
      Count : PolyORB.Types.Unsigned_Long);

   overriding function Clone
     (ACC  : Sequence_Content;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr;

   overriding function Unchecked_Get_V
     (ACC : not null access Sequence_Content) return System.Address;
   --  Return the address of the first stored element

   overriding procedure Finalize_Value
     (ACC : in out Sequence_Content);

end PolyORB.Sequences.Helper;
