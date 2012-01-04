------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E Q U E N C E S . B O U N D E D . H E L P E R      --
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

--  Any conversion subprograms for bounded sequences

with PolyORB.Any;
with PolyORB.Sequences.Helper;

generic
   with function Element_From_Any (Item : PolyORB.Any.Any) return Element;
   with function Element_To_Any   (Item : Element) return PolyORB.Any.Any;
   with function Element_Wrap (X : access Element)
     return PolyORB.Any.Content'Class;

package PolyORB.Sequences.Bounded.Helper is

   function From_Any (Item : PolyORB.Any.Any) return Sequence;
   function To_Any   (Item : Sequence) return PolyORB.Any.Any;
   function Wrap (X : access Sequence) return PolyORB.Any.Content'Class;

   procedure Initialize
     (Element_TC, Sequence_TC : PolyORB.Any.TypeCode.Local_Ref);

private

   function Check_Length (Length : Natural) return Sequence;
   --  Return an empty sequence initialized with the given Length, unless
   --  Length > Max, in which case Constraint_Error is raised.

   package Bounded_Helper is new Sequences.Helper
     (Element              => Element,
      Element_Ptr          => Element_Ptr,
      Sequence             => Sequence,
      Length               => Length,
      New_Sequence         => Check_Length,
      Set_Length           => Set_Length,
      Unchecked_Element_Of => Unchecked_Element_Of,
      Element_From_Any     => Element_From_Any,
      Element_To_Any       => Element_To_Any,
      Element_Wrap         => Element_Wrap);

end PolyORB.Sequences.Bounded.Helper;
