------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E Q U E N C E S . U N B O U N D E D . H E L P E R    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

--  Any conversion subprograms for bounded sequences.

with PolyORB.Sequences.Helper;

package body PolyORB.Sequences.Unbounded.Helper is

   use PolyORB.Any;

   --  Element accessors to be passed to generic helper package

   procedure Set_Element
     (Seq : in out Sequence; Index : Positive; Value : Element);
   pragma Inline (Set_Element);

   function Get_Element (Seq : Sequence; Index : Positive) return Element;
   pragma Inline (Get_Element);

   package Unbounded_Helper is new Sequences.Helper
     (Element          => Element,
      Sequence         => Sequence,
      Length           => Length,
      New_Sequence     => New_Sequence,
      Get_Element      => Get_Element,
      Set_Element      => Set_Element,
      Element_From_Any => Element_From_Any,
      Element_To_Any   => Element_To_Any);

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any.Any) return Sequence
     renames Unbounded_Helper.From_Any;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (Seq : Sequence; Index : Positive) return Element is
   begin
      return Seq.Content (Index);
   end Get_Element;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Element_TC, Sequence_TC : PolyORB.Any.TypeCode.Object)
   is
   begin
      Unbounded_Helper.Initialize
        (Element_TC => Element_TC,
         Sequence_TC => Sequence_TC);
   end Initialize;

   -----------------
   -- Set_Element --
   -----------------

   procedure Set_Element
     (Seq : in out Sequence; Index : Positive; Value : Element) is
   begin
      Seq.Content (Index) := Value;
   end Set_Element;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Sequence) return Any.Any
     renames Unbounded_Helper.To_Any;

end PolyORB.Sequences.Unbounded.Helper;
