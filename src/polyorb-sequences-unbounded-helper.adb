------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E Q U E N C E S . U N B O U N D E D . H E L P E R    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

   type Element_Access is access all Element;

   function Element_Accessor (Seq : Sequence; Index : Positive)
     return Element_Access;
   --  Return an access to the Index'th element in Seq

   package Unbounded_Helper is new Sequences.Helper
     (Element          => Element,
      Element_Access   => Element_Access,
      Sequence         => Sequence,
      Length           => Length,
      New_Sequence     => New_Sequence,
      Element_Accessor => Element_Accessor,
      Element_From_Any => Element_From_Any,
      Element_To_Any   => Element_To_Any);

   ----------------------
   -- Element_Accessor --
   ----------------------

   function Element_Accessor (Seq : Sequence; Index : Positive)
     return Element_Access is
   begin
      return Seq.Content (Index)'Unrestricted_Access;
   end Element_Accessor;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any.Any) return Sequence
     renames Unbounded_Helper.From_Any;

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

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Sequence) return Any.Any
     renames Unbounded_Helper.To_Any;

end PolyORB.Sequences.Unbounded.Helper;
