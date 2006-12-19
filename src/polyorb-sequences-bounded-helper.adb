------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E Q U E N C E S . B O U N D E D . H E L P E R      --
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

--  Any conversion subprograms for bounded sequences

package body PolyORB.Sequences.Bounded.Helper is

   use PolyORB.Any;

   ------------------
   -- Check_Length --
   ------------------

   function Check_Length (Length : Natural) return Sequence is
   begin
      if Length > Max then
         raise Constraint_Error;
      end if;
      declare
         Seq : Sequence;
      begin
         Seq.Length := Length;
         return Seq;
      end;
   end Check_Length;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any.Any) return Sequence
     renames Bounded_Helper.From_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Element_TC, Sequence_TC : PolyORB.Any.TypeCode.Object)
   is
   begin
      Bounded_Helper.Initialize
        (Element_TC => Element_TC,
         Sequence_TC => Sequence_TC);
   end Initialize;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Sequence) return Any.Any
     renames Bounded_Helper.To_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access Sequence) return Any.Content'Class
     renames Bounded_Helper.Wrap;

end PolyORB.Sequences.Bounded.Helper;
