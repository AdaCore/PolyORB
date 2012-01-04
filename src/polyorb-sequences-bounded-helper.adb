------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E Q U E N C E S . B O U N D E D . H E L P E R      --
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
     (Element_TC, Sequence_TC : PolyORB.Any.TypeCode.Local_Ref)
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
