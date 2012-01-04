------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.SEQUENCES.UNBOUNDED.CORBA_HELPER                  --
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

--  Any conversion subprograms for unbounded sequences

package body PolyORB.Sequences.Unbounded.CORBA_Helper is

   ------------------------------
   -- Element_From_Any_Wrapper --
   ------------------------------

   function Element_From_Any_Wrapper (Item : PolyORB.Any.Any) return Element is
   begin
      return Element_From_Any (CORBA.Any (Item));
   end Element_From_Any_Wrapper;

   ----------------------------
   -- Element_To_Any_Wrapper --
   ----------------------------

   function Element_To_Any_Wrapper (Item : Element) return PolyORB.Any.Any is
   begin
      return PolyORB.Any.Any (Element_To_Any (Item));
   end Element_To_Any_Wrapper;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return Sequence is
   begin
      return Neutral_Helper.From_Any (PolyORB.Any.Any (Item));
   end From_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Element_TC, Sequence_TC : CORBA.TypeCode.Object)
   is
      use CORBA.TypeCode.Internals;
   begin
      Neutral_Helper.Initialize
        (Element_TC  => To_PolyORB_Object (Element_TC),
         Sequence_TC => To_PolyORB_Object (Sequence_TC));
   end Initialize;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Sequence) return CORBA.Any is
   begin
      return CORBA.Any (Neutral_Helper.To_Any (Item));
   end To_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access Sequence) return PolyORB.Any.Content'Class
     renames Neutral_Helper.Wrap;

end PolyORB.Sequences.Unbounded.CORBA_Helper;
