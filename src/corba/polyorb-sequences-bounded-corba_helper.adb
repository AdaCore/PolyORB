------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.SEQUENCES.BOUNDED.CORBA_HELPER                   --
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

--  Any conversion subprograms for bounded sequences

package body PolyORB.Sequences.Bounded.CORBA_Helper is

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

   procedure Initialize (Element_TC, Sequence_TC : CORBA.TypeCode.Object) is
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

end PolyORB.Sequences.Bounded.CORBA_Helper;
