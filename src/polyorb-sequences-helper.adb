------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . S E Q U E N C E S . H E L P E R              --
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

--  Any conversion subprograms for sequences

with PolyORB.Types;

package body PolyORB.Sequences.Helper is

   use PolyORB.Any;

   Initialized : Boolean := False;
   Sequence_TC, Element_TC : PolyORB.Any.TypeCode.Object;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any.Any) return Sequence is
      Len : constant Integer
        := Integer
        (Types.Unsigned_Long'
         (From_Any (Get_Aggregate_Element (Item, TC_Unsigned_Long, 0))));

      Result : Sequence := New_Sequence (Len);

   begin
      pragma Assert (Initialized);
      for J in 1 .. Len loop
         Set_Element
           (Result, J,
            Element_From_Any
              (Get_Aggregate_Element
               (Item, Element_TC, Types.Unsigned_Long (J))));
      end loop;
      return Result;
   end From_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Element_TC, Sequence_TC : PolyORB.Any.TypeCode.Object)
   is
   begin
      Helper.Element_TC  := Element_TC;
      Helper.Sequence_TC := Sequence_TC;
      Initialized := True;
   end Initialize;

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
           Element_To_Any (Get_Element (Item, J)));
      end loop;
      return Result;
   end To_Any;

end PolyORB.Sequences.Helper;
