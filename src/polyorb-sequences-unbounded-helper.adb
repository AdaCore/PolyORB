------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E Q U E N C E S . U N B O U N D E D . H E L P E R    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

--  Any conversion subprograms for unbounded sequences.

with PolyORB.Types;

package body PolyORB.Sequences.Unbounded.Helper is

   use PolyORB.Any;

   The_Sequence_TC : PolyORB.Any.TypeCode.Object;
   Initialized : Boolean := False;

   function From_Any (Item : Any.Any) return Sequence is
      Len : constant Types.Unsigned_Long
        := From_Any (Get_Aggregate_Element (Item, TC_Unsigned_Long, 0));
      Arr : Element_Array (1 .. Integer (Len));
   begin
      for J in Arr'Range loop
         Arr (J) := Element_From_Any
           (Get_Aggregate_Element (Item, Element_TC, Types.Unsigned_Long (J)));
      end loop;
      return To_Sequence (Arr);
   end From_Any;

   procedure Initialize is
   begin
      if Initialized then
         return;
      end if;

      The_Sequence_TC := TypeCode.TC_Sequence;

      TypeCode.Add_Parameter
        (The_Sequence_TC, To_Any (Types.Unsigned_Long'(0)));
      --  Unbounded sequence : bound is 0.

      TypeCode.Add_Parameter
        (The_Sequence_TC, To_Any (Element_TC));
      --  Element type.

      Initialized := True;
   end Initialize;

   function Sequence_TC return Any.TypeCode.Object is
   begin
      pragma Assert (Initialized);
      return The_Sequence_TC;
   end Sequence_TC;

   function To_Any   (Item : Sequence) return Any.Any is
      Arr    : constant Element_Array
        := To_Element_Array (Item);
      Result : Any.Any := Get_Empty_Any_Aggregate (Sequence_TC);
   begin
      Add_Aggregate_Element
        (Result, To_Any (Types.Unsigned_Long (Arr'Length)));

      for J in Arr'Range loop
         Add_Aggregate_Element (Result, Element_To_Any (Arr (J)));
      end loop;
      return Result;
   end To_Any;

end PolyORB.Sequences.Unbounded.Helper;
