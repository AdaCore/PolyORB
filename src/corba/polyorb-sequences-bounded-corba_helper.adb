------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E Q U E N C E S . B O U N D E D . H E L P E R      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  Any conversion subprograms for bounded sequences.

with PolyORB.Any;

package body PolyORB.Sequences.Bounded.CORBA_Helper is

   use CORBA;

   The_Element_TC, The_Sequence_TC : CORBA.TypeCode.Object;
   Initialized : Boolean := False;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return Sequence is
      Len : constant Integer
        := Integer
        (CORBA.Unsigned_Long'
         (CORBA.From_Any (CORBA.Get_Aggregate_Element
                          (Item, TC_Unsigned_Long, CORBA.Unsigned_Long'(0)))));

      Result : Sequence;

   begin
      if Len > Max then
         raise Constraint_Error;
      end if;

      Result.Length := Len;

      for J in Result.Content'First .. Result.Content'First + Len - 1 loop
         Result.Content (J) := Element_From_Any
           (Get_Aggregate_Element
            (Item, The_Element_TC,
             CORBA.Unsigned_Long (1 + J - Result.Content'First)));
      end loop;
      return Result;
   end From_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Element_TC : CORBA.TypeCode.Object) is
   begin
      if Initialized then
         return;
      end if;

      The_Element_TC  := Element_TC;
      The_Sequence_TC := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Sequence);

      TypeCode.Internals.Add_Parameter
        (The_Sequence_TC, To_Any (CORBA.Unsigned_Long (Max)));

      TypeCode.Internals.Add_Parameter
        (The_Sequence_TC, To_Any (The_Element_TC));
      --  Element type

      Initialized := True;
   end Initialize;

   -----------------
   -- Sequence_TC --
   -----------------

   function Sequence_TC return CORBA.TypeCode.Object is
   begin
      pragma Assert (Initialized);
      return The_Sequence_TC;
   end Sequence_TC;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Sequence) return CORBA.Any is
      Result : CORBA.Any := Get_Empty_Any_Aggregate (Sequence_TC);

   begin
      Add_Aggregate_Element
        (Result, To_Any (CORBA.Unsigned_Long (Item.Length)));

      for J in Item.Content'First .. Item.Content'First + Item.Length - 1 loop
         Add_Aggregate_Element (Result, Element_To_Any (Item.Content (J)));
      end loop;
      return Result;
   end To_Any;

end PolyORB.Sequences.Bounded.CORBA_Helper;
