------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2009, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Report;

procedure Test000 is

   use PolyORB.Utils.Report;

   package Ls is new PolyORB.Utils.Chained_Lists
     (Integer, Doubly_Chained => True);
   use Ls;
   type A is array (Integer range <>) of Integer;

   function To_Array (L : List) return A;
   function To_Array (L : List) return A is
      Result : A (1 .. Length (L));
      Index : Integer := Result'First;
      It : Iterator := First (L);
   begin
      while not Last (It) loop
         Result (Index) := Value (It).all;
         Index := Index + 1;
         Next (It);
      end loop;

      if Index /= Result'Last + 1 then
         raise Program_Error;
      end if;
      return Result;
   end To_Array;

   L1, L2, L3, L4 : List;
   Int : Integer;
   It  : Iterator;
begin
   Output ("empty", To_Array (L1)'Length = 0);
   Append (L1, 123);
   Output ("single L1", To_Array (L1) = (1 => 123));
   Prepend (L2, 456);
   Output ("single L2", To_Array (L2) = (1 => 456));

   Prepend (L1, 666);
   Output ("double L1", To_Array (L1) = (1 => 666, 2 => 123));
   Append (L2, 789);
   Output ("double L2", To_Array (L2) = (1 => 456, 2 => 789));

   L3 := L1 & 999 & 456 & 789;
   --  L3 is now a copy of L1

   Output ("concat", To_Array (L3) = (666, 123, 999, 456, 789));

   It := First (L3);
   while not (Last (It) or else Value (It).all = 999) loop
      Next (It);
   end loop;

   Output ("scan", Value (It).all = 999);
   Remove (L3, It);
   --  Note, L1 now has dangling pointers

   Output ("remove", To_Array (L3) = (666, 123, 456, 789));
   Output ("remove iterator", Value (It).all = 456);

   L4 := Duplicate (L3);
   Element (L4, 1).all := 321;
   Append (L4, 555);
   Output ("duplicate", To_Array (L3) = (666, 123, 456, 789)
               and then To_Array (L4) = (666, 321, 456, 789, 555));

   Extract_First (L4, Int);
   Output ("extract first",
     Int = 666 and then To_Array (L4) = (321, 456, 789, 555));

   declare
      function Range_400_499 (X : Integer) return Boolean;
      function Range_400_499 (X : Integer) return Boolean is
      begin
         return X in 400 .. 499;
      end Range_400_499;
      procedure Remove is new Ls.Remove_G (Range_400_499);
   begin
      Remove (L3, All_Occurrences => True);
      Output ("remove multiple", To_Array (L3) = (666, 123, 789));
   end;

   --  Deallocate (L1);
   --  Copied into L3, and then elements removed from L3

   Deallocate (L2);
   Deallocate (L3);
   Deallocate (L4);
   End_Report;
end Test000;
