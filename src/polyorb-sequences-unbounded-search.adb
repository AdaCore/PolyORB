------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E Q U E N C E S . U N B O U N D E D . S E A R C H    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

package body PolyORB.Sequences.Unbounded.Search is

   -----------
   -- Count --
   -----------

   function Count
     (Haystack : Sequence;
      Needle   : Needle_Type) return Natural
   is
      Times  : Natural := 0;

   begin
      for Index in 1 .. Haystack.Length loop
         if Match (Get_Element (Haystack, Index), Needle) then
            Times := Times + 1;
         end if;
      end loop;

      return Times;
   end Count;

   -----------
   -- Index --
   -----------

   function Index
     (Haystack : PolyORB.Sequences.Unbounded.Sequence;
      Needle   : Needle_Type;
      Going    : Direction := Forward) return Natural
   is
      Shift  : Integer;
      From   : Natural;
      To     : Natural;

   begin
      if Haystack.Length = 0 then
         return 0;
      end if;

      if Going = Forward then
         Shift := 1;
         From  := 1;
         To    := Haystack.Length;
      else
         Shift := -1;
         From  := Haystack.Length;
         To    := 1;
      end if;

      --  There is at least one pass because Haystack.Length /= 0

      loop
         if Match (Get_Element (Haystack, From), Needle) then
            return From;
         end if;
         exit when From = To;
         From := From + Shift;
      end loop;

      --  No match

      return 0;

   end Index;

   ------------------
   -- Sub_Sequence --
   ------------------

   function Sub_Sequence
     (Haystack : Sequence;
      Needle   : Needle_Type) return Sequence
   is
      Result : Sequence := Null_Sequence;
   begin
      for Index in 1 .. Haystack.Length loop
         declare
            El : Element renames Get_Element (Haystack, Index);
         begin
            if Match (El, Needle) then
               Append (Result, El);
            end if;
         end;
      end loop;

      return Result;
   end Sub_Sequence;

end PolyORB.Sequences.Unbounded.Search;
