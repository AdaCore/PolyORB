------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E Q U E N C E S . U N B O U N D E D . S E A R C H    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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
