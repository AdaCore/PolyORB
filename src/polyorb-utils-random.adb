------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . R A N D O M                  --
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

package body PolyORB.Utils.Random is

   use PolyORB.Types;

   M            : constant := 397;            -- Period parameter

   Upper_Mask   : constant := 16#80000000#;   -- Most significant w-r bits
   Lower_Mask   : constant := 16#7FFFFFFF#;   -- Least significant r bits

   type State_Access is access all State;

   -----------
   -- Magic --
   -----------

   function Magic (S : in Unsigned_Long) return Unsigned_Long;
   --  Return magic value for computation in random number generation

   function Magic (S : in Unsigned_Long) return Unsigned_Long is
   begin
      if (S and 16#1#) = 1 then
         return 16#9908B0DF#;
      else
         return 0;
      end if;
   end Magic;

   ------------
   -- Random --
   ------------

   function Random (G : in Generator) return PolyORB.Types.Unsigned_Long is
      Gen_State : constant State_Access := G.Gen_State'Unrestricted_Access;
      S : Unsigned_Long;

   begin
      if Gen_State.Condition >= N then

         if Gen_State.Condition = Invalid then

            --  The generator is not initialized

            raise Program_Error;
         end if;

         for J in 0 .. N - M - 1 loop

            S := (Gen_State.Vector_N (J) and Upper_Mask)
              or (Gen_State.Vector_N (J + 1) and Lower_Mask);

            Gen_State.Vector_N (J) := Gen_State.Vector_N (J + M)
              xor Shift_Right (S, 1) xor Magic (S);

         end loop;

         for J in N - M .. N - 2 loop

            S := (Gen_State.Vector_N (J) and Upper_Mask)
              or (Gen_State.Vector_N (J + 1) and Lower_Mask);

            Gen_State.Vector_N (J) := Gen_State.Vector_N (J + (M - N))
              xor Shift_Right (S, 1) xor Magic (S);

         end loop;

         S := (Gen_State.Vector_N (N - 1) and Upper_Mask)
           or (Gen_State.Vector_N (0) and Lower_Mask);

         Gen_State.Vector_N (N - 1) := Gen_State.Vector_N (M - 1)
           xor Shift_Right (S, 1) xor Magic (S);

         Gen_State.Condition := 0;
      end if;

      --  Tempering

      S := Gen_State.Vector_N (Gen_State.Condition);

      Gen_State.Condition := Gen_State.Condition + 1;

      S := S xor Shift_Right (S, 11);
      S := S xor (Shift_Left (S,  7) and 16#9D2C5680#);
      S := S xor (Shift_Left (S, 15) and 16#EFC60000#);
      S := S xor Shift_Right (S, 18);

      return S;
   end Random;

   -----------
   -- Reset --
   -----------

   procedure Reset (G : in Generator; Seed : in Seed_Type := Default_Seed) is
      Gen_State : constant State_Access := G.Gen_State'Unrestricted_Access;

   begin
      Gen_State.Seed := Seed;
      Gen_State.Vector_N (0) := Unsigned_Long (Gen_State.Seed)
        and 16#FFFFFFFF#;

      --  See Knuth, "The Art Of Computer Programming" (Vol2. 3rd Ed. p.106)
      --  for multiplier.

      for J in 1 .. N loop
         Gen_State.Vector_N (J)
           := 1_812_433_253
           * (Gen_State.Vector_N (J - 1)
              xor Shift_Right (Gen_State.Vector_N (J - 1), 30))
           + Unsigned_Long (J);

         Gen_State.Vector_N (J) := Gen_State.Vector_N (J) and 16#FFFFFFFF#;
      end loop;

      Gen_State.Condition := N;
   end Reset;

end PolyORB.Utils.Random;
