------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . R A N D O M                  --
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

package body PolyORB.Utils.Random is

   use PolyORB.Types;

   M            : constant := 397;            -- Period parameter

   Upper_Mask   : constant := 16#80000000#;   -- Most significant w-r bits
   Lower_Mask   : constant := 16#7FFFFFFF#;   -- Least significant r bits

   -----------
   -- Magic --
   -----------

   function Magic (S : Unsigned_Long) return Unsigned_Long;
   --  Return magic value for computation in random number generation

   function Magic (S : Unsigned_Long) return Unsigned_Long is
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

   function Random (G : access Generator) return PolyORB.Types.Unsigned_Long is
      S : Unsigned_Long;

   begin
      if G.Gen_State.Condition >= N then

         if G.Gen_State.Condition = Invalid then

            --  The generator is not initialized

            raise Program_Error;
         end if;

         for J in 0 .. N - M - 1 loop

            S := (G.Gen_State.Vector_N (J) and Upper_Mask)
              or (G.Gen_State.Vector_N (J + 1) and Lower_Mask);

            G.Gen_State.Vector_N (J) := G.Gen_State.Vector_N (J + M)
              xor Shift_Right (S, 1) xor Magic (S);

         end loop;

         for J in N - M .. N - 2 loop

            S := (G.Gen_State.Vector_N (J) and Upper_Mask)
              or (G.Gen_State.Vector_N (J + 1) and Lower_Mask);

            G.Gen_State.Vector_N (J) := G.Gen_State.Vector_N (J + (M - N))
              xor Shift_Right (S, 1) xor Magic (S);

         end loop;

         S := (G.Gen_State.Vector_N (N - 1) and Upper_Mask)
           or (G.Gen_State.Vector_N (0) and Lower_Mask);

         G.Gen_State.Vector_N (N - 1) := G.Gen_State.Vector_N (M - 1)
           xor Shift_Right (S, 1) xor Magic (S);

         G.Gen_State.Condition := 0;
      end if;

      --  Tempering

      S := G.Gen_State.Vector_N (G.Gen_State.Condition);

      G.Gen_State.Condition := G.Gen_State.Condition + 1;

      S := S xor Shift_Right (S, 11);
      S := S xor (Shift_Left (S,  7) and 16#9D2C5680#);
      S := S xor (Shift_Left (S, 15) and 16#EFC60000#);
      S := S xor Shift_Right (S, 18);

      return S;
   end Random;

   -----------
   -- Reset --
   -----------

   procedure Reset (G : access Generator; Seed : Seed_Type := Default_Seed)
   is
   begin
      G.Gen_State.Seed := Seed;
      G.Gen_State.Vector_N (0) := Unsigned_Long (G.Gen_State.Seed)
        and 16#FFFFFFFF#;

      --  See Knuth, "The Art Of Computer Programming" (Vol2. 3rd Ed. p.106)
      --  for multiplier.

      for J in 1 .. N loop
         G.Gen_State.Vector_N (J)
           := 1_812_433_253
           * (G.Gen_State.Vector_N (J - 1)
              xor Shift_Right (G.Gen_State.Vector_N (J - 1), 30))
           + Unsigned_Long (J);

         G.Gen_State.Vector_N (J) := G.Gen_State.Vector_N (J) and 16#FFFFFFFF#;
      end loop;

      G.Gen_State.Condition := N;
   end Reset;

end PolyORB.Utils.Random;
