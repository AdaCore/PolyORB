------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . R A N D O M                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2020, Free Software Foundation, Inc.          --
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

--  A pseudo-random generator based on Makoto Matsumoto and Takuji Nishimura
--  `Mersenne Twister' random number generator MT19937.

--  Note: we cannot depend on Ada.Numerics.Discrete_Random because of
--  its wrong categorization. PolyORB components require a preelaborable
--  pseudo-random generator.

with PolyORB.Types;

package PolyORB.Utils.Random is

   pragma Preelaborate;

   type Generator is limited private;

   type Seed_Type is new PolyORB.Types.Unsigned_Long;

   Default_Seed : constant Seed_Type;

   function Random (G : in out Generator) return PolyORB.Types.Unsigned_Long;

   procedure Reset (G : in out Generator; Seed : Seed_Type := Default_Seed);

private

   N : constant := 624; -- Length of state vector

   Invalid : constant := N + 1;

   type Vector is array (0 .. N) of PolyORB.Types.Unsigned_Long;

   type State is record
      Vector_N  : Vector    := (others => 0);
      Condition : Integer   := Invalid;
      Seed      : Seed_Type := Default_Seed;
   end record;

   type Generator is limited record
      Gen_State  : State;
   end record;

   Default_Seed : constant Seed_Type := 19_650_218;

end PolyORB.Utils.Random;
