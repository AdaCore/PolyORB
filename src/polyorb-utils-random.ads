------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . R A N D O M                  --
--                                                                          --
--                                 S p e c                                  --
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

--  A pseudo-random generator based on Makoto Matsumoto and Takuji
--  Nishimura `Mersenne Twister' random number generator MT19937.

--  Note: we cannot depend on Ada.Numerics.Discrete_Random because of
--  its wrong categorisation. PolyORB components require a package
--  that provides pseudo-random generator and that is compliant with
--  'pragma Preelaobrate'.

with PolyORB.Types;

package PolyORB.Utils.Random is

   pragma Preelaborate;

   type Generator is limited private;

   type Seed_Type is new PolyORB.Types.Unsigned_Long;

   Default_Seed : constant Seed_Type;

   function Random (G : access Generator) return PolyORB.Types.Unsigned_Long;

   procedure Reset (G : access Generator; Seed : Seed_Type := Default_Seed);

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
