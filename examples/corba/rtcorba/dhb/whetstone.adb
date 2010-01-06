------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            W H E T S T O N E                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--        Copyright (C) 2006-2008, Free Software Foundation, Inc.           --
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

with Ada.Numerics.Elementary_Functions;
with Ada.Real_Time;

package body Whetstone is

   KWIPS : Positive := 1;

   ---------------------
   -- Small_Whetstone --
   ---------------------

   procedure Small_Whetstone (Kilo_Whets : Positive) is
      use Ada.Numerics.Elementary_Functions;

      T  : constant := 0.499975; --  Values from the original Algol
      T1 : constant := 0.50025;  --  Whetstone program and the
      T2 : constant := 2.0;      --  Pascal SmallWhetstone program

      N8 : constant := 10;       --  Loop iteration count for module 8
      N9 : constant :=  7;       --  Loop iteration count for module 9

      Value     : constant := 0.941377; --  Value calculated in main loop
      Tolerance : constant := 0.00001;  --  Determined by interval arithmetic

      I   : Integer;
      IJ  : Integer := 1;
      IK  : Integer := 2;
      IL  : Integer := 3;

      Y   : constant Float := 1.0; --  Constant within loop
      Z   : Float;
      Sum : Float := 0.0; --  Accumulates value of Z

      subtype Index is Integer range 1 .. N9;

      E1  : array (Index) of Float;

      --  Processing of Small_Whetstone begin here

   begin
      for Outer_Loop_Var in 1 .. Kilo_Whets loop

         --  Clear array

         for Loop_Var in E1'Range loop
            E1 (Loop_Var) := 0.0;
         end loop;

         --  Module 6: Integer arithmetic

         IJ := (IK - IJ) * (IL - IK);
         IK := IL - (IK - IJ);
         IL := (IL - IK) * (IK + IL);
         E1 (IL - 1) := Float (IJ + IK + IL);
         E1 (IK - 1) := Sin (Float (IL));

         --  Module 8: Procedure calls

         Z := E1 (4);
         for Inner_Loop_Var in 1 .. N8 loop
            declare
               Xtemp : constant Float := T * (Z + Y * Float (Inner_Loop_Var));
               Ytemp : constant Float := T * (Xtemp + Y + Z);

            begin
               Z := (Xtemp + Ytemp) / T2;
            end;
         end loop;

         --  Second version of Module 6:

         IJ := IL - (IL - 3) * IK;
         IL := (IL - IK) * (IK - IJ);
         IK := (IL - IK) * IK;
         E1 (IL - 1) := Float (IJ + IK + IL);
         E1 (IK + 1) := abs (Cos (Z));

         --  Module 9: Array references

         I := 1;
         while I <= N9 loop
            E1 (IJ) := E1 (IK);
            E1 (IK) := E1 (IL);
            E1 (I)  := E1 (IJ);

            I := I + 1;
         end loop;

         --  Module 11: Standard mathematical functions

         Z := Sqrt (Exp (Log (E1 (N9)) / T1));

         Sum := Sum + Z;

         --  Check the current value of the loop computation

         if abs (Z - Value) > Tolerance then
            Sum := 2.0 * Sum; -- Forces error at end
            IJ := IJ + 1;     -- Prevents optimization
         end if;

      end loop;

      --  Self-validation check

      if abs (Sum / Float (Kilo_Whets) - Value) >
        Tolerance * Float (Kilo_Whets)
      then
         raise Program_Error;
      end if;

   end Small_Whetstone;

   -------------------
   -- Compute_KWIPS --
   -------------------

   function Compute_KWIPS return Positive is
      use Ada.Real_Time;

      Start_Time : Ada.Real_Time.Time;
      End_Time : Ada.Real_Time.Time;

   begin
      if KWIPS /= 1 then
         return KWIPS;
      end if;

      Start_Time := Ada.Real_Time.Clock;

      for J in 1 .. 10_000 loop
         Small_Whetstone (1);
      end loop;

      End_Time := Ada.Real_Time.Clock;

      KWIPS := Positive (To_Time_Span (10_000.0) / (End_Time - Start_Time));

      return KWIPS;
   end Compute_KWIPS;

end Whetstone;
