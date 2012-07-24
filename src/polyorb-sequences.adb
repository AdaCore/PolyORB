------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . S E Q U E N C E S                     --
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

--  PolyORB.Sequences is the parent of the bounded and unbounded sequence
--  packages.  Some exceptions and types common to both are declared here
--  (following the structure of Ada.Strings).
--
--  Length_Error is raised when sequence lengths are exceeded.
--  Pattern_Error is raised when a null pattern string is passed.
--  Index_Error is raised when indexes are out of range.

with System;

package body PolyORB.Sequences is

   --  Constants for unbounded sequences allocation

   Initial_Size   : constant := 3;
   Increment_Size : constant := 2;

   procedure Check_Length
     (Max_Length : Natural;
      Length     : Natural;
      Drop       : Truncation := Error);
   --  Raise Length_Error if Max_Length is non-zero and Length > Max_Length
   --  and Drop = Error
   --  Otherwise do nothing

   procedure Push (Prog : in out Program; A : Assignment);
   --  Append A as the last operation in P

   procedure Adjust_For_Max_Length
     (Prog       : in out Program;
      Max_Length : Natural;
      Drop       : Truncation);
   --  Adjust the assignments in Prog for a bounded sequence of maximum
   --  length Max_Length, according to the indicated truncation policy.
   --  For an unbounded sequence, this subprogram may be called with a
   --  zero Max_Length parameter, in which case it returns immediately,
   --  leaving Prog unchanged.

   ---------------------------
   -- Adjust_For_Max_Length --
   ---------------------------

   procedure Adjust_For_Max_Length
     (Prog       : in out Program;
      Max_Length : Natural;
      Drop       : Truncation)
   is
      Drop_Length : Natural;
   begin
      if Max_Length = 0 or else Prog.Result_Length <= Max_Length then
         return;
      end if;
      Drop_Length := Prog.Result_Length - Max_Length;
      Prog.Result_Length := Max_Length;

      for PC in 0 .. Prog.Last loop
         declare
            A : Assignment renames Prog.Assignments (PC);
         begin
            case Drop is
               when Left =>
                  A.Target_Bounds.Lo := A.Target_Bounds.Lo - Drop_Length;
                  A.Target_Bounds.Hi := A.Target_Bounds.Hi - Drop_Length;

                  --  Case of an assignment that is entirely dropped
                  --  (this assumes that at program execution time,
                  --  Target'First is always 1 -- this is checked in Run).

                  if A.Target_Bounds.Hi < 1 then
                     A.Target_Bounds.Lo := 1;
                  end if;

                  if A.Target_Bounds.Lo < 1 then
                     --  If Source is not replicated, adjust its bounds

                     if Length (A.Target_Bounds)
                          = Length (A.Source_Bounds)
                     then
                        A.Source_Bounds.Lo :=
                          A.Source_Bounds.Lo + 1 - A.Target_Bounds.Lo;
                     end if;

                     A.Target_Bounds.Lo := 1;
                  end if;

               when Right =>
                  if A.Target_Bounds.Hi > Max_Length then

                     --  Adjust source bounds if the source is not to be
                     --  replicated.

                     if A.Target_Bounds.Lo <= Max_Length
                       and then Length (A.Target_Bounds)
                              = Length (A.Source_Bounds)
                     then
                        A.Source_Bounds.Hi :=
                          A.Source_Bounds.Hi - (A.Target_Bounds.Hi
                                                  - Max_Length);
                     end if;

                     --  Adjust target bounds in all cases

                     A.Target_Bounds.Hi := Max_Length;
                  end if;

               when Error =>

                  --  Already dealt with earlier, never reached

                  raise Program_Error;
            end case;

            --  Check that we did not generate an invalid program

            pragma Assert
              (Length (A.Target_Bounds) = 0
                 or else
               Length (A.Target_Bounds) mod Length (A.Source_Bounds) = 0);
         end;
      end loop;
   end Adjust_For_Max_Length;

   ------------------
   -- Check_Length --
   ------------------

   procedure Check_Length
     (Max_Length : Natural;
      Length     : Natural;
      Drop       : Truncation := Error)
   is
   begin
      if Max_Length > 0 and then Length > Max_Length and then Drop = Error then
         raise Length_Error;
      end if;
   end Check_Length;

   -----------------
   -- Count_Index --
   -----------------

   function Count_Index
     (Check_Slice : Check_Slice_Function;
      Source      : Bounds;
      Pattern     : Bounds;
      What        : Search_Kind;
      Going       : Direction := Forward) return Natural
   is
      S_Length : constant Natural := Length (Source);
      P_Length : constant Natural := Length (Pattern);

      Matches  : Natural := 0;
      From, To : Positive;
      Next     : Natural;
      Step     : Integer;
   begin
      if P_Length = 0 then
         raise Pattern_Error;
      end if;

      if S_Length < P_Length then
         return 0;
      end if;

      if Going = Forward then
         Step := 1;
         From := 1;
         To   := S_Length - (P_Length - 1);
      else
         Step := -1;
         From := S_Length - (P_Length - 1);
         To   := 1;
      end if;

      loop
         if Check_Slice (From, From + P_Length - 1) then
            if What = Return_Index then
               return From;
            else
               Matches := Matches + 1;
            end if;
            Next := From + Step * P_Length;
         else
            Next := From + Step;
         end if;
         exit when (Going = Forward and then Next > To)
           or else (Going = Backward and then Next < To);
         From := Next;
      end loop;
      return Matches;
   end Count_Index;

   -----------
   -- Round --
   -----------

   function Round (Length : Natural) return Natural is
      Times : Natural;
   begin
      if Length = 0 then
         return 0;

      elsif Length <= Initial_Size then
         return Initial_Size;

      else
         Times := ((Length - Initial_Size) / Increment_Size) + 1;
         return Initial_Size + (Increment_Size * Times);
      end if;
   end Round;

   ---------------
   -- Head_Tail --
   ---------------

   function Head_Tail
     (Max_Length       : Natural;
      Source           : Bounds;
      Count            : Natural;
      Drop             : Truncation := Error;
      What             : Extremity;
      Suppress_Padding : Boolean := False) return Program
   is
      Prog : Program;

      Source_Length : constant Natural := Length (Source);
      Copy_Length : Natural;
      Target_Low, Target_High, Source_Low, Source_High : Integer;
   begin
      Check_Length (Max_Length, Count, Drop);

      Prog.Result_Length := Count;

      --  Copy requested elements

      if Source_Length < Count then
         Copy_Length := Source_Length;
      else
         Copy_Length := Count;
      end if;

      if What = Head then
         Target_Low := 1;
         Source_Low := 1;
      else
         Target_Low := Count - Copy_Length + 1;
         Source_Low := Source_Length - Copy_Length + 1;
      end if;

      Source_High := Source_Low + Copy_Length - 1;
      Target_High := Target_Low + Copy_Length - 1;

      Push (Prog,
        (Source => Left,
         Target_Bounds => (Target_Low, Target_High),
         Source_Bounds => (Source_Low, Source_High)));

      --  Add padding for remaining elements, unless suppressed

      if not Suppress_Padding then
         if What = Head then
            Target_Low  := Copy_Length + 1;
            Target_High := Count;
         else
            Target_Low  := 1;
            Target_High := Count - Copy_Length;
         end if;

         Push (Prog,
           (Source => Right,
            Target_Bounds => (Target_Low, Target_High),
            Source_Bounds => (1, 1)));
      end if;

      --  Adjust for bounded case

      Adjust_For_Max_Length (Prog, Max_Length, Drop);

      return Prog;
   end Head_Tail;

   ------------
   -- Length --
   ------------

   function Length (Index_Range : Bounds) return Natural is
   begin
      if Index_Range.Hi < Index_Range.Lo then
         return 0;
      else
         return Index_Range.Hi - Index_Range.Lo + 1;
      end if;
   end Length;

   ----------
   -- Push --
   ----------

   procedure Push (Prog : in out Program; A : Assignment) is
   begin
      --  No need to add an operation that has no effect

      if Length (A.Target_Bounds) = 0 then
         return;
      end if;

      Prog.Last := Prog.Last + 1;
      Prog.Assignments (Prog.Last) := A;
   end Push;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Max_Length : Natural;
      Source     : Bounds;
      Slice      : Bounds;
      By         : Bounds;
      Drop       : Truncation := Error) return Program
   is
      Prog : Program;

      Old_Length   : constant Natural := Length (Source);
      Slice_Length : Natural := Length (Slice);
      By_Length    : constant Natural := Length (By);

      Low  : Positive renames Slice.Lo;
      High : Natural  renames Slice.Hi;
   begin
      if Low > Source.Hi + 1 or else High < Source.Lo - 1 then
         raise Index_Error;
      end if;

      --  Slice.Hi may be out of Source's range, in which case we need to
      --  normalize Slice_Length.

      if Slice.Hi > Source.Hi then
         Slice_Length := Slice_Length - (Slice.Hi - Source.Hi);
      end if;

      Check_Length (Max_Length, Old_Length + By_Length - Slice_Length, Drop);

      Prog.Result_Length := Old_Length + By_Length - Slice_Length;

      Push (Prog,
        Assignment'(
          Source => Left,
          Target_Bounds => (1, Low - 1),
          Source_Bounds => (1, Low - 1)));

      Push (Prog,
        Assignment'(Source => Left,
                    Target_Bounds => (Low + By_Length, Prog.Result_Length),
                    Source_Bounds => (Low + Slice_Length, Old_Length)));

      if By_Length > 0 then
         Push (Prog,
           Assignment'(
             Source => Right,
             Target_Bounds => (Low, Low + By_Length - 1),
             Source_Bounds => By));
      end if;

      Adjust_For_Max_Length (Prog, Max_Length, Drop);
      return Prog;
   end Replace_Slice;

   ---------------
   -- Replicate --
   ---------------

   function Replicate
     (Max_Length : Natural;
      Count      : Natural;
      Item       : Bounds;
      Drop       : Truncation := Error) return Program
   is
      Prog : Program;

      Total_Length : Natural := Count * Length (Item);

      Integral_Count  : Natural;
      Integral_Bounds : Bounds;
      --  Bounds of the slice of the target that is to be filled with integral
      --  copies of Item.

      Fraction_Target_Bounds : Bounds;
      --  Bounds of the slice of the target that is to be filled with a
      --  fraction if Item.

      Fraction_Source_Bounds : Bounds;
      --  Bounds of the corresponding Item slice

   begin
      Check_Length (Max_Length, Total_Length, Drop);

      if Max_Length > 0 and then Total_Length > Max_Length then
         Total_Length := Max_Length;
      end if;

      --  Case of replicating an element array of zero length: return an empty
      --  sequence.

      if Length (Item) = 0 then
         Prog.Result_Length := 0;
         return Prog;
      end if;

      Integral_Count := Total_Length / Length (Item);

      --  Here we cannot just generate one (replicated) assignment of item
      --  into target, because we might require a truncated copy.

      --  First compute the integral copies bounds

      Integral_Bounds.Lo := 1;
      Integral_Bounds.Hi := Length (Item) * Integral_Count;

      --  In the case of a bounded sequence, we might need to generate a
      --  copy of a fragment of Item.

      if Max_Length > 0 and then Integral_Bounds.Hi < Total_Length then

         if Drop = Left then
            --  In the Drop = Left case, the integral copies are at the end,
            --  not at the beginning, so shift them.

            Integral_Bounds.Lo := Total_Length - Integral_Bounds.Hi + 1;
            Integral_Bounds.Hi := Total_Length;

            Fraction_Target_Bounds := (1, Integral_Bounds.Lo - 1);
            Fraction_Source_Bounds :=
              (Item.Hi - Length (Fraction_Target_Bounds) + 1, Item.Hi);
         else
            Fraction_Target_Bounds := (Integral_Bounds.Hi + 1, Max_Length);
            Fraction_Source_Bounds :=
              (Item.Lo, Item.Lo + Length (Fraction_Target_Bounds) - 1);
         end if;

         Push (Prog, Assignment'(
           Source => Left,
           Target_Bounds => Fraction_Target_Bounds,
           Source_Bounds => Fraction_Source_Bounds));
      end if;

      Push (Prog, Assignment'(
        Source => Left,
        Target_Bounds => Integral_Bounds,
        Source_Bounds => Item));

      Prog.Result_Length := Total_Length;
      return Prog;
   end Replicate;

   ---------
   -- Run --
   ---------

   procedure Run
     (Prog : Program;
      Target : out Element_Array;
      Left   : Element_Array;
      Right  : Element_Array)
   is
      use type System.Address;
      In_Place : constant Boolean := Target'Address = Left'Address;

      procedure Assign
        (Source        : Element_Array;
         Source_Bounds : Bounds;
         Target_Bounds : Bounds);
      --  Assign the slice of Source defined by Source_Bounds into the slice
      --  of Target defined by Target_Bounds, replicating the source slice
      --  if necessary.

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Source        : Element_Array;
         Source_Bounds : Bounds;
         Target_Bounds : Bounds)
      is
         Source_Len_Minus_1 : constant Natural :=
           Source_Bounds.Hi - Source_Bounds.Lo;

         Target_Lo : Integer := Target_Bounds.Lo;
         Target_Lo_Last : constant Integer :=
           Target_Bounds.Hi - Source_Len_Minus_1;
      begin
         --  Check that we do not leave any element of the target unassigned

         pragma Assert (Sequences.Length (Target_Bounds)
                        mod Sequences.Length (Source_Bounds) = 0);

         --  Perform as many assignments of the source slice as necessary into
         --  the target.

         while Target_Lo <= Target_Lo_Last loop
            Target (Target_Lo .. Target_Lo + Source_Len_Minus_1) :=
              Source (Source_Bounds.Lo .. Source_Bounds.Hi);
            Target_Lo := Target_Lo + Source_Len_Minus_1 + 1;
         end loop;
      end Assign;

   --  Start of processing for Run

   begin
      pragma Assert (Target'First = 1);

      for PC in 0 .. Prog.Last loop

         declare
            A : Assignment renames Prog.Assignments (PC);
         begin
            if A.Target_Bounds.Lo <= A.Target_Bounds.Hi then
               case A.Source is
                  when Sequences.Left =>
                     if not (In_Place
                             and then A.Target_Bounds = A.Source_Bounds)
                     then
                        Assign (Left, A.Source_Bounds, A.Target_Bounds);
                     end if;

                  when Sequences.Right =>
                     Assign (Right, A.Source_Bounds, A.Target_Bounds);
               end case;
            end if;
         end;
      end loop;
   end Run;

end PolyORB.Sequences;
