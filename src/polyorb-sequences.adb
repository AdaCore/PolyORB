------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . S E Q U E N C E S                     --
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

--  PolyORB.Sequences is the parent of the bounded and unbounded sequence
--  packages.  Some exceptions and types common to both are declared here
--  (following the structure of Ada.Strings).
--
--  Length_Error is raised when sequence lengths are exceeded.
--  Pattern_Error is raised when a null pattern string is passed.
--  Index_Error is raised when indexes are out of range.

with Ada.Unchecked_Deallocation;

package body PolyORB.Sequences is

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (AA : in out Universal_Array_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Universal_Array, Universal_Array_Access);
   begin
      Deallocate (AA.all);
      Free (AA);
   end Deallocate;

   -------------------------
   -- Universal_Unbounded --
   -------------------------

   package body Universal_Unbounded is

      --  Local subprograms

      function Round (Length : Natural) return Natural;
      --  Compute appropriate Length. If Length = 0, return 0. If not, return
      --  Initial_Size + N * Increment_Size where N is the smallest integer
      --  such that Length < Initial_Size + N * Increment_Size.

      ------------
      -- Adjust --
      ------------

      procedure Adjust (S : in out Sequence) is
         Contents : Universal_Array_Access;
      begin
         Contents := Allocate (S.Contents.all, S.Length);
         if S.Length > 0 then
            Copy_Slice (Target_Arr => Contents.all,
                        Target_Low => 1,
                        Source_Arr => S.Contents.all,
                        Source_Low => 1,
                        length     => S.Length);
         end if;
         S.Contents := Contents;
      end Adjust;

      ------------
      -- Append --
      ------------

      procedure Append
        (Source   : in out Sequence;
         New_Item : Universal_Array)
      is
         Old_Length  : constant Natural := Source.Length;
      begin
         Reallocate (Source, Old_Length + Length (New_Item));
         Copy_Slice (Target_Arr => Source.Contents.all,
                     Target_Low => Old_Length + 1,
                     Source_Arr => New_Item,
                     Source_Low => First (New_Item),
                     Length     => Length (New_Item));
      end Append;

      -----------
      -- Count --
      -----------

      function Count_Index
        (Source  : Sequence;
         Pattern : Universal_Array;
         What    : Search_Kind;
         Going   : Direction := Forward) return Natural
      is
         Matches  : Natural := 0;
         P_Length : constant Natural := Length (Pattern);
         P_First  : constant Integer := First (Pattern);

         From, To : Positive;
         Step     : Integer;
      begin
         if P_Length = 0 then
            raise Pattern_Error;
         end if;

         if Source.Length < P_Length then
            return 0;
         end if;

         if Going = Forward then
            Step := 1;
            From := 1;
            To   := Source.Length - (P_Length - 1);
         else
            Step := -1;
            From := Source.Length - (P_Length - 1);
            To   := 1;
         end if;

         loop
            if Slice_Equals (Left_Arr  => Source.Contents.all,
                             Left_Low  => From,
                             Right_Arr => Pattern,
                             Right_Low => P_First,
                             Length    => P_Length)
            then
               if What = Return_Index then
                  return From;
               end if;
               Matches := Matches + 1;
            end if;
            exit when From = To;
            From := From + Step;
         end loop;

         return Matches;
      end Count_Index;

      ------------
      -- Delete --
      ------------

      procedure Delete
        (Source  : in out Sequence;
         From    : Positive;
         Through : Natural)
      is
         Old_Length   : constant Natural := Source.Length;
         Old_Contents : Universal_Array_Access;
         Reallocate   : Boolean;

      begin
         if Source.Length = 0 then
            return;
         end if;

         if From > Old_Length + 1 or else Through > Old_Length then
            raise Index_Error;
         end if;

         if Through < From then
            return;
         end if;

         Source.Length := Old_Length - (Through - From + 1);
         Old_Contents  := Source.Contents;
         Reallocate    := (Length (Source.Contents.all)
                           /= Round (Source.Length));

         if Reallocate then
            Source.Contents := Allocate (Source.Contents.all, Source.Length);
            Copy_Slice (Target_Arr => Source.Contents.all,
                        Target_Low => 1,
                        Source_Arr => Old_Contents.all,
                        Source_Low => 1,
                        Length     => From - 1);
         end if;

         Copy_Slice (Target_Arr => Source.Contents.all,
                     Target_Low => From,
                     Source_Arr => Old_Contents.all,
                     Source_Low => Through + 1,
                     Length     => Old_Length - Through);

         if Reallocate then
            Deallocate (Old_Contents);

         else

            --  Force finalization of remaining elements

            Set_Elements (Source.Contents.all,
                          Low   => Source.Length + 1,
                          High  => Old_Length,
                          Value => System.Null_Address);
         end if;
      end Delete;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (S : in out Sequence) is
      begin
         Deallocate (S.Contents);
      end Finalize;

      --------------
      -- Get_Head --
      --------------

      procedure Get_Head_Tail
        (Source : Sequence;
         Count  : Natural;
         Pad    : System.Address;
         Into   : in out Sequence;
         What   : Extremity)
      is
         Length    : Natural := Count;
         Target_Low, Target_High, Source_Low : Positive;
      begin
         if Source.Length < Count then
            Length := Source.Length;
         end if;

         if What = Head then
            Target_Low := 1;
            Source_Low := 1;
         else
            Target_Low := Count - Length + 1;
            Source_Low := Source.Length - Length + 1;
         end if;

         Copy_Slice (Target_Arr => Into.Contents.all,
                     Target_Low => Target_Low,
                     Source_Arr => Source.Contents.all,
                     Source_Low => Source_Low,
                     Length     => Length);

         if What = Head then
            Target_Low  := Length + 1;
            Target_High := Count;
         else
            Target_Low  := 1;
            Target_High := Count - Length;
         end if;

         Set_Elements (Into.Contents.all,
                       Low   => Target_Low,
                       High  => Target_High,
                       Value => Pad);
      end Get_Head_Tail;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Source   : in out Sequence;
         Before   : Positive;
         New_Item : Universal_Array)
      is
         Item_Length  : constant Natural := Length (New_Item);
         Old_Length   : constant Natural := Source.Length;
         Old_Contents : Universal_Array_Access;
         Reallocate   : Boolean;

      begin
         if Source.Length < Before then
            raise Index_Error;
         end if;

         Source.Length := Old_Length + Item_Length;
         Old_Contents  := Source.Contents;
         Reallocate    := (Length (Source.Contents.all)
                           /= Round (Source.Length));

         if Reallocate then
            Source.Contents := Allocate (Source.Contents.all, Source.Length);
            Copy_Slice (Target_Arr => Source.Contents.all,
                        Target_Low => 1,
                        Source_Arr => Old_Contents.all,
                        Source_Low => 1,
                        Length     => Before - 1);
         end if;

         Copy_Slice (Target_Arr => Source.Contents.all,
                     Target_Low => Before + Item_Length,
                     Source_Arr => Old_Contents.all,
                     Source_Low => Before,
                     Length     => Old_Length - Before + 1);

         Copy_Slice (Target_Arr => Source.Contents.all,
                     Target_Low => Before,
                     Source_Arr => New_Item,
                     Source_Low => First (New_Item),
                     Length     => Item_Length);

         if Reallocate then
            Deallocate (Old_Contents);
         end if;
      end Insert;

      ---------------
      -- Overwrite --
      ---------------

      procedure Overwrite
        (Source   : in out Sequence;
         Position : Positive;
         New_Item : Universal_Array)
      is
         Item_Length  : constant Natural := Length (New_Item);
         Old_Length   : constant Natural := Source.Length;
         Old_Contents : Universal_Array_Access;
         Reallocate   : Boolean;

      begin
         if Position > Source.Length + 1 then
            raise Index_Error;
         end if;

         if Position + Item_Length > Old_Length then
            Source.Length := Position + Item_Length;
         end if;

         Old_Contents := Source.Contents;
         Reallocate   := (Length (Source.Contents.all)
                          /= Round (Source.Length));

         if Reallocate then
            Source.Contents := Allocate (Source.Contents.all, Source.Length);
            Copy_Slice (Target_Arr => Source.Contents.all,
                        Target_Low => 1,
                        Source_Arr => Old_Contents.all,
                        Source_Low => 1,
                        length     => Position - 1);
            Deallocate (Old_Contents);
         end if;

         Copy_Slice (Target_Arr => Source.Contents.all,
                     Target_Low => Position,
                     Source_Arr => New_Item,
                     Source_Low => First (New_Item),
                     Length     => Item_Length);
      end Overwrite;

      ----------------
      -- Reallocate --
      ----------------

      procedure Reallocate
        (Source     : in out Sequence;
         New_Length : Natural)
      is
         Old_Contents : Universal_Array_Access := Source.Contents;
         Old_Length   : constant Natural := Source.Length;

         Min_Length  : Natural;
         --  Count of elements in the new allocation that need to be copied
         --  from the old one.

      begin
         if New_Length = 0 then
            Sequence'Class (Source) := Null_Sequence;
            --  Force dispatching call to abstract constructor
            return;
         end if;

         if Source.Length > New_Length then
            Min_Length := New_Length;
         else
            Min_Length := Source.Length;
         end if;

         Source.Length := New_Length;

         if Length (Source.Contents.all) /= Round (New_Length) then
            Source.Contents := Allocate (Source.Contents.all, New_Length);

            Copy_Slice (Target_Arr => Source.Contents.all,
                        Target_Low => 1,
                        Source_Arr => Old_Contents.all,
                        Source_Low => 1,
                        Length     => Min_Length);
            Deallocate (Old_Contents);

         else

            --  Force finalization, if we have shrunk the list

            Set_Elements
              (Source.Contents.all,
               Low   => Min_Length + 1,
               High  => Old_Length,
               Value => System.Null_Address);
         end if;
      end Reallocate;

      ------------
      -- Repeat --
      ------------

      procedure Repeat
        (Item : Universal_Array;
         Into : in out Sequence)
      is
         I_Length : constant Natural := Length (Item);
         Index  : Positive := 1;
      begin
         if I_Length = 0 then
            return;
         end if;
         while Index < Into.Length loop
            Copy_Slice (Target_Arr => Into.Contents.all,
                        Target_Low => Index,
                        Source_Arr => Item,
                        Source_Low => 1,
                        Length     => I_Length);
            Index := Index + I_Length;
         end loop;
      end Repeat;

      -------------------
      -- Replace_Slice --
      -------------------

      procedure Replace_Slice
        (Source   : in out Sequence;
         Low      : Positive;
         High     : Natural;
         By       : Universal_Array)
      is
         By_Length    : constant Natural := Length (By);
         Old_Length   : constant Natural := Source.Length;
         Old_Contents : Universal_Array_Access;
         Reallocate   : Boolean;

      begin
         if Low > Old_Length + 1
           or else High > Old_Length
         then
            raise Index_Error;
         end if;

         if High < Low then
            Insert (Source => Source, Before => Low, New_Item => By);
            return;
         end if;

         Source.Length := Low - 1 + By_Length + Source.Length - High;
         Old_Contents  := Source.Contents;
         Reallocate    := (Length (Source.Contents.all)
                           /= Round (Source.Length));

         if Reallocate then
            Source.Contents := Allocate (Source.Contents.all, Source.Length);
            Copy_Slice (Target_Arr => Source.Contents.all,
                        Target_Low => 1,
                        Source_Arr => Old_Contents.all,
                        Source_Low => 1,
                        Length     => Low - 1);
         end if;

         Copy_Slice (Target_Arr => Source.Contents.all,
                     Target_Low => Low + By_Length,
                     Source_Arr => Old_Contents.all,
                     Source_Low => High + 1,
                     Length     => Old_Length - High);

         Copy_Slice (Target_Arr => Source.Contents.all,
                     Target_Low => Low,
                     Source_Arr => By,
                     Source_Low => First (By),
                     Length     => By_Length);

         if Reallocate then
            Deallocate (Old_Contents);

         else

            --  Force finalization of remaining elements

            Set_Elements
              (Source.Contents.all,
               Low   => Old_Length + 1,
               High  => Source.Length,
               Value => System.Null_Address);
         end if;
      end Replace_Slice;

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

   end Universal_Unbounded;

end PolyORB.Sequences;
