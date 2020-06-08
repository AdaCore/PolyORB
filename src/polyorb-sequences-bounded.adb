------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . S E Q U E N C E S . B O U N D E D             --
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

pragma Ada_2012;

with Ada.Unchecked_Deallocation;

package body PolyORB.Sequences.Bounded is

   ----------------------------------------------
   -- SVM implementation for bounded sequences --
   ----------------------------------------------

   procedure Run is new Sequences.Run (Element, Element_Array);
   --  Core execution engine

   function Run_Copy
     (Prog  : Program;
      Left  : Element_Array;
      Right : Element_Array := Null_Element_Array) return Sequence;
   --  Execute Prog and return a new sequence containing the result

   procedure Run_In_Place
     (Prog  : Program;
      Left  : in out Sequence;
      Right : Element_Array := Null_Element_Array);
   --  Execute Prog in-place on Left's storage

   -----------------------
   -- Local subprograms --
   -----------------------

   function Append
     (Left  : Element_Array;
      Right : Element_Array;
      Drop  : Truncation := Error) return Sequence;
   --  Return Left & Right

   function Count_Index
     (Source  : Sequence;
      Pattern : Element_Array;
      What    : Search_Kind;
      Going   : Direction := Forward) return Natural;
   --  Common subprogram used to implement Count and Index, depending on
   --  the What parameter.

   function Array_Bounds (A : Element_Array) return Bounds;
   --  Return (A'First, A'Last)

   function Sequence_Bounds (S : Sequence) return Bounds;
   --  Return (1, Length (S))

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Sequence) return Sequence is
   begin
      return Append (Left, Right, Drop => Error);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Sequence; Right : Element_Array) return Sequence is
   begin
      return Append (Left, Right, Drop => Error);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Element_Array; Right : Sequence) return Sequence is
   begin
      return Append (Left, Right, Drop => Error);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Sequence; Right : Element) return Sequence is
   begin
      return Append (Left, Right, Drop => Error);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Element; Right : Sequence) return Sequence is
   begin
      return Append (Left, Right, Drop => Error);
   end "&";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Element) return Sequence is
   begin
      return Replicate (Left, Right, Drop => Error);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Element_Array) return Sequence is
   begin
      return Replicate (Left, Right, Drop => Error);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Sequence) return Sequence is
   begin
      return Replicate (Left, Right, Drop => Error);
   end "*";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Sequence) return Boolean is
      L : Natural renames Left.Length;
   begin
      return L = Right.Length
        and then Left.Content (1 .. L) = Right.Content (1 .. L);
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : Sequence; Right : Element_Array) return Boolean is
   begin
      return Left.Length = Right'Length and then
        Left.Content (1 .. Left.Length) = Right;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : Element_Array; Right : Sequence) return Boolean is
   begin
      return Left'Length = Right.Length and then
        Left = Right.Content (1 .. Right.Length);
   end "=";

   ------------
   -- Append --
   ------------

   function Append
     (Left  : Element_Array;
      Right : Element_Array;
      Drop  : Truncation := Error) return Sequence
   is
      Left_Bounds : constant Bounds := Array_Bounds (Left);
   begin
      --  Replace a null-length slice of Left located right after the last
      --  element with Right.

      return Run_Copy
        (Prog  =>
           Replace_Slice
             (Max_Length,
              Left_Bounds,
              Bounds'(Left_Bounds.Hi + 1, Left_Bounds.Hi),
              Array_Bounds (Right),
              Drop),
         Left  => Left,
         Right => Right);
   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left, Right : Sequence;
      Drop        : Truncation := Error) return Sequence is
   begin
      return Append
        (Left.Content (1 .. Left.Length), Right.Content (1 .. Right.Length),
         Drop);
   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left  : Sequence;
      Right : Element_Array;
      Drop  : Truncation := Error) return Sequence is
   begin
      return Append (Left.Content (1 .. Left.Length), Right, Drop);
   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left  : Element_Array;
      Right : Sequence;
      Drop  : Truncation := Error) return Sequence is
   begin
      return Append (Left, Right.Content (1 .. Right.Length), Drop);
   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left  : Sequence;
      Right : Element;
      Drop  : Truncation := Error) return Sequence is
   begin
      return Append (Left.Content (1 .. Left.Length),
                     Element_Array'(1 => Right),
                     Drop);
   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left  : Element;
      Right : Sequence;
      Drop  : Truncation := Error) return Sequence
   is
   begin
      return Append (Element_Array'(1 => Left),
                     Right.Content (1 .. Right.Length),
                     Drop);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : Element_Array;
      Drop     : Truncation := Error)
   is
      Left_Bounds : constant Bounds := Sequence_Bounds (Source);
   begin
      Run_In_Place
        (Prog  =>
           Replace_Slice
             (Max_Length,
              Left_Bounds,
              Bounds'(Left_Bounds.Hi + 1, Left_Bounds.Hi),
              Array_Bounds (New_Item),
              Drop),
         Left  => Source,
         Right => New_Item);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : Sequence;
      Drop     : Truncation := Error)
   is
   begin
      Append (Source, New_Item.Content (1 .. New_Item.Length), Drop);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : Element;
      Drop     : Truncation := Error) is
   begin
      Append (Source, Element_Array'(1 => New_Item), Drop);
   end Append;

   ------------------
   -- Array_Bounds --
   ------------------

   function Array_Bounds (A : Element_Array) return Bounds is
   begin
      return (Lo => A'First, Hi => A'Last);
   end Array_Bounds;

   -----------
   -- Count --
   -----------

   function Count
     (Source : Sequence;
      Pattern : Element_Array) return Natural is
   begin
      return Count_Index (Source, Pattern, Return_Count, Forward);
   end Count;

   -----------------
   -- Count_Index --
   -----------------

   function Count_Index
     (Source  : Sequence;
      Pattern : Element_Array;
      What    : Search_Kind;
      Going   : Direction := Forward) return Natural
   is
      function Check_For_Pattern (Lo, Hi : Positive) return Boolean;

      function Check_For_Pattern (Lo, Hi : Positive) return Boolean is
      begin
         return Source.Content (Lo .. Hi) = Pattern;
      end Check_For_Pattern;

   begin
      return Sequences.Count_Index
        (Check_Slice => Check_For_Pattern'Unrestricted_Access,
         Source      => Sequence_Bounds (Source),
         Pattern     => Array_Bounds (Pattern),
         What        => What,
         Going       => Going);
   end Count_Index;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Source  : in out Sequence;
      From    : Positive;
      Through : Natural) is
   begin
      Replace_Slice
        (Source,
         Low  => From,
         High => Through,
         By   => Null_Element_Array);
   end Delete;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Sequence;
      From    : Positive;
      Through : Natural) return Sequence
   is
   begin
      return Replace_Slice
        (Source,
         Low  => From,
         High => Through,
         By   => Null_Element_Array);
   end Delete;

   ----------------
   -- Element_Of --
   ----------------

   function Element_Of (Source : Sequence; Index : Positive) return Element is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;

      return Source.Content (Index);
   end Element_Of;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Element_Array_Access) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);
   begin
      Deallocate (X);
   end Free;

   ----------
   -- Head --
   ----------

   procedure Head
     (Source : in out Sequence;
      Count  : Natural;
      Pad    : Element;
      Drop   : Truncation := Error)
   is
   begin
      Run_In_Place
        (Prog => Head_Tail
           (Max_Length,
            Sequence_Bounds (Source),
            Count,
            Drop,
            Head),
         Left  => Source,
         Right => Element_Array'(1 => Pad));
   end Head;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Sequence;
      Count  : Natural;
      Pad    : Element;
      Drop   : Truncation := Error) return Sequence
   is
   begin
      return Run_Copy
        (Prog => Head_Tail
           (Max_Length,
            Sequence_Bounds (Source),
            Count,
            Drop,
            Head),
         Left  => Source.Content (1 .. Source.Length),
         Right => Element_Array'(1 => Pad));
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Sequence;
      Pattern : Element_Array;
      Going   : Direction := Forward) return Natural is
   begin
      return Count_Index (Source, Pattern, Return_Index, Going);
   end Index;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Sequence;
      Before   : Positive;
      New_Item : Element_Array;
      Drop     : Truncation := Error) return Sequence
   is
   begin
      return Replace_Slice
        (Source,
         Low  => Before,
         High => Before - 1,
         By   => New_Item,
         Drop => Drop);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Source   : in out Sequence;
      Before   : Positive;
      New_Item : Element_Array;
      Drop     : Truncation := Error)
   is
   begin
      Replace_Slice
        (Source,
         Low  => Before,
         High => Before - 1,
         By   => New_Item,
         Drop => Drop);
   end Insert;

   ------------
   -- Length --
   ------------

   function Length (Source : Sequence) return Length_Range is
   begin
      return Source.Length;
   end Length;

   ---------------
   -- Overwrite --
   ---------------

   procedure Overwrite
     (Source   : in out Sequence;
      Position : Positive;
      New_Item : Element_Array;
      Drop     : Truncation := Error)
   is
   begin
      Replace_Slice
        (Source => Source,
         Low    => Position,
         High   => Position + New_Item'Length - 1,
         By     => New_Item,
         Drop   => Drop);
   end Overwrite;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Sequence;
      Position : Positive;
      New_Item : Element_Array;
      Drop     : Truncation := Error) return Sequence
   is
   begin
      return Replace_Slice
        (Source,
         Low  => Position,
         High => Position + New_Item'Length - 1,
         By   => New_Item,
         Drop => Drop);
   end Overwrite;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in out Sequence;
      Index  : Positive;
      By     : Element) is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;

      Source.Content (Index) := By;
   end Replace_Element;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : Sequence;
      Low    : Positive;
      High   : Natural;
      By     : Element_Array;
      Drop   : Truncation := Error) return Sequence
   is
   begin
      return Run_Copy
        (Prog  =>
           Replace_Slice
             (Max_Length,
              Sequence_Bounds (Source),
              Bounds'(Low, High),
              Array_Bounds (By),
              Drop),
         Left  => Source.Content (1 .. Source.Length),
         Right => By);
   end Replace_Slice;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Source : in out Sequence;
      Low    : Positive;
      High   : Natural;
      By     : Element_Array;
      Drop   : Truncation := Error)
   is
   begin
      Run_In_Place
        (Prog  =>
           Replace_Slice
             (Max_Length,
              Sequence_Bounds (Source),
              Bounds'(Low, High),
              Array_Bounds (By),
              Drop),
         Left  => Source,
         Right => By);
   end Replace_Slice;

   ---------------
   -- Replicate --
   ---------------

   function Replicate
     (Count : Natural;
      Item  : Element;
      Drop  : Truncation := Error) return Sequence
   is
   begin
      return Replicate (Count, Element_Array'(1 => Item), Drop);
   end Replicate;

   ---------------
   -- Replicate --
   ---------------

   function Replicate
     (Count : Natural;
      Item  : Element_Array;
      Drop  : Truncation := Error) return Sequence
   is
   begin
      return Run_Copy
        (Prog => Replicate (Max_Length, Count, Array_Bounds (Item), Drop),
         Left => Item);
   end Replicate;

   ---------------
   -- Replicate --
   ---------------

   function Replicate
     (Count : Natural;
      Item  : Sequence;
      Drop  : Truncation := Error) return Sequence is
   begin
      return Run_Copy
        (Prog => Replicate (Max_Length, Count, Sequence_Bounds (Item), Drop),
         Left => Item.Content (1 .. Item.Length));
   end Replicate;

   --------------
   -- Run_Copy --
   --------------

   function Run_Copy
     (Prog  : Program;
      Left  : Element_Array;
      Right : Element_Array := Null_Element_Array) return Sequence
   is
      Result : Sequence;
   begin
      Run (Prog, Result.Content, Left, Right);
      Result.Length := Prog.Result_Length;
      return Result;
   end Run_Copy;

   ------------------
   -- Run_In_Place --
   ------------------

   procedure Run_In_Place
     (Prog  : Program;
      Left  : in out Sequence;
      Right : Element_Array := Null_Element_Array)
   is
   begin
      pragma Warnings (Off);
      --  Newer compilers complain, ``warning: writable actual for "Target"
      --  overlaps with actual for "Left"''. But Run is safe in the presence of
      --  overlaps.
      Run (Prog, Left.Content, Left.Content (1 .. Left.Length), Right);
      pragma Warnings (On);
      Left.Length := Prog.Result_Length;
   end Run_In_Place;

   ---------------------
   -- Sequence_Bounds --
   ---------------------

   function Sequence_Bounds (S : Sequence) return Bounds is
   begin
      return (Lo => 1, Hi => S.Length);
   end Sequence_Bounds;

   ---------
   -- Set --
   ---------

   procedure Set
     (Item   : in out Sequence;
      Source : Element_Array;
      Drop   : Truncation := Error) is
   begin
      Item := To_Sequence (Source, Drop);
   end Set;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length (Source : in out Sequence; Length : Length_Range) is
   begin
      Source.Length := Length;
   end Set_Length;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Sequence;
      Low    : Positive;
      High   : Natural) return Element_Array
   is
   begin

      if Low > Source.Length + 1 or else High > Source.Length then
         raise Index_Error;
      end if;

      return Source.Content (Low .. High);
   end Slice;

   ----------
   -- Tail --
   ----------

   procedure Tail
     (Source : in out Sequence;
      Count  : Natural;
      Pad    : Element;
      Drop   : Truncation := Error)
   is
   begin
      Run_In_Place
        (Prog => Head_Tail
           (Max_Length,
            Sequence_Bounds (Source),
            Count,
            Drop,
            Tail),
         Left  => Source,
         Right => Element_Array'(1 => Pad));
   end Tail;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Sequence;
      Count  : Natural;
      Pad    : Element;
      Drop   : Truncation := Error) return Sequence
   is
   begin
      return Run_Copy
        (Prog => Head_Tail
           (Max_Length,
            Sequence_Bounds (Source),
            Count,
            Drop,
            Tail),
         Left  => Source.Content (1 .. Source.Length),
         Right => Element_Array'(1 => Pad));
   end Tail;

   ----------------------
   -- To_Element_Array --
   ----------------------

   function To_Element_Array (Source : Sequence) return Element_Array is
   begin
      return Source.Content (1 .. Source.Length);
   end To_Element_Array;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence
     (Source : Element_Array;
      Drop   : Truncation := Error) return Sequence is
   begin
      return Replicate (1, Source, Drop);
   end To_Sequence;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Length : Length_Range) return Sequence is
      Result : Sequence;
      pragma Warnings (Off, Sequence);
      --  Not fully initialized, but the default initialization is what we
      --  want.
   begin
      Result.Length := Length;
      return Result;
   end To_Sequence;

   --------------------------
   -- Unchecked_Element_Of --
   --------------------------

   function Unchecked_Element_Of
     (Source : not null access Sequence;
      Index  : Positive) return Element_Ptr
   is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;

      return Source.Content (Index)'Unrestricted_Access;
   end Unchecked_Element_Of;

end PolyORB.Sequences.Bounded;
