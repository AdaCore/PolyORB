------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E Q U E N C E S . U N B O U N D E D           --
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

package body PolyORB.Sequences.Unbounded is

   Dummy_Element_Ptr : Element_Ptr;
   pragma Warnings (Off, Dummy_Element_Ptr);
   --  This variable is only used to provide a placeholder expression of type
   --  Element that is preelaborable (but is never actually evaluated).

   Empty_Element_Array : aliased Element_Array :=
     (1 .. 0 => Dummy_Element_Ptr.all);
   Empty : constant Element_Array_Access := Empty_Element_Array'Access;

   ------------------------------------------------
   -- SVM implementation for unbounded sequences --
   ------------------------------------------------

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

   function Allocate (Length : Natural) return Element_Array_Access;
   --  Return a newly allocated element array of the given length, except
   --  if Length is 0, in which case Empty is returned.

   function Array_Bounds (A : Element_Array) return Bounds;
   --  Return (A'First, A'Last)

   function Sequence_Bounds (S : Sequence) return Bounds;
   --  Return (1, Length (S))

   function "&" (Left, Right : Element_Array) return Sequence;
   --  Return To_Sequence (Left & Right)

   function Count_Index
     (Source  : Sequence;
      Pattern : Element_Array;
      What    : Search_Kind;
      Going   : Direction := Forward) return Natural;
   --  Common subprogram used to implement Count and Index, depending on
   --  the What parameter.

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Element_Array) return Sequence is
      Left_Bounds : constant Bounds := Array_Bounds (Left);
   begin
      --  Replace a null-length slice of Left located right after the last
      --  element with Right.

      return Run_Copy
        (Prog  =>
           Replace_Slice
             (0,
              Left_Bounds,
              Bounds'(Left_Bounds.Hi + 1, Left_Bounds.Hi),
              Array_Bounds (Right)),
         Left  => Left,
         Right => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Sequence) return Sequence is
   begin
      return Left.Content (1 .. Left.Length)
        & Right.Content (1 .. Right.Length);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Sequence; Right : Element_Array) return Sequence is
   begin
      return Sequence'(Left.Content (1 .. Left.Length) & Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Element_Array; Right : Sequence) return Sequence is
   begin
      return Sequence'(Left & Right.Content (1 .. Right.Length));
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Sequence; Right : Element) return Sequence is
   begin
      return Left & Element_Array'(1 => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Element; Right : Sequence) return Sequence is
   begin
      return Element_Array'(1 => Left) & Right;
   end "&";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Element) return Sequence is
   begin
      return Left * Element_Array'(1 => Right);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Element_Array) return Sequence is
   begin
      return Run_Copy
        (Prog => Replicate (0, Left, Array_Bounds (Right)),
         Left => Right);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Sequence) return Sequence is
   begin
      return Run_Copy
        (Prog => Replicate (0, Left, Sequence_Bounds (Right)),
         Left => Right.Content (1 .. Right.Length));
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

   function "=" (Left : Element_Array; Right : Sequence) return Boolean is
      L : Natural renames Right.Length;
   begin
      return Left'Length = L
        and then Left = Right.Content (1 .. L);
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : Sequence; Right : Element_Array) return Boolean is
   begin
      return Right = Left;
   end "=";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (X : in out Sequence) is
   begin
      if X.Length > 0 then
         X.Content := new Element_Array'(X.Content.all);
      end if;
   end Adjust;

   --------------
   -- Allocate --
   --------------

   function Allocate (Length : Natural) return Element_Array_Access is
   begin
      if Length > 0 then
         return new Element_Array (1 .. Length);
      else
         return Empty;
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out Sequence; New_Item : Sequence) is
   begin
      Append (Source, New_Item.Content (1 .. New_Item.Length));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out Sequence; New_Item : Element_Array) is
      Left_Bounds : constant Bounds := Sequence_Bounds (Source);
   begin
      Run_In_Place
        (Prog  =>
           Replace_Slice
             (0,
              Left_Bounds,
              Bounds'(Left_Bounds.Hi + 1, Left_Bounds.Hi),
              Array_Bounds (New_Item)),
         Left  => Source,
         Right => New_Item);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out Sequence; New_Item : Element) is
   begin
      Append (Source, Element_Array'(1 => New_Item));
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
     (Source  : Sequence; Pattern : Element_Array) return Natural
   is
   begin
      return Count_Index (Source, Pattern, What => Return_Count);
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
      --  True when Source (Lo .. Hi) = Pattern

      -----------------------
      -- Check_For_Pattern --
      -----------------------

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
      Through : Natural)
   is
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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (X : in out Sequence) is
   begin
      --  Note: X.Content'Length is the allocated length of the sequence, can
      --  be greater than X.Length (the current length). If X.Content'Length
      --  is 0, we know that X.Content is Empty, not an access to a dynamically
      --  allocated array.

      if X.Content'Length > 0 then
         Free (X.Content);
      end if;

      X.Length  := 0;
      X.Content := Empty;
   end Finalize;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Source : Sequence;
      Index  : Positive) return Element
   is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;
      return Source.Content (Index);
   end Get_Element;

   ----------
   -- Head --
   ----------

   procedure Head
     (Source : in out Sequence;
      Count  : Natural;
      Pad    : Element)
   is
   begin
      Run_In_Place
        (Prog => Head_Tail
           (0,
            Sequence_Bounds (Source),
            Count,
            What => Head),
         Left  => Source,
         Right => Element_Array'(1 => Pad));
   end Head;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Sequence;
      Count  : Natural;
      Pad    : Element) return Sequence
   is
   begin
      return Run_Copy
        (Prog => Head_Tail
           (0,
            Sequence_Bounds (Source),
            Count,
            What => Head),
         Left  => Source.Content (1 .. Source.Length),
         Right => Element_Array'(1 => Pad));
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Sequence;
      Pattern : Element_Array;
      Going   : Direction := Forward) return Natural
   is
   begin
      return Count_Index (Source, Pattern, Return_Index, Going);
   end Index;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (X : in out Sequence) is
   begin
      X.Length  := 0;
      X.Content := Allocate (0);
   end Initialize;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Sequence;
      Before   : Positive;
      New_Item : Element_Array) return Sequence
   is
   begin
      return Replace_Slice
        (Source,
         Low  => Before,
         High => Before - 1,
         By   => New_Item);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Source   : in out Sequence;
      Before   : Positive;
      New_Item : Element_Array)
   is
   begin
      Replace_Slice
        (Source,
         Low  => Before,
         High => Before - 1,
         By   => New_Item);
   end Insert;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Source : Sequence) return Boolean is
   begin
      return Source.Length = 0;
   end Is_Null;

   ------------
   -- Length --
   ------------

   function Length (Source : Sequence) return Natural is
   begin
      return Source.Length;
   end Length;

   -------------------
   -- Null_Sequence --
   -------------------

   function Null_Sequence return Sequence is
   begin
      return To_Sequence (0);
   end Null_Sequence;

   ---------------
   -- Overwrite --
   ---------------

   procedure Overwrite
     (Source   : in out Sequence;
      Position : Positive;
      New_Item : Element_Array)
   is
   begin
      Replace_Slice
        (Source => Source,
         Low    => Position,
         High   => Position + New_Item'Length - 1,
         By     => New_Item);
   end Overwrite;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Sequence;
      Position : Positive;
      New_Item : Element_Array) return Sequence
   is
   begin
      return Replace_Slice
        (Source,
         Low  => Position,
         High => Position + New_Item'Length - 1,
         By   => New_Item);
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
      By     : Element_Array) return Sequence
   is
   begin
      return Run_Copy
        (Prog  =>
           Replace_Slice
             (0,
              Sequence_Bounds (Source),
              Bounds'(Low, High),
              Array_Bounds (By)),
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
      By     : Element_Array)
   is
   begin
      Run_In_Place
        (Prog  =>
           Replace_Slice
             (0,
              Sequence_Bounds (Source),
              Bounds'(Low, High),
              Array_Bounds (By)),
         Left  => Source,
         Right => By);
   end Replace_Slice;

   --------------
   -- Run_Copy --
   --------------

   function Run_Copy
     (Prog  : Program;
      Left  : Element_Array;
      Right : Element_Array := Null_Element_Array) return Sequence
   is
      Result : constant Sequence := To_Sequence (Prog.Result_Length);
   begin
      Run (Prog, Result.Content.all, Left, Right);
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
      Old_Contents : Element_Array_Access := Left.Content;
      New_Contents : Element_Array_Access;

      Old_Alloc : constant Natural := Old_Contents'Length;
      New_Alloc : constant Natural := Round (Prog.Result_Length);
   begin
      if New_Alloc = Old_Alloc then
         New_Contents := Old_Contents;
      else
         New_Contents := Allocate (New_Alloc);
      end if;

      Run (Prog, New_Contents.all, Old_Contents (1 .. Left.Length), Right);
      Left.Length  := Prog.Result_Length;
      Left.Content := New_Contents;

      if New_Contents /= Old_Contents and then Old_Contents'Length > 0 then
         Free (Old_Contents);
      end if;
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
      Source : Element_Array)
   is
   begin
      Item := To_Sequence (Source);
   end Set;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length (Source : in out Sequence; Length : Natural) is
   begin
      Run_In_Place
        (Prog  => Head_Tail
                    (0,
                     Sequence_Bounds (Source),
                     Length,
                     What             => Head,
                     Suppress_Padding => True),
         Left  => Source,
         Right => Empty_Element_Array);
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

   function To_Sequence (Source : Element_Array) return Sequence is
   begin
      return 1 * Source;
   end To_Sequence;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Length : Natural) return Sequence is
   begin
      return (Ada.Finalization.Controlled with
              Length  => Length,
              Content => Allocate (Length));
   end To_Sequence;

   ----------
   -- Tail --
   ----------

   procedure Tail
     (Source : in out Sequence;
      Count  : Natural;
      Pad    : Element)
   is
   begin
      Run_In_Place
        (Prog => Head_Tail
           (0,
            Sequence_Bounds (Source),
            Count,
            What => Tail),
         Left  => Source,
         Right => Element_Array'(1 => Pad));
   end Tail;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Sequence;
      Count  : Natural;
      Pad    : Element) return Sequence
   is
   begin
      return Run_Copy
        (Prog => Head_Tail
           (0,
            Sequence_Bounds (Source),
            Count,
            What => Tail),
         Left  => Source.Content (1 .. Source.Length),
         Right => Element_Array'(1 => Pad));
   end Tail;

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
      return Element_Ptr'(Source.Content (Index)'Unrestricted_Access);
   end Unchecked_Element_Of;

end PolyORB.Sequences.Unbounded;
