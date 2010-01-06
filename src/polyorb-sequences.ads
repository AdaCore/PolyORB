------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . S E Q U E N C E S                     --
--                                                                          --
--                                 S p e c                                  --
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
--  Pattern_Error is raised when a null pattern string is
--  passed. Index_Error is raised when indexes are out of range.

package PolyORB.Sequences is

   pragma Preelaborate;

   Length_Error, Pattern_Error, Index_Error : exception;

   type Alignment is (Left, Right, Center);
   type Truncation is (Left, Right, Error);
   type Membership is (Inside, Outside);
   type Direction is (Forward, Backward);
   type Trim_End is (Left, Right, Both);

   type Extremity is (Head, Tail);
   type Search_Kind is (Return_Count, Return_Index);

   --  The low and high bound of an element array or slice thereof

   type Bounds is record
      Lo, Hi : Integer;
   end record;

private

   function Length (Index_Range : Bounds) return Natural;
   --  Return the length of the slice or array whose bounds are given

   function Round (Length : Natural) return Natural;
   --  Compute appropriate Length. If Length = 0, return 0. If not, return
   --  Initial_Size + N * Increment_Size where N is the smallest integer
   --  such that Length < Initial_Size + N * Increment_Size.

   -----------------------------------
   -- The Sequences Virtual Machine --
   -----------------------------------

   --  All sequences operations can be represented without reference to
   --  the sequence element type as a sequence of slice assignments from
   --  at most two "operand" element arrays into a "target" element array.

   --  Non-generic versions of all sequence operations are provided in this
   --  package which operate only on element array indices; the generic
   --  versions of these operations, operating on actual element arrays, can
   --  thus be implemented by computing the appropriate sequence of assignments
   --  and then applying it to the actual arrays.

   Max_Program_Length : constant := 3;
   type Any_Program_Index is new Integer range -1 .. Max_Program_Length - 1;
   subtype Program_Index is Any_Program_Index
                              range 0 .. Any_Program_Index'Last;
   --  A sequence operation consists in at most three slice assignments

   type Operand_Reference is (Left, Right);
   --  The source of one assignment is either the left operand or the right
   --  operand of the operation.

   --  Description of an elementary operation:
   --  A slice of the result array is assigned from a slice of either operand;
   --  if the source slice is shorter than the target slice, it is replicated
   --  as necessary to fill the target slice. In that case, the length of the
   --  target slice must always be an integral multiple of the length of the
   --  source slice.

   type Assignment is record
      Source : Operand_Reference;
      Target_Bounds, Source_Bounds : Bounds;
   end record;
   type Assignment_Array is array (Program_Index) of Assignment;

   --  A program describes a sequence operation in terms of successive
   --  slice assignments.

   type Program is record
      Result_Length  : Natural;
      --  Length of the resulting sequence

      Last : Any_Program_Index := -1;
      --  Index of last assignment in program (i.e. program length - 1)

      Assignments    : Assignment_Array;
      --  Description of each slice assignments. Only items indexed
      --  0 .. Program_Length - 1 are meaningful.
   end record;

   generic
      type Element is private;
      type Element_Array is array (Positive range <>) of Element;
   procedure Run
     (Prog : Program;
      Target : out Element_Array;
      Left   : Element_Array;
      Right  : Element_Array);
   --  Generic execution engine to be instantiated with appropriate element
   --  and element array types.

   --  For all functions below, Max_Length is the maximum length for the case
   --  of bounded sequences, or 0 for the case of unbounded sequences. The
   --  Left and Right indications designate what arguments should be assigned
   --  to the Left and Right operands when running the returned program.

   function Head_Tail
     (Max_Length       : Natural;
      Source           : Bounds;
      Count            : Natural;
      Drop             : Truncation := Error;
      What             : Extremity;
      Suppress_Padding : Boolean := False) return Program;
   --  Get Head or Tail, depending on What.
   --  Left:  Source
   --  Right: Padding element (bounds 1 .. 1)
   --  If Suppress_Padding is True, the operation to copy the provided
   --  padding value into the target sequence is not generated, and the
   --  Right operand will be ignored at execution.

   function Replace_Slice
     (Max_Length : Natural;
      Source     : Bounds;
      Slice      : Bounds;
      By         : Bounds;
      Drop       : Truncation := Error) return Program;
   --  Replace Slice in Source with By.
   --  Left:  Source
   --  Right: By

   function Replicate
     (Max_Length : Natural;
      Count      : Natural;
      Item       : Bounds;
      Drop       : Truncation := Error) return Program;
   --  Replicate Item Count times.
   --  Left:  Item
   --  Right: unused

   type Check_Slice_Function is
     access function (Lo, Hi : Positive) return Boolean;
   --  Test for a given slice of a certain sequence against a certain property

   function Count_Index
     (Check_Slice : Check_Slice_Function;
      Source      : Bounds;
      Pattern     : Bounds;
      What        : Search_Kind;
      Going       : Direction := Forward) return Natural;
   --  Common subprogram used to implement Count and Index, depending on
   --  the What parameter. In both cases Check_Slice should return True if
   --  the indicated slice of the sequence being processed matches the
   --  desired pattern, whose bounds are indicated.

end PolyORB.Sequences;
