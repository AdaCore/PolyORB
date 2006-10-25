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

with Ada.Finalization;
with System;

package PolyORB.Sequences is

   pragma Preelaborate;

   Length_Error, Pattern_Error, Index_Error : exception;

   type Alignment is (Left, Right, Center);
   type Truncation is (Left, Right, Error);
   type Membership is (Inside, Outside);
   type Direction is (Forward, Backward);

   type Trim_End is (Left, Right, Both);

   ---------------------------------
   -- Implementation of sequences --
   ---------------------------------

   --  The implementation of sequences is split into two parts:

   --  * a universal abstract array wrapper, which encapsulates an array of
   --    elements, of which a concrete implementation must be provided for
   --    each possible element type.

   --  * a common abstract sequence type providing concrete shared subprograms
   --    the core of the sequence management code, which is shared across all
   --    instance types). Concrete versions are tied to a particular type
   --    of array wrapper, and are provided by a generic instantiation.

   --  Universal Integer-index array wrapper

   type Universal_Array_Base is abstract tagged limited private;
   subtype Universal_Array is Universal_Array_Base'Class;
   --  Type Universal_Array is a universal array wrapper, derived for each
   --  possible element type. Sequence operations only manipulate storage
   --  through Universal_Array objects, and can thus be factored across all
   --  instances.

   type Universal_Array_Access is access Universal_Array;

   function First (A : Universal_Array_Base) return Integer is abstract;
   --  Return the index of A's first element

   function Length (A : Universal_Array_Base) return Natural is abstract;
   --  Return the length of A

--     function Get_Element
--       (A : Universal_Array_Base; Index : Integer) return System.Address
--        is abstract;
--     --  Return the address of the Index'th element in A

   procedure Set_Elements
     (A         : in out Universal_Array_Base;
      Low, High : Integer;
      Value     : System.Address) is abstract;
   --  Perform element assignment:
   --  Assign Value.all to each element of A (Low .. High)
   --  If Value is null, set to the default value of the element type.

   procedure Copy_Slice
     (Target_Arr : in out Universal_Array_Base;
      Target_Low : Integer;
      Source_Arr : Universal_Array_Base;
      Source_Low : Integer;
      Length     : Natural) is abstract;
   --  Perform slice copy:
   --  Target_Arr (Target_Low .. Target_Low + Length - 1) :=
   --    Source_Arr (Source_Low .. Source_Low + Length - 1)

   function Slice_Equals
     (Left_Arr  : Universal_Array_Base;
      Left_Low  : Integer;
      Right_Arr : Universal_Array_Base;
      Right_Low : Integer;
      Length    : Natural) return Boolean is abstract;
   --  Slice equality:
   --  Left_Arr (Left_Low .. Left_Low + Length - 1)
   --    = Right_Arr (Right_Low .. Right_Low + Length - 1)

   function Allocate
     (A      : Universal_Array_Base;
      Length : Natural) return Universal_Array_Access is abstract;
   --  Dynamically allocate a universal array with the given length.
   --  Parameter A is used only for dispatching.

   procedure Deallocate (A : in out Universal_Array_Base) is abstract;
   --  Deallocate the underlying storage of an array allocated by Allocate.
   --  Causes A to become invalid. Subsidiary routine for the following
   --  procedure.

   --  Common code for all unbounded sequences

   package Universal_Unbounded is

      Initial_Size   : constant Natural := 3;
      Increment_Size : constant Natural := 2;
      --  XXX move to body ???
      --  XXX make named numbers???

      type Sequence is abstract new Ada.Finalization.Controlled with record
         Length   : Natural;

         Contents : Universal_Array_Access;
         --  Must never be null
      end record;

      procedure Initialize (S : in out Sequence) is abstract;

      function Null_Sequence return Sequence is abstract;
      --  Abstract constructor: returns a sequence of zero length

      procedure Append
        (Source   : in out Sequence;
         New_Item : Universal_Array);

      procedure Delete
        (Source  : in out Sequence;
         From    : Positive;
         Through : Natural);

      procedure Insert
        (Source   : in out Sequence;
         Before   : Positive;
         New_Item : Universal_Array);

      procedure Overwrite
        (Source   : in out Sequence;
         Position : Positive;
         New_Item : Universal_Array);

      procedure Replace_Slice
        (Source : in out Sequence;
         Low    : Positive;
         High   : Natural;
         By     : Universal_Array);

      procedure Reallocate
        (Source     : in out Sequence;
         New_Length : Natural);
      --  Set Source's length to New_Length. This includes checking whether
      --  Source.Contents should be extended. If so, then copy old
      --  Source.Contents (1 .. Source.Length) in new Source.Contents, and
      --  deallocate previous Contents.

      --  Subprograms below are as defined in the CORBA standard mapping for
      --  IDL sequences, which itself specifies semantics modeled after
      --  Ada string operations.

      type Search_Kind is (Return_Count, Return_Index);

      function Count_Index
        (Source  : Sequence;
         Pattern : Universal_Array;
         What    : Search_Kind;
         Going   : Direction := Forward) return Natural;
      --  Common subprogram used to implement Count and Index, depending on
      --  the What parameter.

      type Extremity is (Head, Tail);
      procedure Get_Head_Tail
        (Source : Sequence;
         Count  : Natural;
         Pad    : System.Address;
         Into   : in out Sequence;
         What   : Extremity);
      --  Into := [Head|Tail] (Source, Count, Pad)
      --  Into must be a newly-allocated sequence of length Count.

      procedure Repeat
        (Item : Universal_Array;
         Into : in out Sequence);
      --  Set the contents of Into to repetitions of Item.
      --  Into.Length must be an integral multiple of Length (Item).

   private

      procedure Adjust (S : in out Sequence);
      procedure Finalize (S : in out Sequence);

   end Universal_Unbounded;

private

   type Universal_Array_Base is abstract tagged limited null record;

   procedure Deallocate (AA : in out Universal_Array_Access);
   --  Deallocate an array allocated by Allocate

end PolyORB.Sequences;
