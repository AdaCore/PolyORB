------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E Q U E N C E S . U N B O U N D E D           --
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

with System.Address_To_Access_Conversions;

package body PolyORB.Sequences.Unbounded is

   package Address_To_Pointers is
     new System.Address_To_Access_Conversions (Element);
   function To_Pointer
     (A : System.Address) return Address_To_Pointers.Object_Pointer
     renames Address_To_Pointers.To_Pointer;

   Dummy_Element_Ptr : Element_Ptr;
   pragma Warnings (Off, Dummy_Element_Ptr);
   --  This variable is only used to provide a placeholder expression of type
   --  Element that is preelaborable (but is never actually evaluated).

   Empty_Element_Array : aliased Element_Array :=
                           (1 .. 0 => Dummy_Element_Ptr.all);
   Empty : constant Element_Array_Access := Empty_Element_Array'Access;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Unchecked_Elements (Source : Sequence) return Element_Array_Access;
   --  Return an access to the underlying element array of Source

   function Allocate (Length : Natural) return Universal_Array_Access;
   --  Dynamically allocate a universal array with range 1 .. Length

   --------------
   -- Allocate --
   --------------

   function Allocate (Length : Natural) return Universal_Array_Access is
      Elements : Element_Array_Access := Empty;
   begin
      if Length > 0 then
         Elements := new Element_Array (1 .. Length);
      end if;
      return new Element_Array_Wrapper (Elements);
   end Allocate;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (A      : Element_Array_Wrapper;
      Length : Natural) return Universal_Array_Access
   is
      pragma Unreferenced (A);
      --  A is used only for dispatching
   begin
      return Allocate (Length);
   end Allocate;

   ----------------
   -- Copy_Slice --
   ----------------

   procedure Copy_Slice
     (Target_Arr : in out Element_Array_Wrapper;
      Target_Low : Integer;
      Source_Arr : Element_Array_Wrapper;
      Source_Low : Integer;
      Length     : Natural) is
   begin
      Target_Arr.E (Target_Low .. Target_Low + Length - 1) :=
        Source_Arr.E (Source_Low .. Source_Low + Length - 1);
   end Copy_Slice;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (A : in out Element_Array_Wrapper) is
      Dynamic_Elements : Element_Array_Access := A.E.all'Unchecked_Access;
   begin
      if Dynamic_Elements /= Empty then
         Free (Dynamic_Elements);
      end if;
   end Deallocate;

   -----------
   -- First --
   -----------

   function First (A : Element_Array_Wrapper) return Integer is
   begin
      return A.E'First;
   end First;

   ------------
   -- Length --
   ------------

   function Length (A : Element_Array_Wrapper) return Natural is
   begin
      return A.E'Length;
   end Length;

   ------------------
   -- Set_Elements --
   ------------------

   procedure Set_Elements
     (A         : in out Element_Array_Wrapper;
      Low, High : Integer;
      Value     : System.Address)
   is
      Null_Element : aliased Element;
      pragma Warnings (Off, Null_Element);
      --  Used to provide default value, if Value = null

      Actual_Value : System.Address := Value;

      use System;

   begin
      if Actual_Value = Null_Address then
         Actual_Value := Null_Element'Address;
      end if;

      for J in Low .. High loop
         A.E (J) := To_Pointer (Actual_Value).all;
      end loop;
   end Set_Elements;

   ------------------
   -- Slice_Equals --
   ------------------

   function Slice_Equals
     (Left_Arr  : Element_Array_Wrapper;
      Left_Low  : Integer;
      Right_Arr : Element_Array_Wrapper;
      Right_Low : Integer;
      Length    : Natural) return Boolean is
   begin
      return Left_Arr.E (Left_Low .. Left_Low + Length - 1)
        = Right_Arr.E (Right_Low .. Right_Low + Length - 1);
   end Slice_Equals;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Sequence) return Boolean is
   begin
      return Left.Length = Right.Length
        and then Unchecked_Elements (Left) (1 .. Right.Length)
               = Unchecked_Elements (Right) (1 .. Right.Length);
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : Element_Array; Right : Sequence) return Boolean is
   begin
      return
        Left'Length = Right.Length
        and then Left (Left'Range)
               = Unchecked_Elements (Right) (1 .. Right.Length);
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : Sequence; Right : Element_Array) return Boolean is
   begin
      return Right = Left;
   end "=";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Sequence) return Sequence is
      Result : Sequence := Left;
   begin
      Append (Result, Right.Contents.all);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Sequence; Right : Element_Array) return Sequence is
      Result : Sequence := Left;
      Right_Wrapper : Element_Array_Wrapper (E => Right'Unrestricted_Access);
   begin
      Append (Result, Right_Wrapper);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Element_Array; Right : Sequence) return Sequence is
      Result : Sequence := To_Sequence (Left);
   begin
      Append (Result, Right);
      return Result;
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
      Result : Sequence := To_Sequence (Left * Right'Length);
   begin
      if Left > 0 and then Right'Length > 0 then
         declare
            Right_Wrapper : Element_Array_Wrapper
                              (E => Right'Unrestricted_Access);
         begin
            Repeat (Right_Wrapper, Into => Result);
         end;
      end if;
      return Result;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Natural; Right : Sequence) return Sequence is
      Result : Sequence := To_Sequence (Left * Right.Length);
   begin
      Repeat (Right.Contents.all, Into => Result);
      return Result;
   end "*";

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out Sequence; New_Item : Sequence) is
   begin
      Append (Source, New_Item.Contents.all);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Source : in out Sequence; New_Item : Element_Array)
   is
      New_Item_Wrapper : Element_Array_Wrapper
                           (E => New_Item'Unrestricted_Access);
   begin
      Append (Source, New_Item_Wrapper);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : Element) is
   begin
      Reallocate (Source, Source.Length + 1);
      Unchecked_Elements (Source) (Source.Length) := New_Item;
   end Append;

   -----------
   -- Count --
   -----------

   function Count (Source  : Sequence; Pattern : Element_Array) return Natural
   is
      Pattern_Wrapper : Element_Array_Wrapper
                          (E => Pattern'Unrestricted_Access);
   begin
      return Count_Index (Source, Pattern_Wrapper,
                          What => Universal_Unbounded.Return_Count);
   end Count;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Source  : in out Sequence;
      From    : Positive;
      Through : Natural) is
   begin
      Universal_Unbounded.Delete
        (Universal_Unbounded.Sequence (Source), From, Through);
   end Delete;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Sequence;
      From    : Positive;
      Through : Natural) return Sequence
   is
      Result : Sequence := Source;
   begin
      Delete (Result, From, Through);
      return Result;
   end Delete;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Source : Sequence;
      Index  : Positive) return Element is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;
      return Unchecked_Elements (Source) (Index);
   end Get_Element;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Sequence;
      Count  : Natural;
      Pad    : Element) return Sequence
   is
      Result : Sequence := To_Sequence (Count);
   begin
      Get_Head_Tail (Source, Count, Pad'Address,
                     Into => Result, What => Universal_Unbounded.Head);
      return Result;
   end Head;

   ----------
   -- Head --
   ----------

   procedure Head
     (Source : in out Sequence;
      Count  : Natural;
      Pad    : Element) is
   begin
      Source := Head (Source, Count, Pad);
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Sequence;
      Pattern : Element_Array;
      Going   : Direction := Forward) return Natural
   is
      Pattern_Wrapper : Element_Array_Wrapper
                          (E => Pattern'Unrestricted_Access);
   begin
      return Count_Index
        (Source, Pattern_Wrapper, Universal_Unbounded.Return_Index, Going);
   end Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Sequence) is
   begin
      Object.Length   := 0;
      Object.Contents := Allocate (0);
   end Initialize;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Sequence;
      Before   : Positive;
      New_Item : Element_Array) return Sequence
   is
      Result : Sequence := Source;
   begin
      Insert (Result, Before, New_Item);
      return Result;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Source   : in out Sequence;
      Before   : Positive;
      New_Item : Element_Array)
   is
      New_Item_Wrapper : Element_Array_Wrapper
                           (E => New_Item'Unrestricted_Access);
   begin
      Insert (Source, Before, New_Item_Wrapper);
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

   function Overwrite
     (Source   : Sequence;
      Position : Positive;
      New_Item : Element_Array) return Sequence
   is
      Result : Sequence := Source;
   begin
      Overwrite (Result, Position, New_Item);
      return Result;
   end Overwrite;

   ---------------
   -- Overwrite --
   ---------------

   procedure Overwrite
     (Source   : in out Sequence;
      Position : Positive;
      New_Item : Element_Array)
   is
      New_Item_Wrapper : Element_Array_Wrapper
                           (E => New_Item'Unrestricted_Access);
   begin
      Overwrite (Source, Position, New_Item_Wrapper);
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
      Unchecked_Elements (Source) (Index) := By;
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
      Result    : Sequence := Source;

   begin
      Replace_Slice (Result, Low, High, By);
      return Result;
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
      By_Wrapper : Element_Array_Wrapper (E => By'Unrestricted_Access);
   begin
      Replace_Slice (Source, Low, High, By_Wrapper);
   end Replace_Slice;

   ---------
   -- Set --
   ---------

   procedure Set
     (Item   : in out Sequence;
      Source : Element_Array) is
   begin
      Item := To_Sequence (Source);
   end Set;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length (Source : in out Sequence; Length : Natural) is
   begin
      Reallocate (Source, Length);
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
      if Source.Length < Low or else Source.Length < High then
         raise Index_Error;
      end if;
      return Unchecked_Elements (Source) (Low .. High);
   end Slice;

   ----------------------
   -- To_Element_Array --
   ----------------------

   function To_Element_Array (Source : Sequence) return Element_Array is
   begin
      return Unchecked_Elements (Source).all;
   end To_Element_Array;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Source : Element_Array) return Sequence is
      Result : constant Sequence := To_Sequence (Source'Length);
   begin
      Unchecked_Elements (Result) (1 .. Source'Length) :=
        Source (Source'Range);
      return Result;
   end To_Sequence;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Length : Natural) return Sequence is
   begin
      return (Ada.Finalization.Controlled with
              Length   => Length,
              Contents => Allocate (Length));
   end To_Sequence;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Sequence;
      Count  : Natural;
      Pad    : Element) return Sequence
   is
      Result : Sequence := To_Sequence (Count);
   begin
      Get_Head_Tail (Source, Count, Pad'Address,
                     Into => Result, What => Universal_Unbounded.Tail);
      return Result;
   end Tail;

   ----------
   -- Tail --
   ----------

   procedure Tail
     (Source : in out Sequence;
      Count  : Natural;
      Pad    : Element) is
   begin
      Source := Tail (Source, Count, Pad);
   end Tail;

   --------------------------
   -- Unchecked_Element_Of --
   --------------------------

   function Unchecked_Element_Of
     (Source : access Sequence;
      Index  : Positive) return Element_Ptr
   is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;
      return Element_Ptr'
        (Unchecked_Elements (Source.all) (Index)'Unrestricted_Access);
   end Unchecked_Element_Of;

   ------------------------
   -- Unchecked_Elements --
   ------------------------

   function Unchecked_Elements
     (Source : Sequence) return Element_Array_Access
   is
   begin
      return Element_Array_Access
        (Element_Array_Wrapper (Source.Contents.all).E);
   end Unchecked_Elements;

end PolyORB.Sequences.Unbounded;
