------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . S E Q U E N C E S . U N B O U N D E D           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Unchecked_Deallocation;

package body PolyORB.Sequences.Unbounded is

   Initial_Size   : constant Natural := 3;
   Increment_Size : constant Natural := 2;

   Null_Contents : Element_Array_Access;

   function Get_Null_Contents return Element_Array_Access;
   pragma Inline (Get_Null_Contents);
   --  Obtain a pointer to a global zero-length element array.

   procedure Allocate
     (Source : in out Sequence;
      Length : in Natural);
   --  Allocate Source.Content and set Source.Length to Length. Do not
   --  release previous Source.Content.

   procedure Reallocate
     (Source : in out Sequence;
      Length : in Natural);
   --  See whether Source.Content should be extended. If so, then copy
   --  old Source.Content (1 .. Source.Length) in new Source.Content and
   --  deallocate previous Content.

   function  Round (Length : Natural) return Natural;
   --  Compute appropriate Length. If Length = 0, return 0. If not,
   --  return Initial_Size + N * Increment_Size where N is the
   --  smallest integer such that Length < Initial_Size + N * Increment_Size.

   function Get_Null_Contents return Element_Array_Access
   is
      Null_Element : Element;
      pragma Warnings (Off, Null_Element);
      --  Not initialised explicitly.
   begin
      if Null_Contents = null then
         Null_Contents := new Element_Array'(1 .. 0 => Null_Element);
      end if;

      pragma Assert (Null_Contents /= null);

      return Null_Contents;
   end Get_Null_Contents;

   function Element_Array_Equal (Left, Right : Element_Array)
     return Boolean;

   function Element_Array_Equal (Left, Right : Element_Array)
     return Boolean is
   begin
      return Left'Length = Right'Length
        and then Left (Left'Range) = Right (Right'Range);
   end Element_Array_Equal;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Sequence)
     return Boolean is
   begin
      return Element_Array_Equal
        (Left.Content (1 .. Left.Length),
         Right.Content (1 .. Right.Length));
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : in Element_Array; Right : in Sequence)
     return Boolean is
   begin
      return Element_Array_Equal
        (Left,
         Right.Content (1 .. Right.Length));
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : in Sequence; Right : in Element_Array)
     return Boolean is
   begin
      return Element_Array_Equal
        (Left.Content (1 .. Left.Length),
         Right);
   end "=";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : in Sequence)
     return Sequence
   is
   begin
      return To_Sequence
        (Left.Content (1 .. Left.Length) &
         Right.Content (1 .. Right.Length));
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : in Sequence; Right : in Element_Array)
     return Sequence
   is
   begin
      return To_Sequence
        (Left.Content (1 .. Left.Length) & Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : in Element_Array; Right : in Sequence)
     return Sequence
   is
   begin
      return To_Sequence
        (Left & Right.Content (1 .. Right.Length));
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : in Sequence; Right : in Element)
     return Sequence
   is
   begin
      return Left & Element_Array'(1 => Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : in Element; Right : in Sequence)
     return Sequence
   is
   begin
      return Element_Array'(1 => Left) & Right;
   end "&";

   ---------
   -- "*" --
   ---------

   function "*" (Left : in Natural; Right : in Element)
     return Sequence
   is
   begin
      return Left * Element_Array'(1 => Right);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : in Natural; Right : in Element_Array)
     return Sequence
   is
      Length : constant Natural := Right'Length;
      Index  : Natural := 1;
      Result : Sequence;

   begin
      if Left = 0 or else Right'Length = 0 then
         return Result;
      end if;

      Allocate (Result, Left * Length);

      for I in 1 .. Left loop
         Result.Content (Index .. Index + Length - 1) := Right;
         Index := Index + Length;
      end loop;

      return Result;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : in Natural; Right : in Sequence)
     return Sequence
   is
   begin
      return Left * Right.Content.all;
   end "*";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Sequence)
   is
      Content : Element_Array_Access;

   begin
      if Object.Content /= Get_Null_Contents then
         Content := Object.Content;
         Allocate (Object, Object.Length);
         Object.Content (1 .. Object.Length)
           := Content (1 .. Object.Length);
      end if;
   end Adjust;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Source : in out Sequence;
      Length : in Natural) is
   begin
      if Length > 0 then
         Source.Content := new Element_Array (1 .. Round (Length));
      else
         Source.Content := Get_Null_Contents;
      end if;
      Source.Length := Length;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : in Sequence)
   is
      Old_Length  : constant Natural := Source.Length;

   begin
      Reallocate (Source, Old_Length + New_Item.Length);
      Source.Content (Old_Length + 1 .. Source.Length)
        := New_Item.Content (1 .. New_Item.Length);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : in Element_Array)
   is
      Old_Length  : constant Natural := Source.Length;

   begin
      Reallocate (Source, Old_Length + New_Item'Length);
      Source.Content (Old_Length + 1 .. Source.Length) := New_Item;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : in Element)
   is
      Old_Length  : constant Natural := Source.Length;

   begin
      Reallocate (Source, Old_Length + 1);
      Source.Content (Source.Length) := New_Item;
   end Append;

   -----------
   -- Count --
   -----------

   function Count
      (Source  : in Sequence;
       Pattern : in Element_Array)
       return Natural
   is
      Match  : Natural := 0;
      Offset : Natural;
      Length : constant Natural := Pattern'Length;
      First  : constant Integer := Pattern'First;

   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      for Index in 1 .. Source.Length - (Length - 1) loop
         Offset := 0;
         while Offset < Length loop
            exit when Source.Content (Index + Offset)
              /= Pattern (First + Offset);
            Offset := Offset + 1;
         end loop;

         if Offset = Length then
            Match := Match + 1;
         end if;
      end loop;

      return Match;
   end Count;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : in Sequence;
      From    : in Positive;
      Through : in Natural)
      return Sequence
   is
      Result : Sequence;
      Length : Natural;

   begin
      if From > Source.Length + 1
        or else Through > Source.Length
      then
         raise Index_Error;
      end if;

      if Through < From then
         Length := 0;
      else
         Length := Through - From + 1;
      end if;

      Allocate (Result, Source.Length - Length);
      Result.Content (1 .. From - 1)
        := Source.Content (1 .. From - 1);
      Result.Content (From .. Result.Length) :=
        Source.Content (Through + 1 .. Source.Length);

      return Result;
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Source  : in out Sequence;
      From    : in Positive;
      Through : in Natural)
   is
      Old_Length  : constant Natural := Source.Length;
      Old_Content : Element_Array_Access;
      Reallocated : Boolean;

   begin
      if From > Old_Length + 1
        or else Through > Old_Length
      then
         raise Index_Error;
      end if;

      if Through < From then
         return;
      end if;

      Source.Length := Old_Length - (Through - From + 1);
      Old_Content   := Source.Content;
      Reallocated   := (Source.Content'Length /= Round (Source.Length));

      if Reallocated then
         Allocate (Source, Source.Length);
         Source.Content (1 .. From - 1) := Old_Content (1 .. From - 1);
      end if;

      Source.Content (From .. Source.Length)
        := Old_Content (Through + 1 .. Old_Length);

      if Reallocated then
         Free (Old_Content);

      else
         --  Force finalization.

         declare
            Null_Element : Element;
            pragma Warnings (Off, Null_Element);
            --  Null_Element is never assign a value.
         begin
            for I in Source.Length + 1 .. Old_Length loop
               Source.Content (I) := Null_Element;
            end loop;
         end;
      end if;
   end Delete;

   ----------------
   -- Element_Of --
   ----------------

   function Element_Of
     (Source : in Sequence;
      Index  : in Positive)
      return Element is
   begin
      if Index > Source.Length then
         raise Index_Error;
      end if;

      return Source.Content (Index);
   end Element_Of;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Sequence) is
   begin
      Free (Object.Content);
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Element_Array_Access) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Element_Array, Element_Array_Access);
   begin
      if X /= Get_Null_Contents then
         Deallocate (X);
      end if;
   end Free;

   ----------
   -- Head --
   ----------

   function Head
     (Source : in Sequence;
      Count  : in Natural;
      Pad    : in Element)
      return Sequence
   is
      Length : Natural;
      Result : Sequence;

   begin
      Allocate (Result, Count);

      if Source.Length < Count then
         Length := Source.Length;
      else
         Length := Count;
      end if;

      Result.Content (1 .. Length) := Source.Content (1 .. Length);
      for I in Length + 1 .. Count loop
         Result.Content (I) := Pad;
      end loop;

      return Result;
   end Head;

   ----------
   -- Head --
   ----------

   procedure Head
     (Source : in out Sequence;
      Count  : in Natural;
      Pad    : in Element)
   is
      Old_Length : constant Natural := Source.Length;

   begin
      Reallocate (Source, Count);
      for I in Old_Length .. Count loop
         Source.Content (I) := Pad;
      end loop;
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : in Sequence;
      Pattern : in Element_Array;
      Going   : in Direction := Forward)
      return Natural
   is
      Shift  : Integer;
      From   : Natural;
      To     : Natural;

      Offset : Natural;
      Length : constant Natural := Pattern'Length;
      First  : constant Integer := Pattern'First;

   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;

      if Source.Length < Length then
         return 0;
      end if;

      if Going = Forward then
         Shift := 1;
         From  := 1;
         To    := Source.Length - (Length - 1);
      else
         Shift := -1;
         From  := Source.Length - (Length - 1);
         To    := 1;
      end if;

      --  There is always at least one pass because Length <= Source.Length
      loop
         Offset := 0;
         while Offset < Length loop
            exit when Source.Content (From + Offset)
              /= Pattern (First + Offset);
            Offset := Offset + 1;
         end loop;

         if Offset = Length then
            return From;
         end if;

         exit when From = To;
         From := From + Shift;
      end loop;

      --  No match
      return 0;
   end Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Sequence) is
   begin
      Object.Length  := 0;
      Object.Content := Get_Null_Contents;
   end Initialize;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : in Sequence;
      Before   : in Positive;
      New_Item : in Element_Array)
     return Sequence
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
      Before   : in Positive;
      New_Item : in Element_Array)
   is
      Item_Length : constant Natural := New_Item'Length;
      Old_Length  : constant Natural := Source.Length;
      Old_Content : Element_Array_Access;
      Reallocated : Boolean;

   begin
      if Source.Length < Before then
         raise Index_Error;
      end if;

      Source.Length := Old_Length + Item_Length;
      Old_Content   := Source.Content;
      Reallocated   := (Source.Content'Length /= Round (Source.Length));

      if Reallocated then
         Allocate (Source, Source.Length);
         Source.Content (1 .. Before - 1) := Old_Content (1 .. Before - 1);
      end if;

      Source.Content (Before + Item_Length .. Source.Length)
        := Old_Content (Before .. Old_Length);
      Source.Content (Before .. Before + Item_Length - 1) := New_Item;

      if Reallocated then
         Free (Old_Content);
      end if;
   end Insert;

   -------------------
   -- Null_Sequence --
   -------------------

   function Null_Sequence return Sequence is
   begin
      return (Ada.Finalization.Controlled with
              Length  => 0,
              Content => Get_Null_Contents);
   end Null_Sequence;

   ------------
   -- Length --
   ------------

   function Length (Source : in Sequence) return Natural is
   begin
      return Source.Length;
   end Length;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : in Sequence;
      Position : in Positive;
      New_Item : in Element_Array)
      return Sequence
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
      Position : in Positive;
      New_Item : in Element_Array)
   is
      Item_Length : constant Natural := New_Item'Length;
      Old_Length  : constant Natural := Source.Length;
      Old_Content : Element_Array_Access;
      Reallocated : Boolean;

   begin
      if Position > Source.Length + 1 then
         raise Index_Error;
      end if;

      if Position + Item_Length > Old_Length then
         Source.Length := Position + Item_Length;
      end if;

      Old_Content := Source.Content;
      Reallocated := (Source.Content'Length /= Round (Source.Length));

      if Reallocated then
         Allocate (Source, Source.Length);
         Source.Content (1 .. Position - 1) := Old_Content (1 .. Position - 1);
         Free (Old_Content);
      end if;

      Source.Content (Position .. Position + Item_Length - 1) := New_Item;
   end Overwrite;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate
     (Source : in out Sequence;
      Length : in Natural)
   is
      Old_Content : Element_Array_Access := Source.Content;
      Old_Length  : constant Natural := Source.Length;

      Min_Length  : Natural;
      --  The number of elements in the new allocation that
      --  need to be copied from the old one.

   begin
      if Length = 0 then
         Source := Null_Sequence;
         Free (Old_Content);
      else
         if Source.Length > Length then
            Min_Length := Length;
         else
            Min_Length := Source.Length;
         end if;

         if Source.Content'Length /= Round (Length) then
            Allocate (Source, Length);
            Source.Content (1 .. Min_Length)
              := Old_Content (1 .. Min_Length);
            Free (Old_Content);
         else
            --  Force finalization (for the case where we have
            --  shrunk then list).

            declare
               Null_Element : Element;
               pragma Warnings (Off, Null_Element);
               --  Null_Element is never assign a value.
            begin
               for I in Min_Length + 1 .. Old_Length loop
                  Source.Content (I) := Null_Element;
               end loop;
            end;
            Source.Length := Length;
         end if;
      end if;
   end Reallocate;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in out Sequence;
      Index  : in Positive;
      By     : in Element) is
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
     (Source : in Sequence;
      Low    : in Positive;
      High   : in Natural;
      By     : in Element_Array)
      return Sequence
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
      Low    : in Positive;
      High   : in Natural;
      By     : in Element_Array)
   is
      By_Length   : constant Natural := By'Length;
      Old_Length  : constant Natural := Source.Length;
      Old_Content : Element_Array_Access;
      Reallocated : Boolean;

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
      Old_Content   := Source.Content;
      Reallocated   := (Source.Content'Length /= Round (Source.Length));

      if Reallocated then
         Allocate (Source, Source.Length);
         Source.Content (1 .. Low - 1) := Old_Content (1 .. Low - 1);
      end if;

      Source.Content (Low + By_Length .. Source.Length)
        := Old_Content (High + 1 .. Old_Length);
      Source.Content (Low .. Low + By_Length - 1)
        := By;

      if Reallocated then
         Free (Old_Content);

      else
         --  Force finalization

         declare
            Null_Element : Element;
            pragma Warnings (Off, Null_Element);
            --  Null_Element is never assign a value.
         begin
            for I in Old_Length + 1 .. Source.Length loop
               Source.Content (I) := Null_Element;
            end loop;
         end;
      end if;
   end Replace_Slice;

   -----------
   -- Round --
   -----------

   function Round (Length : Natural) return Natural
   is
      Times : Natural;

   begin
      if Length = 0 then
         return 0;
      end if;

      if Length <= Initial_Size then
         return Initial_Size;
      end if;

      Times := ((Length - Initial_Size) / Increment_Size) + 1;
      return Initial_Size + (Increment_Size * Times);
   end Round;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : in Sequence;
      Low    : in Positive;
      High   : in Natural)
      return Element_Array is
   begin
      if Source.Length < Low
        or else Source.Length < High
      then
         raise Index_Error;
      end if;

      return Source.Content (Low .. High);
   end Slice;

   ----------------------
   -- To_Element_Array --
   ----------------------

   function To_Element_Array (Source : in Sequence) return Element_Array is
   begin
      return Source.Content (1 .. Source.Length);
   end To_Element_Array;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Source : in Element_Array)
     return Sequence
   is
      Result : Sequence;

   begin
      Allocate (Result, Source'Length);
      Result.Content (1 .. Result.Length) := Source;
      return Result;
   end To_Sequence;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Length : in Natural)
     return Sequence
   is
      Result : Sequence;

   begin
      Allocate (Result, Length);
      return Result;
   end To_Sequence;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : in Sequence;
      Count  : in Natural;
      Pad    : in Element)
      return Sequence
   is
      Result : Sequence;

   begin
      Allocate (Result, Count);

      Result.Content (Result.Length - Count + 1 .. Result.Length)
        := Source.Content (Source.Length - Count + 1 .. Source.Length);

      for I in 1 .. Result.Length - Count loop
         Result.Content (I) := Pad;
      end loop;

      return Result;
   end Tail;

   ----------
   -- Tail --
   ----------

   procedure Tail
     (Source : in out Sequence;
      Count  : in Natural;
      Pad    : in Element) is
   begin
      Source := Tail (Source, Count, Pad);
   end Tail;

end PolyORB.Sequences.Unbounded;

