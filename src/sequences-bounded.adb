------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    S E Q U E N C E S . B O U N D E D                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  This package provides the definitions required by the IDL-to-Ada
--  mapping specification for bounded sequences.  This package is
--  instantiated for each IDL bounded sequence type. This package defines
--  the sequence type and the operations upon it. This package is modeled
--  after Ada.Strings.
--
--  Most query operations are not usable until the sequence object has been
--  initialized through an assignment.
--
--  Value semantics apply to assignment, that is, assignment of a sequence
--  value to a sequence object yields a copy of the value.
--
--  The exception INDEX_ERROR is raised when indexes are not in the range
--  of the object being manipulated.
--
--  The exception CONSTRAINT_ERROR is raised when objects that have not
--  been initialized or assigned to are manipulated.

--  $Id$

with Ada.Unchecked_Deallocation;

package body Sequences.Bounded is

   ------------
   -- Length --
   ------------

   function Length (Source : in Sequence) return Length_Range is
   begin
      return Source.Length;
   end Length;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Element_Array_Access) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation
        (Element_Array, Element_Array_Access);
   begin
      Deallocate (X);
   end Free;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence
     (Source : in Element_Array;
      Drop   : in Truncation := Error)
      return Sequence
   is
      Source_Length : constant Natural := Source'Length;
      Result        : Sequence;
   begin

      if Source_Length <= Max_Length then

         Result.Length := Source_Length;
         Result.Content (1 .. Source_Length) := Source;

      else

         case Drop is

            when Left =>
               Result.Length := Max_Length;
               Result.Content (1 .. Max_Length) :=
                 Source (Source'Last - (Max_Length - 1) .. Source'Last);

            when Right =>
               Result.Length := Max_Length;
               Result.Content (1 .. Max_Length) :=
                 Source (Source'First .. Source'First + Max_Length - 1);

            when Error =>
               raise Length_Error;

         end case;

      end if;

      return Result;
   end To_Sequence;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Length : in Length_Range) return Sequence is

      Result : Sequence;

   begin

      Result.Length := Length;
      Result.Content (1 .. Length) := Null_Element_Array;
      return Result;

   end To_Sequence;

   ----------------------
   -- To_Element_Array --
   ----------------------

   function To_Element_Array (Source : in Sequence) return Element_Array is
   begin

      return Source.Content (1 .. Source.Length);

   end To_Element_Array;

   ------------
   -- Append --
   ------------

   function Append
     (Left, Right : in Sequence;
      Drop        : in Truncation := Error)
      return Sequence
   is

      Left_Length : constant Length_Range := Left.Length;
      Right_Length : constant Length_Range := Right.Length;
      Total_Length : constant Natural := Left_Length + Right_Length;
      Result : Sequence;

   begin

      if Total_Length <= Max_Length then

         --  Resulting sequence will still fit

         Result.Length := Total_Length;
         Result.Content (1 .. Left_Length) :=
           Left.Content (1 .. Left_Length);
         Result.Content (Left_Length + 1 .. Total_Length) :=
           Right.Content (1 .. Right_Length);

      else

         --  Resulting sequence is too large

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if Right_Length < Max_Length then

                  Result.Content (1 .. Max_Length - Right_Length) :=
                    Left.Content (Left_Length -
                                  (Max_Length - Right_Length) + 1 ..
                                  Left_Length);
                  Result.Content
                    (Max_Length - Right_Length + 1 .. Max_Length) :=
                    Right.Content (1 .. Right_Length);

               else

                  Result.Content := Right.Content;

               end if;

            when Sequences.Right =>
               if Left_Length < Max_Length then

                  Result.Content (1 .. Left_Length) :=
                    Left.Content (1 .. Left_Length);
                  Result.Content (Left_Length + 1 .. Max_Length) :=
                    Right.Content (1 .. Max_Length - Left_Length);

               else

                  Result.Content := Left.Content;

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;
      return Result;

   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left  : in Sequence;
      Right : in Element_Array;
      Drop  : in Truncation := Error)
      return Sequence
   is

      Left_Length : constant Length_Range := Left.Length;
      Right_Length : constant Length_Range := Right'Length;
      Total_Length : constant Natural := Left_Length + Right_Length;
      Result : Sequence;

   begin

      if Total_Length <= Max_Length then

         --  Resulting sequence will still fit

         Result.Length := Total_Length;
         Result.Content (1 .. Left_Length) :=
           Left.Content (1 .. Left_Length);
         Result.Content (Left_Length + 1 .. Total_Length) := Right;

      else

         --  Resulting sequence is too large

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if Right_Length < Max_Length then

                  Result.Content (1 .. Max_Length - Right_Length) :=
                    Left.Content (Left_Length -
                                  (Max_Length - Right_Length) + 1 ..
                                  Left_Length);
                  Result.Content
                    (Max_Length - Right_Length + 1 .. Max_Length) :=
                    Right;

               else

                  Result.Content (1 .. Max_Length) :=
                    Right (Right'Last - (Max_Length - 1) .. Right'Last);

               end if;

            when Sequences.Right =>
               if Left_Length < Max_Length then

                  Result.Content (1 .. Left_Length) :=
                    Left.Content (1 .. Left_Length);
                  Result.Content (Left_Length + 1 .. Max_Length) :=
                    Right (Right'First .. Right'First - 1 +
                           Max_Length - Left_Length);

               else

                  Result.Content := Left.Content;

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;
      return Result;

   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left  : in Element_Array;
      Right : in Sequence;
      Drop  : in Truncation := Error)
      return Sequence
   is

      Left_Length : constant Length_Range := Left'Length;
      Right_Length : constant Length_Range := Right.Length;
      Total_Length : constant Natural := Left_Length + Right_Length;
      Result : Sequence;

   begin

      if Total_Length <= Max_Length then

         --  Resulting sequence will still fit

         Result.Length := Total_Length;
         Result.Content (1 .. Left_Length) := Left;
         Result.Content (Left_Length + 1 .. Total_Length) :=
           Right.Content (1 .. Right_Length);

      else

         --  Resulting sequence is too large

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if Right_Length < Max_Length then

                  Result.Content (1 .. Max_Length - Right_Length) :=
                    Left (Left'Last - (Max_Length - Right_Length) + 1 ..
                          Left'Last);
                  Result.Content
                    (Max_Length - Right_Length + 1 .. Max_Length) :=
                    Right.Content (1 .. Right_Length);

               else

                  Result.Content (1 .. Max_Length) :=
                    Right.Content (Right_Length - Max_Length + 1 ..
                                   Right_Length);

               end if;

            when Sequences.Right =>
               if Left_Length < Max_Length then

                  Result.Content (1 .. Left_Length) := Left;
                  Result.Content (Left_Length + 1 .. Max_Length) :=
                    Right.Content (1 .. Max_Length - Left_Length);

               else

                  Result.Content (1 .. Max_Length) :=
                    Left (Left'First .. Left'First + Max_Length - 1);

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

      return Result;
   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left  : in Sequence;
      Right : in Element;
      Drop  : in Truncation := Error)
      return Sequence
   is

      Left_Length : constant Length_Range := Left.Length;
      Result : Sequence;

   begin

      if Left_Length < Max_Length then

         --  Resulting sequence will still fit

         Result.Length := Left_Length + 1;
         Result.Content (1 .. Left_Length) :=
           Left.Content (1 .. Left_Length);
         Result.Content (Left_Length + 1) := Right;
         return Result;

      else

         --  Resulting sequence is too large

         case Drop is

            when Sequences.Left =>
               Result.Length := Max_Length;
               Result.Content (1 .. Max_Length - 1) :=
                 Left.Content (2 .. Max_Length);
               Result.Content (Max_Length) := Right;
               return Result;

            when Sequences.Right =>
               return Left;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Left  : in Element;
      Right : in Sequence;
      Drop  : in Truncation := Error)
      return Sequence
   is

      Right_Length : constant Length_Range := Right.Length;
      Result : Sequence;

   begin

      if Right_Length < Max_Length then

         --  Resulting sequence will still fit

         Result.Length := Right_Length + 1;
         Result.Content (1) := Left;
         Result.Content (2 .. Right_Length + 1) :=
           Right.Content (1 .. Right_Length);
         return Result;

      else

         --  Resulting sequence is too large

         case Drop is

            when Sequences.Left =>
               return Right;

            when Sequences.Right =>
               Result.Length := Max_Length;
               Result.Content (1) := Left;
               Result.Content (2 .. Max_Length) :=
                 Right.Content (1 .. Max_Length - 1);
               return Result;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : in Sequence;
      Drop     : in Truncation := Error)
   is

      Source_Length : constant Length_Range := Source.Length;
      New_Length : constant Length_Range := New_Item.Length;
      Total_Length : constant Natural := Source_Length + New_Length;

   begin

      if Total_Length <= Max_Length then

         --  Resulting sequence will still fit

         Source.Length := Total_Length;
         Source.Content (Source_Length + 1 .. Total_Length) :=
           New_Item.Content (1 .. New_Length);

      else

         --  Resulting sequence is too large

         Source.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if New_Length < Max_Length then

                  Source.Content (1 .. Max_Length - New_Length) :=
                    Source.Content (Source_Length -
                                    (Max_Length - New_Length) + 1 ..
                                    Source_Length);
                  Source.Content
                    (Max_Length - New_Length + 1 .. Max_Length) :=
                    New_Item.Content (1 .. New_Length);

               else -- New_Length = Max_Length

                  Source.Content := New_Item.Content;

               end if;

            when Sequences.Right =>
               if Source_Length < Max_Length then

                  Source.Content (Source_Length + 1 .. Max_Length) :=
                    New_Item.Content (1 .. Max_Length - Source_Length);

               else -- Source_Length = Max_Length

                  null; -- do nothing

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : in Element_Array;
      Drop     : in Truncation := Error)
   is

      Source_Length : constant Length_Range := Source.Length;
      New_Length : constant Length_Range := New_Item'Length;
      Total_Length : constant Natural := Source_Length + New_Length;

   begin

      if Total_Length <= Max_Length then

         --  Resulting sequence will still fit

         Source.Length := Total_Length;
         Source.Content (Source_Length + 1 .. Total_Length) := New_Item;

      else

         --  Resulting sequence is too large

         Source.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if New_Length < Max_Length then

                  Source.Content (1 .. Max_Length - New_Length) :=
                    Source.Content (Source_Length -
                                    (Max_Length - New_Length) + 1 ..
                                    Source_Length);
                  Source.Content
                    (Max_Length - New_Length + 1 .. Max_Length) :=
                    New_Item;

               else

                  Source.Content (1 .. Max_Length) :=
                    New_Item (New_Item'Last - Max_Length + 1 ..
                              New_Item'Last);

               end if;

            when Sequences.Right =>
               if Source_Length < Max_Length then

                  Source.Content (Source_Length + 1 .. Max_Length) :=
                    New_Item (New_Item'First ..
                              New_Item'First + Max_Length -
                              Source_Length - 1);

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Sequence;
      New_Item : in Element;
      Drop     : in Truncation := Error) is

      Source_Length : constant Length_Range := Source.Length;

   begin

      if Source_Length < Max_Length then

         --  Resulting sequence will still fit

         Source.Length := Source_Length + 1;
         Source.Content (Source_Length + 1) := New_Item;

      else

         --  Resulting sequence is too large

         case Drop is

            when Sequences.Left =>
               Source.Content (1 .. Max_Length - 1) :=
                 Source.Content (2 .. Max_Length);
               Source.Content (Max_Length) := New_Item;

            when Sequences.Right =>
               null; -- do nothing

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Append;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : in Sequence) return Sequence is

      Left_Length : constant Length_Range := Left.Length;
      Right_Length : constant Length_Range := Right.Length;
      Total_Length : constant Natural := Left_Length + Right_Length;
      Result : Sequence;

   begin

      if Total_Length <= Max_Length then

         Result.Length := Total_Length;
         Result.Content (1 .. Left_Length) :=
           Left.Content (1 .. Left_Length);
         Result.Content (Left_Length + 1 .. Total_Length) :=
           Right.Content (1 .. Right_Length);

      else

         raise Length_Error;

      end if;
      return Result;

   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : in Sequence; Right : in Element_Array)
                 return Sequence is

      Left_Length : constant Length_Range := Left.Length;
      Total_Length : constant Natural := Left_Length + Right'Length;
      Result : Sequence;

   begin

      if Total_Length <= Max_Length then

         Result.Length := Total_Length;
         Result.Content (1 .. Left_Length) :=
           Left.Content (1 .. Left_Length);
         Result.Content (Left_Length + 1 .. Total_Length) := Right;

      else

         raise Length_Error;

      end if;

      return Result;

   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : in Element_Array; Right : in Sequence)
                 return Sequence is

      Left_Length : constant Length_Range := Left'Length;
      Right_Length : constant Length_Range := Right.Length;
      Total_Length : constant Natural := Left_Length + Right_Length;
      Result : Sequence;

   begin

      if Total_Length <= Max_Length then

         Result.Length := Total_Length;
         Result.Content (1 .. Left_Length) := Left;
         Result.Content (Left_Length + 1 .. Total_Length) :=
           Right.Content (1 .. Right_Length);

      else

         raise Length_Error;

      end if;

      return Result;

   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : in Sequence; Right : in Element) return Sequence is

      Left_Length : constant Length_Range := Left.Length;
      Result : Sequence;

   begin

      if Left_Length < Max_Length then

         Result.Length := Left_Length + 1;
         Result.Content (1 .. Left_Length) :=
           Left.Content (1 .. Left_Length);
         Result.Content (Result.Length) := Right;

      else

         raise Length_Error;

      end if;
      return Result;

   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : in Element; Right : in Sequence) return Sequence is

      Right_Length : constant Length_Range := Right.Length;
      Result : Sequence;

   begin

      if Right_Length < Max_Length then

         Result.Length := Right_Length + 1;
         Result.Content (1) := Left;
         Result.Content (2 .. Result.Length) :=
           Right.Content (1 .. Right_Length);

      else

         raise Length_Error;

      end if;
      return Result;

   end "&";

   ----------------
   -- Element_Of --
   ----------------

   function Element_Of
     (Source : in Sequence; Index : in Positive) return Element is
   begin

      if Index <= Source.Length then

         return Source.Content (Index);

      else

         raise Index_Error;

      end if;

   end Element_Of;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in out Sequence;
      Index  : in Positive;
      By     : in Element)
   is
   begin

      if Index <= Source.Length then

         Source.Content (Index) := By;

      else

         raise Index_Error;

      end if;

   end Replace_Element;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : in Sequence;
      Low    : in Positive;
      High   : in Natural)
      return Element_Array
   is
   begin

      if Low <= Source.Length and then High <= Source.Length then

         return Source.Content (Low .. High);

      else

         raise Index_Error;

      end if;

   end Slice;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Sequence) return Boolean is
   begin

      return Left.Length = Right.Length and then
        Left.Content (1 .. Left.Length) =
        Right.Content (1 .. Right.Length);

   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : in Sequence; Right : in Element_Array)
                 return Boolean is
   begin

      return Left.Length = Right'Length and then
        Left.Content (1 .. Left.Length) = Right;

   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : in Element_Array; Right : in Sequence)
                 return Boolean is
   begin

      return Left'Length = Right.Length and then
        Left = Right.Content (1 .. Right.Length);

   end "=";

   -----------
   -- Index --
   -----------

   function Index
     (Source  : in Sequence;
      Pattern : in Element_Array;
      Going   : in Direction := Forward)
      return Natural
   is
   begin

      if Pattern = Null_Element_Array then

         raise Pattern_Error;

      end if;

      case Going is

         when Forward =>
            for J in 1 .. Source.Length - Pattern'Length + 1 loop

               if Pattern = Source.Content (J ..
                                            J + Pattern'Length - 1) then
                  return J;
               end if;

            end loop;

         when Backward =>
            for J in reverse 1 .. Source.Length - Pattern'Length + 1 loop

               if Pattern = Source.Content (J ..
                                            J + Pattern'Length - 1) then
                  return J;
               end if;

            end loop;

      end case;

      --  No match found yet
      return 0;

   end Index;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : in Sequence;
      Pattern : in Element_Array)
      return Natural
   is

      N : Natural;
      J : Natural;

   begin

      if Pattern = Null_Element_Array then
         raise Pattern_Error;
      end if;

      N := 0;
      J := 1;

      while J <= Source.Length - (Pattern'Length - 1) loop

         if Source.Content (J .. J + Pattern'Length - 1) = Pattern then
            N := N + 1;
            J := J + Pattern'Length;
         else
            J := J + 1;
         end if;

      end loop;
      return N;

   end Count;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : in Sequence;
      Low    : in Positive;
      High   : in Natural;
      By     : in Element_Array;
      Drop   : in Truncation := Error)
      return Sequence
   is

      Source_Length : constant Length_Range := Source.Length;
      By_Length     : constant Integer := By'Length;
      Total_Length  : constant Integer :=
        Source_Length + By_Length + Low - High - 1;
      Drop_Length   : constant Integer := Total_Length - Max_Length;
      Result : Sequence;

   begin

      if Low > Source_Length + 1 then

         raise Index_Error;

      elsif High < Low then

         return Insert (Source => Source,
                        Before => Low,
                        New_Item => By,
                        Drop => Drop);

      elsif Total_Length > Max_Length then

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               Result.Content (Max_Length - (Source_Length - High) + 1 ..
                               Max_Length) :=
                 Source.Content (High + 1 .. Source_Length);
               if Drop_Length >= Low - 1 then

                  Result.Content
                    (1 .. Max_Length - (Source_Length - High)) :=
                    By (By'Last -
                        (Max_Length - (Source_Length - High)) + 1 ..
                        By'Last);

               else

                  Result.Content
                    (Low - Drop_Length ..
                     Max_Length - (Source_Length - High)) := By;
                  Result.Content (1 .. Low - (Drop_Length + 1)) :=
                    Source.Content (Drop_Length + 1 .. Low - 1);

               end if;

            when Sequences.Right =>
               Result.Content (1 .. Low - 1) :=
                 Source.Content (1 .. Low - 1);
               if Drop_Length > Source_Length - High then

                  Result.Content (Low .. Max_Length) :=
                    By (By'First .. By'First + Max_Length - Low);

               else

                  Result.Content (Low .. Low + By_Length - 1) := By;
                  Result.Content (Low + By_Length .. Max_Length) :=
                    Source.Content (High + 1 ..
                                    Source_Length - Drop_Length);

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      else

         Result.Length := Total_Length;
         Result.Content (1 .. Low - 1) := Source.Content (1 .. Low - 1);
         Result.Content (Low .. Low + By_Length - 1) := By;
         Result.Content (Low + By_Length .. Total_Length) :=
           Source.Content (High + 1 .. Source_Length);

      end if;

      return Result;

   end Replace_Slice;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Source : in out Sequence;
      Low    : in Positive;
      High   : in Natural;
      By     : in Element_Array;
      Drop   : in Truncation := Error)
   is
      Source_Length : constant Length_Range := Source.Length;
      By_Length     : constant Integer := By'Length;
      Total_Length  : constant Integer :=
        Source_Length + By_Length + Low - High - 1;
      Drop_Length   : constant Integer := Total_Length - Max_Length;
   begin

      if Low > Source_Length + 1 then

         raise Index_Error;

      elsif High < Low then

         Insert (Source => Source,
                 Before => Low,
                 New_Item => By,
                 Drop => Drop);

      elsif Total_Length > Max_Length then

         Source.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               Source.Content (Max_Length - (Source_Length - High) + 1 ..
                               Max_Length) :=
                 Source.Content (High + 1 .. Source_Length);
               if Drop_Length >= Low - 1 then

                  Source.Content
                    (1 .. Max_Length - (Source_Length - High)) :=
                    By (By'Last -
                        (Max_Length - (Source_Length - High)) + 1 ..
                        By'Last);

               else

                  Source.Content (1 .. Low - (Drop_Length + 1)) :=
                    Source.Content (Drop_Length + 1 .. Low - 1);
                  Source.Content
                    (Low - Drop_Length ..
                     Max_Length - (Source_Length - High)) := By;

               end if;

            when Sequences.Right =>
               if Drop_Length > Source_Length - High then

                  Source.Content (Low .. Max_Length) :=
                    By (By'First .. By'First + Max_Length - Low);

               else

                  Source.Content (Low + By_Length .. Max_Length) :=
                    Source.Content (High + 1 ..
                                    Source_Length - Drop_Length);
                  Source.Content (Low .. Low + By_Length - 1) := By;

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      else

         Source.Length := Total_Length;
         Source.Content (Low .. Low + By_Length - 1) := By;
         Source.Content (Low + By_Length .. Total_Length) :=
           Source.Content (High + 1 .. Source_Length);

      end if;

   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : in Sequence;
      Before   : in Positive;
      New_Item : in Element_Array;
      Drop     : in Truncation := Error)
      return Sequence
   is
      Source_Length : constant Length_Range := Source.Length;
      New_Length    : constant Natural := New_Item'Length;
      Total_Length  : constant Natural := Source_Length + New_Length;
      Drop_Length   : constant Integer := Total_Length - Max_Length;
      Result        : Sequence;
   begin

      if Before > Source_Length + 1 then

         raise Index_Error;

      elsif Total_Length > Max_Length then

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               Result.Content
                 (Max_Length - Source_Length + Before .. Max_Length) :=
                 Source.Content (Before .. Source_Length);
               if Drop_Length >= Before - 1 then

                  Result.Content
                    (1 .. Max_Length - Source_Length + Before - 1) :=
                    New_Item (New_Item'Last - Max_Length +
                              Source_Length - (Before - 1) + 1 ..
                              New_Item'Last);

               else

                  Result.Content
                    (Before - Drop_Length ..
                     Max_Length - Source_Length + Before - 1) :=
                    New_Item;
                  Result.Content (1 .. Before - 1 - Drop_Length) :=
                    Source.Content (Drop_Length + 1 .. Before - 1);

               end if;

            when Sequences.Right =>
               Result.Content (1 .. Before - 1) :=
                 Source.Content (1 .. Before - 1);
               if Drop_Length > (Source_Length - (Before - 1)) then

                  Result.Content (Before .. Max_Length) :=
                    New_Item (New_Item'First ..
                              New_Item'First + Max_Length - Before);

               else

                  Result.Content (Before .. Before + New_Length - 1) :=
                    New_Item;
                  Result.Content (Before + New_Length .. Max_Length) :=
                    Source.Content (Before ..
                                    Source_Length - Drop_Length);

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      else

         Result.Length := Total_Length;
         Result.Content (1 .. Before - 1) :=
           Source.Content (1 .. Before - 1);
         Result.Content (Before .. Before + New_Length - 1) := New_Item;
         Result.Content (Before + New_Length .. Total_Length) :=
           Source.Content (Before .. Source_Length);

      end if;

      return Result;

   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Source   : in out Sequence;
      Before   : in Positive;
      New_Item : in Element_Array;
      Drop     : in Truncation := Error)
   is
      Source_Length : constant Length_Range := Source.Length;
      New_Length    : constant Natural := New_Item'Length;
      Total_Length  : constant Natural := Source_Length + New_Length;
      Drop_Length   : constant Integer := Total_Length - Max_Length;
   begin

      if Before > Source_Length + 1 then

         raise Index_Error;

      elsif Total_Length > Max_Length then

         Source.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               Source.Content
                 (Max_Length - Source_Length + Before .. Max_Length) :=
                 Source.Content (Before .. Source_Length);
               if Drop_Length >= Before - 1 then

                  Source.Content
                    (1 .. Max_Length - Source_Length + Before - 1) :=
                    New_Item (New_Item'Last - Max_Length +
                              Source_Length - (Before - 1) + 1 ..
                              New_Item'Last);

               else

                  Source.Content (1 .. Before - 1 - Drop_Length) :=
                    Source.Content (Drop_Length + 1 .. Before - 1);
                  Source.Content
                    (Before - Drop_Length ..
                     Max_Length - Source_Length + (Before - 1)) :=
                    New_Item;

               end if;

            when Sequences.Right =>
               if Drop_Length > (Source_Length - (Before - 1)) then

                  Source.Content (Before .. Max_Length) :=
                    New_Item (New_Item'First ..
                              New_Item'First + Max_Length - Before);

               else

                  Source.Content (Before + New_Length .. Max_Length) :=
                    Source.Content (Before ..
                                    Source_Length - Drop_Length);
                  Source.Content (Before .. Before + New_Length - 1) :=
                    New_Item;

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      else

         Source.Length := Total_Length;
         Source.Content (Before + New_Length .. Total_Length) :=
           Source.Content (Before .. Source_Length);
         Source.Content (Before .. Before + New_Length - 1) := New_Item;

      end if;

   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : in Sequence;
      Position : in Positive;
      New_Item : in Element_Array;
      Drop     : in Truncation := Error)
      return Sequence
   is
      Source_Length : constant Length_Range := Source.Length;
      New_Length    : constant Integer := New_Item'Length;
      Endpos        : constant Natural := Position + New_Length - 1;
      Drop_Length   : constant Integer := Endpos - Max_Length;
      Result        : Sequence;
   begin

      if Position > Source_Length + 1 then

         raise Index_Error;

      elsif New_Item'Length = 0 then

         return Source;

      elsif Endpos <= Source_Length then

         Result.Length := Source.Length;
         Result.Content (1 .. Source_Length) :=
           Source.Content (1 .. Source_Length);
         Result.Content (Position .. Endpos) := New_Item;
         return Result;

      elsif Endpos <= Max_Length then

         Result.Length := Endpos;
         Result.Content (1 .. Position - 1) :=
           Source.Content (1 .. Position - 1);
         Result.Content (Position .. Endpos) := New_Item;
         return Result;

      else

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if New_Length >= Max_Length then

                  Result.Content (1 .. Max_Length) :=
                    New_Item (New_Item'Last - Max_Length + 1 ..
                              New_Item'Last);
                  return Result;

               else

                  Result.Content (1 .. Max_Length - New_Length) :=
                    Source.Content (Drop_Length + 1 .. Position - 1);
                  Result.Content
                    (Max_Length - New_Length + 1 .. Max_Length) :=
                    New_Item;
                  return Result;

               end if;

            when Sequences.Right =>
               Result.Content (1 .. Position - 1) :=
                 Source.Content (1 .. Position - 1);
               Result.Content (Position .. Max_Length) :=
                 New_Item (New_Item'First .. New_Item'Last - Drop_Length);
               return Result;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Overwrite;

   ---------------
   -- Overwrite --
   ---------------

   procedure Overwrite
     (Source   : in out Sequence;
      Position : in Positive;
      New_Item : in Element_Array;
      Drop     : in Truncation := Error)
   is
      Source_Length : constant Length_Range := Source.Length;
      New_Length    : constant Integer := New_Item'Length;
      Endpos        : constant Natural := Position + New_Length - 1;
      Drop_Length   : constant Integer := Endpos - Max_Length;
   begin

      if Position > Source_Length + 1 then

         raise Index_Error;

      elsif Endpos <= Source_Length then

         Source.Content (Position .. Endpos) := New_Item;

      elsif Endpos <= Max_Length then

         Source.Length := Endpos;
         Source.Content (Position .. Endpos) := New_Item;

      else

         Source.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if New_Length > Max_Length then

                  Source.Content (1 .. Max_Length) :=
                    New_Item (New_Item'Last - Max_Length + 1 ..
                              New_Item'Last);

               else

                  Source.Content (1 .. Max_Length - New_Length) :=
                    Source.Content (Drop_Length + 1 .. Position - 1);
                  Source.Content
                    (Max_Length - New_Length + 1 .. Max_Length) :=
                    New_Item;

               end if;

            when Sequences.Right =>
               Source.Content (Position .. Max_Length) :=
                 New_Item (New_Item'First .. New_Item'Last - Drop_Length);

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : in Sequence;
      From    : in Positive;
      Through : in Natural)
      return Sequence
   is
      Source_Length : constant Length_Range := Source.Length;
      Num_Delete    : constant Integer := Through - From + 1;
      Result        : Sequence;
   begin

      if Num_Delete <= 0 then

         return Source;

      elsif From > Source_Length + 1 then

         raise Index_Error;

      elsif Through >= Source_Length then

         Result.Length := From - 1;
         Result.Content (1 .. From - 1) := Source.Content (1 .. From - 1);
         return Result;

      else

         Result.Length := Source_Length - Num_Delete;
         Result.Content (1 .. From - 1) := Source.Content (1 .. From - 1);
         Result.Content (From .. Result.Length) :=
           Source.Content (Through + 1 .. Source_Length);
         return Result;

      end if;

   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Source  : in out Sequence;
      From    : in Positive;
      Through : in Natural)
   is
      Source_Length : constant Length_Range := Source.Length;
      Num_Delete    : constant Integer := Through - From + 1;
   begin

      if Num_Delete <= 0 then

         return;

      elsif From > Source_Length + 1 then

         raise Index_Error;

      elsif Through <= Source_Length then

         Source.Length := From - 1;

      else

         Source.Length := Source_Length - Num_Delete;
         Source.Content (From .. Source.Length) :=
           Source.Content (Through + 1 .. Source_Length);

      end if;

   end Delete;

   ----------
   -- Head --
   ----------

   function Head
     (Source : in Sequence;
      Count  : in Natural;
      Pad    : in Element;
      Drop   : in Truncation := Error)
      return Sequence
   is
      Source_Length : constant Natural := Source.Length;
      Npad          : constant Integer := Count - Source_Length;
      Result        : Sequence;
   begin

      if Npad <= 0 then

         Result.Length := Count;
         Result.Content (1 .. Count) := Source.Content (1 .. Count);

      elsif Count <= Max_Length then

         Result.Length := Count;
         Result.Content (1 .. Source_Length) :=
           Source.Content (1 .. Source_Length);
         Result.Content (Source_Length + 1 .. Count) := (others => Pad);

      else

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if Npad >= Max_Length then

                  Result.Content := (others => Pad);

               else

                  Result.Content (1 .. Max_Length - Npad) :=
                    Source.Content (Count - Max_Length + 1 ..
                                    Source_Length);
                  Result.Content (Max_Length - Npad + 1 .. Max_Length) :=
                    (others => Pad);

               end if;

            when Sequences.Right =>
               Result.Content (1 .. Source_Length) :=
                 Source.Content (1 .. Source_Length);
               Result.Content (Source_Length + 1 .. Max_Length) :=
                 (others => Pad);

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;
      return Result;

   end Head;

   ----------
   -- Head --
   ----------

   procedure Head
     (Source : in out Sequence;
      Count  : in Natural;
      Pad    : in Element;
      Drop   : in Truncation := Error)
   is
      Source_Length : constant Natural := Source.Length;
      Npad          : constant Integer := Count - Source_Length;
      Temp          : Element_Array (1 .. Max_Length);
   begin

      if Npad <= 0 then

         Source.Length := Count;

      elsif Count <= Max_Length then

         Source.Length := Count;
         Source.Content (Source_Length + 1 .. Count) := (others => Pad);

      else

         Source.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               if Npad > Max_Length then

                  Source.Content := (others => Pad);

               else

                  Temp := Source.Content;
                  Source.Content (1 .. Max_Length - Npad) :=
                    Temp (Count - Max_Length + 1 .. Source_Length);

                  for J in Max_Length - Npad + 1 .. Max_Length loop
                     Source.Content (J) := Pad;
                  end loop;

               end if;

            when Sequences.Right =>
               Source.Content (Source_Length + 1 .. Max_Length) :=
                 (others => Pad);

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : in Sequence;
      Count  : in Natural;
      Pad    : in Element;
      Drop   : in Truncation := Error)
      return Sequence
   is
      Source_Length : constant Natural := Source.Length;
      Npad          : constant Integer := Count - Source_Length;
      Result        : Sequence;
   begin

      if Npad <= 0 then

         Result.Length := Count;
         Result.Content (1 .. Count) :=
           Source.Content (Source_Length - Count + 1 .. Source_Length);

      elsif Count <= Max_Length then

         Result.Length := Count;
         Result.Content (1 .. Npad) := (others => Pad);
         Result.Content (Npad + 1 .. Count) :=
           Source.Content (1 .. Source_Length);

      else

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               Result.Content (1 .. Max_Length - Source_Length) :=
                 (others => Pad);
               Result.Content
                 (Max_Length - Source_Length + 1 .. Max_Length) :=
                 Source.Content (1 .. Source_Length);

            when Sequences.Right =>
               if Npad >= Max_Length then

                  Result.Content := (others => Pad);

               else

                  Result.Content (1 .. Npad) := (others => Pad);
                  Result.Content (Npad + 1 .. Max_Length) :=
                    Source.Content (1 .. Max_Length - Npad);

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

      return Result;

   end Tail;

   --

   procedure Tail (Source : in out Sequence;
                   Count : in Natural;
                   Pad : in Element;
                   Drop : in Truncation := Error) is

      Source_Length : constant Natural := Source.Length;
      Npad : constant Integer := Count - Source_Length;
      Temp : Element_Array (1 .. Max_Length) := Source.Content;

   begin

      if Npad <= 0 then

         Source.Length := Count;
         Source.Content (1 .. Count) :=
           Temp (Source_Length - (Count - 1) .. Source_Length);

      elsif Count <= Max_Length then

         Source.Length := Count;
         Source.Content (1 .. Npad) := (others => Pad);
         Source.Content (Npad + 1 .. Count) := Temp (1 .. Source_Length);

      else

         Source.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               for J in 1 .. Max_Length - Source_Length loop
                  Source.Content (J) := Pad;
               end loop;
               Source.Content
                 (Max_Length - Source_Length + 1 .. Max_Length) :=
                 Temp (1 .. Source_Length);

            when Sequences.Right =>
               if Npad >= Max_Length then

                  Source.Content := (others => Pad);

               else

                  Source.Content (1 .. Npad) := (others => Pad);
                  Source.Content (Npad + 1 .. Max_Length) :=
                    Temp (1 .. Max_Length - Npad);

               end if;

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;

   end Tail;

   ---------
   -- "*" --
   ---------

   function "*" (Left : in Natural; Right : in Element) return Sequence is

      Result : Sequence;

   begin

      if Left <= Max_Length then

         Result.Length := Left;
         for J in 1 .. Left loop
            Result.Content (J) := Right;
         end loop;

      else

         raise Length_Error;

      end if;

      return Result;

   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : in Natural; Right : in Element_Array)
                 return Sequence is

      Right_Length : constant Natural := Right'Length;
      Total_Length : constant Natural := Left * Right_Length;
      Pos : Positive := 1;
      Result : Sequence;

   begin

      if Total_Length <= Max_Length then

         Result.Length := Total_Length;
         if Total_Length > 0 then

            for J in 1 .. Left loop

               Result.Content (Pos .. Pos + Right_Length - 1) := Right;
               Pos := Pos + Right_Length;

            end loop;

         end if;

      else

         raise Index_Error;

      end if;
      return Result;

   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : in Natural; Right : in Sequence) return Sequence is

      Right_Length : constant Length_Range := Right.Length;
      Total_Length : constant Natural := Left * Right_Length;
      Pos : Positive := 1;
      Result : Sequence;

   begin

      if Total_Length <= Max_Length then

         Result.Length := Total_Length;

         if Total_Length > 0 then

            for J in 1 .. Left loop

               Result.Content (Pos .. Pos + Right_Length - 1) :=
                 Right.Content (1 .. Right_Length);
               Pos := Pos + Right_Length;

            end loop;

         end if;

      else

         raise Length_Error;

      end if;
      return Result;

   end "*";

   ---------------
   -- Replicate --
   ---------------

   function Replicate
     (Count : in Natural;
      Item  : in Element;
      Drop  : in Truncation := Error)
      return Sequence
   is
      Result : Sequence;
   begin

      case Drop is

         when Error =>
            raise Length_Error;

         when others =>
            if Count <= Max_Length then

               Result.Length := Count;

            else

               Result.Length := Max_Length;

            end if;

      end case;
      Result.Content (1 .. Result.Length) := (others => Item);
      return Result;

   end Replicate;

   ---------------
   -- Replicate --
   ---------------

   function Replicate
     (Count : in Natural;
      Item  : in Element_Array;
      Drop  : in Truncation := Error)
      return Sequence
   is
      Item_Length  : constant Integer := Item'Length;
      Total_Length : constant Integer := Count * Item_Length;
      Result       : Sequence;
      Indx         : Positive;
   begin

      if Total_Length <= Max_Length then

         Result.Length := Total_Length;
         if Total_Length > 0 then

            Indx := 1;
            for J in 1 .. Count loop
               Result.Content (Indx .. Indx + Item_Length - 1) := Item;
               Indx := Indx + Item_Length;
            end loop;

         end if;

      else

         Result.Length := Max_Length;
         case Drop is

            when Sequences.Left =>
               Indx := Max_Length;
               while Indx - Item_Length >= 1 loop

                  Result.Content (Indx - (Item_Length - 1) .. Indx) :=
                    Item;
                  Indx := Indx - Item_Length;

               end loop;
               Result.Content (1 .. Indx) :=
                 Item (Item'Last - Indx + 1 .. Item'Last);

            when Sequences.Right =>
               Indx := 1;
               while Indx + Item_Length <= Max_Length + 1 loop

                  Result.Content (Indx .. Indx + Item_Length - 1) := Item;
                  Indx := Indx + Item_Length;

               end loop;
               Result.Content (Indx .. Max_Length) :=
                 Item (Item'First .. Item'First + Max_Length - Indx);

            when Sequences.Error =>
               raise Length_Error;

         end case;

      end if;
      return Result;

   end Replicate;

   ---------------
   -- Replicate --
   ---------------

   function Replicate
     (Count : in Natural;
      Item  : in Sequence;
      Drop  : in Truncation := Error)
      return Sequence
   is
   begin

      return Replicate (Count => Count,
                        Item => Item.Content (1 .. Item.Length),
                        Drop => Drop);

   end Replicate;

end Sequences.Bounded;

