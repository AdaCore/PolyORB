------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . S E Q U E N C E S . B O U N D E D             --
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

--  This package provides the definitions required by the IDL-to-Ada
--  mapping specification for bounded sequences. This package is
--  instantiated for each IDL bounded sequence type. This package
--  defines the sequence type and the operations upon it. This package
--  is modeled after Ada.Strings and is compliant with the specifications
--  of CORBA.Sequences.Bounded defined in the CORBA Ada Mapping.
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

generic

    type Element is private;
    Max : Positive;    -- Maximum length of the bounded sequence

package PolyORB.Sequences.Bounded is

   pragma Preelaborate;

   Max_Length : constant Positive := Max;

   type Element_Array is array (Positive range <>) of Element;

   Null_Element_Array : Element_Array (1 .. 0);

   type Sequence is private;

   Null_Sequence : constant Sequence;

   subtype Length_Range is Natural range 0 .. Max_Length;

   function Length (Source : Sequence) return Length_Range;

   type Element_Array_Access is access all Element_Array;

   procedure Free (X : in out Element_Array_Access);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Sequence
     (Source : Element_Array;
      Drop   : Truncation := Error)
      return Sequence;

   function To_Sequence
     (Length : Length_Range)
      return Sequence;

   procedure Set
     (Item   : in out Sequence;
      Source : Element_Array;
      Drop   : Truncation := Error);

   function To_Element_Array
     (Source : Sequence)
      return Element_Array;

   function Append
     (Left, Right : Sequence;
      Drop        : Truncation := Error)
      return Sequence;

   function Append
     (Left  : Sequence;
      Right : Element_Array;
      Drop  : Truncation := Error)
      return Sequence;

   function Append
     (Left  : Element_Array;
      Right : Sequence;
      Drop  : Truncation := Error)
      return Sequence;

   function Append
     (Left  : Sequence;
      Right : Element;
      Drop  : Truncation := Error)
      return Sequence;

   function Append
     (Left  : Element;
      Right : Sequence;
      Drop  : Truncation := Error)
      return Sequence;

   procedure Append
     (Source   : in out Sequence;
      New_Item : Sequence;
      Drop     : Truncation := Error);

   procedure Append
     (Source   : in out Sequence;
      New_Item : Element_Array;
      Drop     : Truncation := Error);

   procedure Append
     (Source   : in out Sequence;
      New_Item : Element;
      Drop     : Truncation := Error);

   function "&" (Left, Right : Sequence) return Sequence;

   function "&"
     (Left  : Sequence;
      Right : Element_Array)
     return Sequence;

   function "&"
     (Left  : Element_Array;
      Right : Sequence)
     return Sequence;

   function "&"
     (Left  : Sequence;
      Right : Element)
     return Sequence;

   function "&"
     (Left  : Element;
      Right : Sequence)
     return Sequence;

   function Element_Of
     (Source : Sequence;
      Index  : Positive)
     return Element;

   function Get_Element
     (Source : Sequence;
      Index  : Positive)
     return Element
     renames Element_Of;

   procedure Replace_Element
     (Source : in out Sequence;
      Index  : Positive;
      By     : Element);

   function Slice
     (Source : Sequence;
      Low    : Positive;
      High   : Natural)
      return Element_Array;

   function "=" (Left, Right : Sequence)
                return Boolean;

   function "="
     (Left  : Sequence;
      Right : Element_Array)
     return Boolean;

   function "="
     (Left  : Element_Array;
      Right : Sequence)
     return Boolean;

   ----------------------
   -- Search functions --
   ----------------------

   function Index
     (Source  : Sequence;
      Pattern : Element_Array;
      Going   : Direction := Forward)
      return Natural;

   function Count
     (Source  : Sequence;
      Pattern : Element_Array)
      return Natural;

   -----------------------------------------
   -- Sequence transformation subprograms --
   -----------------------------------------

   function Replace_Slice
     (Source : Sequence;
      Low    : Positive;
      High   : Natural;
      By     : Element_Array;
      Drop   : Truncation := Error)
      return Sequence;

   procedure Replace_Slice
     (Source : in out Sequence;
      Low    : Positive;
      High   : Natural;
      By     : Element_Array;
      Drop   : Truncation := Error);

   function Insert
     (Source   : Sequence;
      Before   : Positive;
      New_Item : Element_Array;
      Drop     : Truncation := Error)
      return Sequence;

   procedure Insert
     (Source   : in out Sequence;
      Before   : Positive;
      New_Item : Element_Array;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : Sequence;
      Position : Positive;
      New_Item : Element_Array;
      Drop     : Truncation := Error)
      return Sequence;

   procedure Overwrite
     (Source   : in out Sequence;
      Position : Positive;
      New_Item : Element_Array;
      Drop     : Truncation := Error);

   function Delete
     (Source  : Sequence;
      From    : Positive;
      Through : Natural)
      return Sequence;

   procedure Delete
     (Source  : in out Sequence;
      From    : Positive;
      Through : Natural);

   -----------------------------------
   -- Sequence selector subprograms --
   -----------------------------------

   function Head
     (Source : Sequence;
      Count  : Natural;
      Pad    : Element;
      Drop   : Truncation := Error)
      return Sequence;

   procedure Head
     (Source : in out Sequence;
      Count  : Natural;
      Pad    : Element;
      Drop   : Truncation := Error);

   function Tail
     (Source : Sequence;
      Count  : Natural;
      Pad    : Element;
      Drop   : Truncation := Error)
      return Sequence;

   procedure Tail
     (Source : in out Sequence;
      Count  : Natural;
      Pad    : Element;
      Drop   : Truncation := Error);

   --------------------------------------
   -- Sequence constructor subprograms --
   --------------------------------------

   function "*"
     (Left  : Natural;
      Right : Element)
     return Sequence;

   function "*"
     (Left  : Natural;
      Right : Element_Array)
     return Sequence;

   function "*"
     (Left  : Natural;
      Right : Sequence)
     return Sequence;

   function Replicate
     (Count : Natural;
      Item  : Element;
      Drop  : Truncation := Error)
      return Sequence;

   function Replicate
     (Count : Natural;
      Item  : Element_Array;
      Drop  : Truncation := Error)
      return Sequence;

   function Replicate
     (Count : Natural;
      Item  : Sequence;
      Drop  : Truncation := Error)
      return Sequence;

private

   type Sequence is
      record
         Length  : Length_Range := 0;
         Content : Element_Array (1 .. Max_Length);
      end record;

   Default : Sequence;
   pragma Warnings (Off, Default);
   --  The default initial value is fine.

   Null_Sequence : constant Sequence := Default;

end PolyORB.Sequences.Bounded;
