------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                C O R B A . B O U N D E D _ S T R I N G S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

pragma Warnings (Off);
with Ada.Strings.Superbounded;  --  Internal GNAT unit
pragma Warnings (On);
with Ada.Unchecked_Conversion;

package body CORBA.Bounded_Strings is

   use CBS;

   function To_Bounded_String is new Ada.Unchecked_Conversion
     (Ada.Strings.Superbounded.Super_String, Bounded_String);

   function To_Super_String is new Ada.Unchecked_Conversion
     (Bounded_String, Ada.Strings.Superbounded.Super_String);

   -----------------------
   -- TC_Bounded_String --
   -----------------------

   TC_Cache : PolyORB.Any.TypeCode.Local_Ref;

   function TC_Bounded_String return PolyORB.Any.TypeCode.Local_Ref;
   --  Internal representation as a PolyORB TypeCode

   function TC_Bounded_String return PolyORB.Any.TypeCode.Local_Ref is
      use PolyORB.Any.TypeCode;
   begin
      if Is_Nil (TC_Cache) then
         TC_Cache := Build_String_TC
                       (PolyORB.Types.Unsigned_Long (Max_Length));
         Disable_Reference_Counting (Object_Of (TC_Cache).all);
      end if;
      return TC_Cache;
   end TC_Bounded_String;

   -----------------------
   -- TC_Bounded_String --
   -----------------------

   function TC_Bounded_String return CORBA.TypeCode.Object is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object (TC_Bounded_String);
   end TC_Bounded_String;

   --------------
   -- From_Any --
   --------------

   function From_Any (From : CORBA.Any) return Bounded_String is
      Super : constant Ada.Strings.Superbounded.Super_String :=
                From_Any (From);
   begin
      if Super.Max_Length /= Max_Length then
         raise Constraint_Error;
      end if;
      return To_Bounded_String (Super);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (To : Bounded_String) return CORBA.Any is
   begin
      return To_Any (To_Super_String (To), TC_Bounded_String'Access);
   end To_Any;

   ------------
   -- Length --
   ------------

   overriding function Length (Source : Bounded_String) return Length_Range
   is
      Result : constant CBS.Length_Range :=
        CBS.Length (CBS.Bounded_String (Source));
   begin
      return Length_Range (Result);
   end Length;

   -----------------------
   -- To_Bounded_String --
   -----------------------

   overriding function To_Bounded_String
     (Source : Standard.String;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
     return   Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        CBS.To_Bounded_String (Source, Drop);
   begin
      return Bounded_String (Result);
   end To_Bounded_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Source : Bounded_String)
     return Standard.String
   is
      Result : constant Standard.String :=
        CBS.To_String (CBS.Bounded_String (Source));
   begin
      return Result;
   end To_String;

   ------------
   -- Append --
   ------------

   overriding function Append
     (Left, Right : Bounded_String;
      Drop        : Ada.Strings.Truncation  := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (CBS.Bounded_String (Left),
         CBS.Bounded_String (Right),
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   overriding function Append
     (Left  : Bounded_String;
      Right : Standard.String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (CBS.Bounded_String (Left),
         Right,
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   overriding function Append
     (Left  : Standard.String;
      Right : Bounded_String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (Left,
         CBS.Bounded_String (Right),
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   overriding function Append
     (Left  : Bounded_String;
      Right : Character;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (CBS.Bounded_String (Left),
         Right,
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   overriding function Append
     (Left  : Character;
      Right : Bounded_String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (Left,
         CBS.Bounded_String (Right),
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   overriding procedure Append
     (Source   : in out Bounded_String;
      New_Item : Bounded_String;
      Drop     : Ada.Strings.Truncation  := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Append
        (CBS_Source,
         CBS.Bounded_String (New_Item),
         Drop);
      Source := Bounded_String (CBS_Source);
   end Append;

   overriding procedure Append
     (Source   : in out Bounded_String;
      New_Item : Standard.String;
      Drop     : Ada.Strings.Truncation  := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Append
        (CBS_Source,
         New_Item,
         Drop);
      Source := Bounded_String (CBS_Source);
   end Append;

   overriding procedure Append
     (Source   : in out Bounded_String;
      New_Item : Character;
      Drop     : Ada.Strings.Truncation  := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Append
        (CBS_Source,
         New_Item,
         Drop);
      Source := Bounded_String (CBS_Source);
   end Append;

   ---------
   -- "&" --
   ---------

   overriding function "&"
     (Left, Right : Bounded_String)
     return        Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        CBS.Bounded_String (Left) & CBS.Bounded_String (Right);
   begin
      return Bounded_String (Result);
   end "&";

   overriding function "&"
     (Left  : Bounded_String;
      Right : Standard.String)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        CBS.Bounded_String (Left) & Right;
   begin
      return Bounded_String (Result);
   end "&";

   overriding function "&"
     (Left  : Standard.String;
      Right : Bounded_String)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        Left & CBS.Bounded_String (Right);
   begin
      return Bounded_String (Result);
   end "&";

   overriding function "&"
     (Left  : Bounded_String;
      Right : Character)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        CBS.Bounded_String (Left) & Right;
   begin
      return Bounded_String (Result);
   end "&";

   overriding function "&"
     (Left  : Character;
      Right : Bounded_String)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        Left & CBS.Bounded_String (Right);
   begin
      return Bounded_String (Result);
   end "&";

   -------------
   -- Element --
   -------------

   overriding function Element
     (Source : Bounded_String;
      Index  : Positive)
     return   Character
   is
      Result : constant Character := CBS.Element
        (CBS.Bounded_String (Source), Index);
   begin
      return Result;
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   overriding procedure Replace_Element
     (Source : in out Bounded_String;
      Index  : Positive;
      By     : Character)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Replace_Element (CBS_Source, Index, By);
      Source := Bounded_String (CBS_Source);
   end Replace_Element;

   -----------
   -- Slice --
   -----------

   overriding function Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural)
     return   Standard.String
   is
      Result : constant Standard.String := CBS.Slice
        (CBS.Bounded_String (Source), Low, High);
   begin
      return Result;
   end Slice;

   ---------
   -- "=" --
   ---------

   overriding function "="  (Left, Right : Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) = CBS.Bounded_String (Right);
   begin
      return Result;
   end "=";

   overriding function "="
     (Left  : Bounded_String;
      Right : Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) = Right;
   begin
      return Result;
   end "=";

   overriding function "="
     (Left  : Standard.String;
      Right : Bounded_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left = CBS.Bounded_String (Right);
   begin
      return Result;
   end "=";

   ---------
   -- "<" --
   ---------

   overriding function "<"  (Left, Right : Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) < CBS.Bounded_String (Right);
   begin
      return Result;
   end "<";

   overriding function "<"
     (Left  : Bounded_String;
      Right : Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) < Right;
   begin
      return Result;
   end "<";

   overriding function "<"
     (Left  : Standard.String;
      Right : Bounded_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left < CBS.Bounded_String (Right);
   begin
      return Result;
   end "<";

   ----------
   -- "<=" --
   ----------

   overriding function "<=" (Left, Right : Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) <= CBS.Bounded_String (Right);
   begin
      return Result;
   end "<=";

   overriding function "<="
     (Left  : Bounded_String;
      Right : Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) <= Right;
   begin
      return Result;
   end "<=";

   overriding function "<="
     (Left  : Standard.String;
      Right : Bounded_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left <= CBS.Bounded_String (Right);
   begin
      return Result;
   end "<=";

   ---------
   -- ">" --
   ---------

   overriding function ">"  (Left, Right : Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) > CBS.Bounded_String (Right);
   begin
      return Result;
   end ">";

   overriding function ">"
     (Left  : Bounded_String;
      Right : Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) > Right;
   begin
      return Result;
   end ">";

   overriding function ">"
     (Left  : Standard.String;
      Right : Bounded_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left > CBS.Bounded_String (Right);
   begin
      return Result;
   end ">";

   ----------
   -- ">=" --
   ----------

   overriding function ">=" (Left, Right : Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) >= CBS.Bounded_String (Right);
   begin
      return Result;
   end ">=";

   overriding function ">="
     (Left  : Bounded_String;
      Right : Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) >= Right;
   begin
      return Result;
   end ">=";

   overriding function ">="
     (Left  : Standard.String;
      Right : Bounded_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left >= CBS.Bounded_String (Right);
   begin
      return Result;
   end ">=";

   -----------
   -- Index --
   -----------

   overriding function Index
     (Source  : Bounded_String;
      Pattern : Standard.String;
      Going   : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity)
     return    Natural
   is
      Result : constant Natural := CBS.Index
        (CBS.Bounded_String (Source),
         Pattern,
         Going,
         Mapping);
   begin
      return Result;
   end Index;

   overriding function Index
     (Source  : Bounded_String;
      Pattern : Standard.String;
      Going   : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
     return    Natural
   is
      Result : constant Natural := CBS.Index
        (CBS.Bounded_String (Source),
         Pattern,
         Going,
         Mapping);
   begin
      return Result;
   end Index;

   overriding function Index
     (Source : Bounded_String;
      Set    : Ada.Strings.Maps.Character_Set;
      Test   : Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : Ada.Strings.Direction  := Ada.Strings.Forward)
     return   Natural
   is
      Result : constant Natural := CBS.Index
        (CBS.Bounded_String (Source),
         Set,
         Test,
         Going);
   begin
      return Result;
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   overriding function Index_Non_Blank
     (Source : Bounded_String;
      Going  : Ada.Strings.Direction := Ada.Strings.Forward)
     return   Natural
   is
      Result : constant Natural := CBS.Index_Non_Blank
        (CBS.Bounded_String (Source),
         Going);
   begin
      return Result;
   end Index_Non_Blank;

   -----------
   -- Count --
   -----------

   overriding function Count
     (Source  : Bounded_String;
      Pattern : Standard.String;
      Mapping : Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity)
     return    Natural
   is
      Result : constant Natural := CBS.Count
        (CBS.Bounded_String (Source),
         Pattern,
         Mapping);
   begin
      return Result;
   end Count;

   overriding function Count
     (Source  : Bounded_String;
      Pattern : Standard.String;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
     return    Natural
   is
      Result : constant Natural := CBS.Count
        (CBS.Bounded_String (Source),
         Pattern,
         Mapping);
   begin
      return Result;
   end Count;

   overriding function Count
     (Source : Bounded_String;
      Set    : Ada.Strings.Maps.Character_Set)
     return   Natural
   is
      Result : constant Natural := CBS.Count
        (CBS.Bounded_String (Source),
         Set);
   begin
      return Result;
   end Count;

   ----------------
   -- Find_Token --
   ----------------

   overriding procedure Find_Token
     (Source : Bounded_String;
      Set    : Ada.Strings.Maps.Character_Set;
      Test   : Ada.Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      CBS.Find_Token
        (CBS.Bounded_String (Source),
         Set,
         Test,
         First,
         Last);
   end Find_Token;

   ---------------
   -- Translate --
   ---------------

   overriding function Translate
     (Source   : Bounded_String;
      Mapping  : Ada.Strings.Maps.Character_Mapping)
     return     Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Translate
        (CBS.Bounded_String (Source),
         Mapping);
   begin
      return Bounded_String (Result);
   end Translate;

   overriding procedure Translate
     (Source   : in out Bounded_String;
      Mapping  : Ada.Strings.Maps.Character_Mapping)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Translate (CBS_Source, Mapping);
      Source := Bounded_String (CBS_Source);
   end Translate;

   overriding function Translate
     (Source  : Bounded_String;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
     return    Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Translate
        (CBS.Bounded_String (Source),
         Mapping);
   begin
      return Bounded_String (Result);
   end Translate;

   overriding procedure Translate
     (Source  : in out Bounded_String;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Translate (CBS_Source, Mapping);
      Source := Bounded_String (CBS_Source);
   end Translate;

   -------------------
   -- Replace_Slice --
   -------------------

   overriding function Replace_Slice
     (Source   : Bounded_String;
      Low      : Positive;
      High     : Natural;
      By       : Standard.String;
      Drop     : Ada.Strings.Truncation := Ada.Strings.Error)
     return     Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Replace_Slice
        (CBS.Bounded_String (Source),
         Low,
         High,
         By,
         Drop);
   begin
      return Bounded_String (Result);
   end Replace_Slice;

   overriding procedure Replace_Slice
     (Source   : in out Bounded_String;
      Low      : Positive;
      High     : Natural;
      By       : Standard.String;
      Drop     : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Replace_Slice (CBS_Source, Low, High, By, Drop);
      Source := Bounded_String (CBS_Source);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   overriding function Insert
     (Source   : Bounded_String;
      Before   : Positive;
      New_Item : Standard.String;
      Drop     : Ada.Strings.Truncation := Ada.Strings.Error)
     return     Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Insert
        (CBS.Bounded_String (Source),
         Before,
         New_Item,
         Drop);
   begin
      return Bounded_String (Result);
   end Insert;

   overriding procedure Insert
     (Source   : in out Bounded_String;
      Before   : Positive;
      New_Item : Standard.String;
      Drop     : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Insert (CBS_Source, Before, New_Item, Drop);
      Source := Bounded_String (CBS_Source);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   overriding function Overwrite
     (Source    : Bounded_String;
      Position  : Positive;
      New_Item  : Standard.String;
      Drop      : Ada.Strings.Truncation := Ada.Strings.Error)
     return      Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Overwrite
        (CBS.Bounded_String (Source),
         Position,
         New_Item,
         Drop);
   begin
      return Bounded_String (Result);
   end Overwrite;

   overriding procedure Overwrite
     (Source    : in out Bounded_String;
      Position  : Positive;
      New_Item  : Standard.String;
      Drop      : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Overwrite (CBS_Source, Position, New_Item, Drop);
      Source := Bounded_String (CBS_Source);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   overriding function Delete
     (Source  : Bounded_String;
      From    : Positive;
      Through : Natural)
     return    Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Delete
        (CBS.Bounded_String (Source),
         From,
         Through);
   begin
      return Bounded_String (Result);
   end Delete;

   overriding procedure Delete
     (Source  : in out Bounded_String;
      From    : Positive;
      Through : Natural)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Delete (CBS_Source, From, Through);
      Source := Bounded_String (CBS_Source);
   end Delete;

   ----------
   -- Trim --
   ----------

   overriding function Trim
     (Source : Bounded_String;
      Side   : Ada.Strings.Trim_End)
     return   Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Trim
        (CBS.Bounded_String (Source), Side);
   begin
      return Bounded_String (Result);
   end Trim;

   overriding procedure Trim
     (Source : in out Bounded_String;
      Side   : Ada.Strings.Trim_End)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Trim (CBS_Source, Side);
      Source := Bounded_String (CBS_Source);
   end Trim;

   overriding function Trim
     (Source  : Bounded_String;
      Left   : Ada.Strings.Maps.Character_Set;
      Right  : Ada.Strings.Maps.Character_Set)
     return   Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Trim
        (CBS.Bounded_String (Source), Left, Right);
   begin
      return Bounded_String (Result);
   end Trim;

   overriding procedure Trim
     (Source : in out Bounded_String;
      Left   : Ada.Strings.Maps.Character_Set;
      Right  : Ada.Strings.Maps.Character_Set)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Trim (CBS_Source, Left, Right);
      Source := Bounded_String (CBS_Source);
   end Trim;

   ----------
   -- Head --
   ----------

   overriding function Head
     (Source : Bounded_String;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
     return   Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Head
        (CBS.Bounded_String (Source), Count, Pad, Drop);
   begin
      return Bounded_String (Result);
   end Head;

   overriding procedure Head
     (Source : in out Bounded_String;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Head (CBS_Source, Count, Pad, Drop);
      Source := Bounded_String (CBS_Source);
   end Head;

   ----------
   -- Tail --
   ----------

   overriding function Tail
     (Source : Bounded_String;
      Count  : Natural;
      Pad    : Character  := Ada.Strings.Space;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
     return Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Tail
        (CBS.Bounded_String (Source), Count, Pad, Drop);
   begin
      return Bounded_String (Result);
   end Tail;

   overriding procedure Tail
     (Source : in out Bounded_String;
      Count  : Natural;
      Pad    : Character  := Ada.Strings.Space;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Tail (CBS_Source, Count, Pad, Drop);
      Source := Bounded_String (CBS_Source);
   end Tail;

   ---------
   -- "*" --
   ---------

   overriding function "*"
     (Left  : Natural;
      Right : Character)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := Left * Right;
   begin
      return Bounded_String (Result);
   end "*";

   overriding function "*"
     (Left  : Natural;
      Right : Standard.String)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := Left * Right;
   begin
      return Bounded_String (Result);
   end "*";

   overriding function "*"
     (Left  : Natural;
      Right : Bounded_String)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        Left * CBS.Bounded_String (Right);
   begin
      return Bounded_String (Result);
   end "*";

   ---------------
   -- Replicate --
   ---------------

   overriding function Replicate
     (Count : Natural;
      Item  : Character;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Replicate
        (Count, Item, Drop);
   begin
      return Bounded_String (Result);
   end Replicate;

   overriding function Replicate
     (Count : Natural;
      Item  : Standard.String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Replicate
        (Count, Item, Drop);
   begin
      return Bounded_String (Result);
   end Replicate;

   overriding function Replicate
     (Count : Natural;
      Item  : Bounded_String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Replicate
        (Count, CBS.Bounded_String (Item), Drop);
   begin
      return Bounded_String (Result);
   end Replicate;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access Bounded_String) return PolyORB.Any.Content'Class
   is
   begin
      return PolyORB.Any.Wrap
        (To_Super_String (X.all)'Unrestricted_Access);
   end Wrap;

end CORBA.Bounded_Strings;
