------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C O R B A . B O U N D E D _ W I D E _ S T R I N G S            --
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
with Ada.Strings.Wide_Superbounded;  --  Internal GNAT unit
pragma Warnings (On);
with Ada.Unchecked_Conversion;

package body CORBA.Bounded_Wide_Strings is

   use CBWS;

   function To_Bounded_Wide_String is new Ada.Unchecked_Conversion
     (Ada.Strings.Wide_Superbounded.Super_String, Bounded_Wide_String);

   function To_Super_String is new Ada.Unchecked_Conversion
     (Bounded_Wide_String, Ada.Strings.Wide_Superbounded.Super_String);

   ----------------------------
   -- TC_Bounded_Wide_String --
   ----------------------------

   TC_Cache : PolyORB.Any.TypeCode.Local_Ref;

   function TC_Bounded_Wide_String return PolyORB.Any.TypeCode.Local_Ref;
   --  Internal representation as a PolyORB TypeCode

   function TC_Bounded_Wide_String return PolyORB.Any.TypeCode.Local_Ref is
      use PolyORB.Any.TypeCode;
   begin
      if Is_Nil (TC_Cache) then
         TC_Cache := Build_Wstring_TC
                       (PolyORB.Types.Unsigned_Long (Max_Length));
         Disable_Reference_Counting (Object_Of (TC_Cache).all);
      end if;
      return TC_Cache;
   end TC_Bounded_Wide_String;

   ----------------------------
   -- TC_Bounded_Wide_String --
   ----------------------------

   function TC_Bounded_Wide_String return CORBA.TypeCode.Object is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object (TC_Bounded_Wide_String);
   end TC_Bounded_Wide_String;

   --------------
   -- From_Any --
   --------------

   function From_Any (From : CORBA.Any) return Bounded_Wide_String is
      Super : constant Ada.Strings.Wide_Superbounded.Super_String :=
        From_Any (From);
   begin
      if Super.Max_Length /= Max_Length then
         raise Constraint_Error;
      end if;
      return To_Bounded_Wide_String (Super);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (To : Bounded_Wide_String) return CORBA.Any is
   begin
      return To_Any (To_Super_String (To), TC_Bounded_Wide_String'Access);
   end To_Any;

   ------------
   -- Length --
   ------------

   overriding function Length
     (Source : Bounded_Wide_String)
     return Length_Range
   is
      Result : constant CBWS.Length_Range :=
        CBWS.Length (CBWS.Bounded_Wide_String (Source));
   begin
      return Length_Range (Result);
   end Length;

   ----------------------------
   -- To_Bounded_Wide_String --
   ----------------------------

   overriding function To_Bounded_Wide_String
     (Source : Standard.Wide_String;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
     return   Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        CBWS.To_Bounded_Wide_String (Source, Drop);
   begin
      return Bounded_Wide_String (Result);
   end To_Bounded_Wide_String;

   --------------------
   -- To_Wide_String --
   --------------------

   overriding function To_Wide_String
     (Source : Bounded_Wide_String)
     return Standard.Wide_String
   is
      Result : constant Standard.Wide_String :=
        CBWS.To_Wide_String (CBWS.Bounded_Wide_String (Source));
   begin
      return Result;
   end To_Wide_String;

   ------------
   -- Append --
   ------------

   overriding function Append
     (Left, Right : Bounded_Wide_String;
      Drop        : Ada.Strings.Truncation  := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (CBWS.Bounded_Wide_String (Left),
         CBWS.Bounded_Wide_String (Right),
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   overriding function Append
     (Left  : Bounded_Wide_String;
      Right : Standard.Wide_String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (CBWS.Bounded_Wide_String (Left),
         Right,
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   overriding function Append
     (Left  : Standard.Wide_String;
      Right : Bounded_Wide_String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (Left,
         CBWS.Bounded_Wide_String (Right),
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   overriding function Append
     (Left  : Bounded_Wide_String;
      Right : Wide_Character;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (CBWS.Bounded_Wide_String (Left),
         Right,
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   overriding function Append
     (Left  : Wide_Character;
      Right : Bounded_Wide_String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (Left,
         CBWS.Bounded_Wide_String (Right),
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   overriding procedure Append
     (Source   : in out Bounded_Wide_String;
      New_Item : Bounded_Wide_String;
      Drop     : Ada.Strings.Truncation  := Ada.Strings.Error)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Append
        (CBWS_Source,
         CBWS.Bounded_Wide_String (New_Item),
         Drop);
      Source := Bounded_Wide_String (CBWS_Source);
   end Append;

   overriding procedure Append
     (Source   : in out Bounded_Wide_String;
      New_Item : Standard.Wide_String;
      Drop     : Ada.Strings.Truncation  := Ada.Strings.Error)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Append
        (CBWS_Source,
         New_Item,
         Drop);
      Source := Bounded_Wide_String (CBWS_Source);
   end Append;

   overriding procedure Append
     (Source   : in out Bounded_Wide_String;
      New_Item : Wide_Character;
      Drop     : Ada.Strings.Truncation  := Ada.Strings.Error)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Append
        (CBWS_Source,
         New_Item,
         Drop);
      Source := Bounded_Wide_String (CBWS_Source);
   end Append;

   ---------
   -- "&" --
   ---------

   overriding function "&"
     (Left, Right : Bounded_Wide_String)
     return        Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        CBWS.Bounded_Wide_String (Left) & CBWS.Bounded_Wide_String (Right);
   begin
      return Bounded_Wide_String (Result);
   end "&";

   overriding function "&"
     (Left  : Bounded_Wide_String;
      Right : Standard.Wide_String)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        CBWS.Bounded_Wide_String (Left) & Right;
   begin
      return Bounded_Wide_String (Result);
   end "&";

   overriding function "&"
     (Left  : Standard.Wide_String;
      Right : Bounded_Wide_String)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        Left & CBWS.Bounded_Wide_String (Right);
   begin
      return Bounded_Wide_String (Result);
   end "&";

   overriding function "&"
     (Left  : Bounded_Wide_String;
      Right : Wide_Character)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        CBWS.Bounded_Wide_String (Left) & Right;
   begin
      return Bounded_Wide_String (Result);
   end "&";

   overriding function "&"
     (Left  : Wide_Character;
      Right : Bounded_Wide_String)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        Left & CBWS.Bounded_Wide_String (Right);
   begin
      return Bounded_Wide_String (Result);
   end "&";

   -------------
   -- Element --
   -------------

   overriding function Element
     (Source : Bounded_Wide_String;
      Index  : Positive)
     return   Wide_Character
   is
      Result : constant Wide_Character := CBWS.Element
        (CBWS.Bounded_Wide_String (Source), Index);
   begin
      return Result;
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   overriding procedure Replace_Element
     (Source : in out Bounded_Wide_String;
      Index  : Positive;
      By     : Wide_Character)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Replace_Element (CBWS_Source, Index, By);
      Source := Bounded_Wide_String (CBWS_Source);
   end Replace_Element;

   -----------
   -- Slice --
   -----------

   overriding function Slice
     (Source : Bounded_Wide_String;
      Low    : Positive;
      High   : Natural)
     return   Standard.Wide_String
   is
      Result : constant Standard.Wide_String := CBWS.Slice
        (CBWS.Bounded_Wide_String (Source), Low, High);
   begin
      return Result;
   end Slice;

   ---------
   -- "=" --
   ---------

   overriding function "="  (Left, Right : Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) = CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "=";

   overriding function "="
     (Left  : Bounded_Wide_String;
      Right : Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) = Right;
   begin
      return Result;
   end "=";

   overriding function "="
     (Left  : Standard.Wide_String;
      Right : Bounded_Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left = CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "=";

   ---------
   -- "<" --
   ---------

   overriding function "<"  (Left, Right : Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) < CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "<";

   overriding function "<"
     (Left  : Bounded_Wide_String;
      Right : Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) < Right;
   begin
      return Result;
   end "<";

   overriding function "<"
     (Left  : Standard.Wide_String;
      Right : Bounded_Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left < CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "<";

   ----------
   -- "<=" --
   ----------

   overriding function "<=" (Left, Right : Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) <= CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "<=";

   overriding function "<="
     (Left  : Bounded_Wide_String;
      Right : Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) <= Right;
   begin
      return Result;
   end "<=";

   overriding function "<="
     (Left  : Standard.Wide_String;
      Right : Bounded_Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left <= CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "<=";

   ---------
   -- ">" --
   ---------

   overriding function ">"  (Left, Right : Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) > CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end ">";

   overriding function ">"
     (Left  : Bounded_Wide_String;
      Right : Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) > Right;
   begin
      return Result;
   end ">";

   overriding function ">"
     (Left  : Standard.Wide_String;
      Right : Bounded_Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left > CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end ">";

   ----------
   -- ">=" --
   ----------

   overriding function ">=" (Left, Right : Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) >= CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end ">=";

   overriding function ">="
     (Left  : Bounded_Wide_String;
      Right : Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) >= Right;
   begin
      return Result;
   end ">=";

   overriding function ">="
     (Left  : Standard.Wide_String;
      Right : Bounded_Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        Left >= CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end ">=";

   -----------
   -- Index --
   -----------

   overriding function Index
     (Source  : Bounded_Wide_String;
      Pattern : Standard.Wide_String;
      Going   : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Wide_Maps.Wide_Character_Mapping
        := Ada.Strings.Wide_Maps.Identity)
     return    Natural
   is
      Result : constant Natural := CBWS.Index
        (CBWS.Bounded_Wide_String (Source),
         Pattern,
         Going,
         Mapping);
   begin
      return Result;
   end Index;

   overriding function Index
     (Source  : Bounded_Wide_String;
      Pattern : Standard.Wide_String;
      Going   : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Wide_Maps.Wide_Character_Mapping_Function)
     return    Natural
   is
      Result : constant Natural := CBWS.Index
        (CBWS.Bounded_Wide_String (Source),
         Pattern,
         Going,
         Mapping);
   begin
      return Result;
   end Index;

   overriding function Index
     (Source : Bounded_Wide_String;
      Set    : Ada.Strings.Wide_Maps.Wide_Character_Set;
      Test   : Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : Ada.Strings.Direction  := Ada.Strings.Forward)
     return   Natural
   is
      Result : constant Natural := CBWS.Index
        (CBWS.Bounded_Wide_String (Source),
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
     (Source : Bounded_Wide_String;
      Going  : Ada.Strings.Direction := Ada.Strings.Forward)
     return   Natural
   is
      Result : constant Natural := CBWS.Index_Non_Blank
        (CBWS.Bounded_Wide_String (Source),
         Going);
   begin
      return Result;
   end Index_Non_Blank;

   -----------
   -- Count --
   -----------

   overriding function Count
     (Source  : Bounded_Wide_String;
      Pattern : Standard.Wide_String;
      Mapping : Ada.Strings.Wide_Maps.Wide_Character_Mapping
        := Ada.Strings.Wide_Maps.Identity)
     return    Natural
   is
      Result : constant Natural := CBWS.Count
        (CBWS.Bounded_Wide_String (Source),
         Pattern,
         Mapping);
   begin
      return Result;
   end Count;

   overriding function Count
     (Source  : Bounded_Wide_String;
      Pattern : Standard.Wide_String;
      Mapping : Ada.Strings.Wide_Maps.Wide_Character_Mapping_Function)
     return    Natural
   is
      Result : constant Natural := CBWS.Count
        (CBWS.Bounded_Wide_String (Source),
         Pattern,
         Mapping);
   begin
      return Result;
   end Count;

   overriding function Count
     (Source : Bounded_Wide_String;
      Set    : Ada.Strings.Wide_Maps.Wide_Character_Set)
     return   Natural
   is
      Result : constant Natural := CBWS.Count
        (CBWS.Bounded_Wide_String (Source),
         Set);
   begin
      return Result;
   end Count;

   ----------------
   -- Find_Token --
   ----------------

   overriding procedure Find_Token
     (Source : Bounded_Wide_String;
      Set    : Ada.Strings.Wide_Maps.Wide_Character_Set;
      Test   : Ada.Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      CBWS.Find_Token
        (CBWS.Bounded_Wide_String (Source),
         Set,
         Test,
         First,
         Last);
   end Find_Token;

   ---------------
   -- Translate --
   ---------------

   overriding function Translate
     (Source   : Bounded_Wide_String;
      Mapping  : Ada.Strings.Wide_Maps.Wide_Character_Mapping)
     return     Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Translate
        (CBWS.Bounded_Wide_String (Source),
         Mapping);
   begin
      return Bounded_Wide_String (Result);
   end Translate;

   overriding procedure Translate
     (Source   : in out Bounded_Wide_String;
      Mapping  : Ada.Strings.Wide_Maps.Wide_Character_Mapping)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Translate (CBWS_Source, Mapping);
      Source := Bounded_Wide_String (CBWS_Source);
   end Translate;

   overriding function Translate
     (Source  : Bounded_Wide_String;
      Mapping : Ada.Strings.Wide_Maps.Wide_Character_Mapping_Function)
     return    Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Translate
        (CBWS.Bounded_Wide_String (Source),
         Mapping);
   begin
      return Bounded_Wide_String (Result);
   end Translate;

   overriding procedure Translate
     (Source  : in out Bounded_Wide_String;
      Mapping : Ada.Strings.Wide_Maps.Wide_Character_Mapping_Function)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Translate (CBWS_Source, Mapping);
      Source := Bounded_Wide_String (CBWS_Source);
   end Translate;

   -------------------
   -- Replace_Slice --
   -------------------

   overriding function Replace_Slice
     (Source   : Bounded_Wide_String;
      Low      : Positive;
      High     : Natural;
      By       : Standard.Wide_String;
      Drop     : Ada.Strings.Truncation := Ada.Strings.Error)
     return     Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Replace_Slice
        (CBWS.Bounded_Wide_String (Source),
         Low,
         High,
         By,
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Replace_Slice;

   overriding procedure Replace_Slice
     (Source   : in out Bounded_Wide_String;
      Low      : Positive;
      High     : Natural;
      By       : Standard.Wide_String;
      Drop     : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Replace_Slice (CBWS_Source, Low, High, By, Drop);
      Source := Bounded_Wide_String (CBWS_Source);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   overriding function Insert
     (Source   : Bounded_Wide_String;
      Before   : Positive;
      New_Item : Standard.Wide_String;
      Drop     : Ada.Strings.Truncation := Ada.Strings.Error)
     return     Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Insert
        (CBWS.Bounded_Wide_String (Source),
         Before,
         New_Item,
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Insert;

   overriding procedure Insert
     (Source   : in out Bounded_Wide_String;
      Before   : Positive;
      New_Item : Standard.Wide_String;
      Drop     : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Insert (CBWS_Source, Before, New_Item, Drop);
      Source := Bounded_Wide_String (CBWS_Source);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   overriding function Overwrite
     (Source    : Bounded_Wide_String;
      Position  : Positive;
      New_Item  : Standard.Wide_String;
      Drop      : Ada.Strings.Truncation := Ada.Strings.Error)
     return      Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Overwrite
        (CBWS.Bounded_Wide_String (Source),
         Position,
         New_Item,
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Overwrite;

   overriding procedure Overwrite
     (Source    : in out Bounded_Wide_String;
      Position  : Positive;
      New_Item  : Standard.Wide_String;
      Drop      : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Overwrite (CBWS_Source, Position, New_Item, Drop);
      Source := Bounded_Wide_String (CBWS_Source);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   overriding function Delete
     (Source  : Bounded_Wide_String;
      From    : Positive;
      Through : Natural)
     return    Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Delete
        (CBWS.Bounded_Wide_String (Source),
         From,
         Through);
   begin
      return Bounded_Wide_String (Result);
   end Delete;

   overriding procedure Delete
     (Source  : in out Bounded_Wide_String;
      From    : Positive;
      Through : Natural)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Delete (CBWS_Source, From, Through);
      Source := Bounded_Wide_String (CBWS_Source);
   end Delete;

   ----------
   -- Trim --
   ----------

   overriding function Trim
     (Source : Bounded_Wide_String;
      Side   : Ada.Strings.Trim_End)
     return   Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Trim
        (CBWS.Bounded_Wide_String (Source), Side);
   begin
      return Bounded_Wide_String (Result);
   end Trim;

   overriding procedure Trim
     (Source : in out Bounded_Wide_String;
      Side   : Ada.Strings.Trim_End)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Trim (CBWS_Source, Side);
      Source := Bounded_Wide_String (CBWS_Source);
   end Trim;

   overriding function Trim
     (Source  : Bounded_Wide_String;
      Left   : Ada.Strings.Wide_Maps.Wide_Character_Set;
      Right  : Ada.Strings.Wide_Maps.Wide_Character_Set)
     return   Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Trim
        (CBWS.Bounded_Wide_String (Source), Left, Right);
   begin
      return Bounded_Wide_String (Result);
   end Trim;

   overriding procedure Trim
     (Source : in out Bounded_Wide_String;
      Left   : Ada.Strings.Wide_Maps.Wide_Character_Set;
      Right  : Ada.Strings.Wide_Maps.Wide_Character_Set)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Trim (CBWS_Source, Left, Right);
      Source := Bounded_Wide_String (CBWS_Source);
   end Trim;

   ----------
   -- Head --
   ----------

   overriding function Head
     (Source : Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Ada.Strings.Wide_Space;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
     return   Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Head
        (CBWS.Bounded_Wide_String (Source), Count, Pad, Drop);
   begin
      return Bounded_Wide_String (Result);
   end Head;

   overriding procedure Head
     (Source : in out Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Ada.Strings.Wide_Space;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Head (CBWS_Source, Count, Pad, Drop);
      Source := Bounded_Wide_String (CBWS_Source);
   end Head;

   ----------
   -- Tail --
   ----------

   overriding function Tail
     (Source : Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character  := Ada.Strings.Wide_Space;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
     return Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Tail
        (CBWS.Bounded_Wide_String (Source), Count, Pad, Drop);
   begin
      return Bounded_Wide_String (Result);
   end Tail;

   overriding procedure Tail
     (Source : in out Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character  := Ada.Strings.Wide_Space;
      Drop   : Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBWS_Source : CBWS.Bounded_Wide_String :=
        CBWS.Bounded_Wide_String (Source);
   begin
      CBWS.Tail (CBWS_Source, Count, Pad, Drop);
      Source := Bounded_Wide_String (CBWS_Source);
   end Tail;

   ---------
   -- "*" --
   ---------

   overriding function "*"
     (Left  : Natural;
      Right : Wide_Character)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := Left * Right;
   begin
      return Bounded_Wide_String (Result);
   end "*";

   overriding function "*"
     (Left  : Natural;
      Right : Standard.Wide_String)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := Left * Right;
   begin
      return Bounded_Wide_String (Result);
   end "*";

   overriding function "*"
     (Left  : Natural;
      Right : Bounded_Wide_String)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        Left * CBWS.Bounded_Wide_String (Right);
   begin
      return Bounded_Wide_String (Result);
   end "*";

   ---------------
   -- Replicate --
   ---------------

   overriding function Replicate
     (Count : Natural;
      Item  : Wide_Character;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Replicate
        (Count, Item, Drop);
   begin
      return Bounded_Wide_String (Result);
   end Replicate;

   overriding function Replicate
     (Count : Natural;
      Item  : Standard.Wide_String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Replicate
        (Count, Item, Drop);
   begin
      return Bounded_Wide_String (Result);
   end Replicate;

   overriding function Replicate
     (Count : Natural;
      Item  : Bounded_Wide_String;
      Drop  : Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Replicate
        (Count, CBWS.Bounded_Wide_String (Item), Drop);
   begin
      return Bounded_Wide_String (Result);
   end Replicate;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (X : access Bounded_Wide_String) return PolyORB.Any.Content'Class
   is
   begin
      return PolyORB.Any.Wrap (To_Super_String (X.all)'Unrestricted_Access);
   end Wrap;

end CORBA.Bounded_Wide_Strings;
