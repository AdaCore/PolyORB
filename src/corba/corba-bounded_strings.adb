------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                C O R B A . B O U N D E D _ S T R I N G S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with PolyORB.Any;

package body CORBA.Bounded_Strings is

   use CBS;

   -----------------------
   -- TC_Bounded_String --
   -----------------------

   function TC_Bounded_String return CORBA.TypeCode.Object is
      PTC_Bounded_String : constant PolyORB.Any.TypeCode.Object :=
        PolyORB.Any.TypeCode.Build_Bounded_String_TC (Max_Length);
   begin
      return TypeCode.Internals.To_CORBA_Object (PTC_Bounded_String);
   end TC_Bounded_String;

   --  Since the bounded string type does not exist in the neutral core
   --  of PolyORB, we handle this type internally as an unbounded string
   --  So, the TypeCode is changed to unbounded string  at the beginning
   --  of the From_Any function and to bounded string at the end of the
   --  To_Any function in order to assure interoperability with other ORBs.

   --------------
   -- From_Any --
   --------------

   function From_Any (From : CORBA.Any) return Bounded_String is
      From_Cache : Any := From;
      CORBA_Str  : CORBA.String;
   begin
      Internals.Set_Type (From_Cache, TC_String);
      CORBA_Str := From_Any (From_Cache);
      return To_Bounded_String (To_Standard_String (CORBA_Str));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (To : Bounded_String) return CORBA.Any is
      CORBA_Str : constant CORBA.String := To_CORBA_String
        (To_String (To));
      Result    : Any;
   begin
      Result := To_Any (CORBA_Str);
      Internals.Set_Type (Result, TC_Bounded_String);
      return Result;
   end To_Any;

   ------------
   -- Length --
   ------------

   function Length (Source : in Bounded_String) return Length_Range
   is
      Result : constant CBS.Length_Range :=
        CBS.Length (CBS.Bounded_String (Source));
   begin
      return Length_Range (Result);
   end Length;

   -----------------------
   -- To_Bounded_String --
   -----------------------

   function To_Bounded_String
     (Source : in Standard.String;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   function To_String (Source : in Bounded_String) return Standard.String
   is
      Result : constant Standard.String :=
        CBS.To_String (CBS.Bounded_String (Source));
   begin
      return Result;
   end To_String;

   ------------
   -- Append --
   ------------

   function Append
     (Left, Right : in Bounded_String;
      Drop        : in Ada.Strings.Truncation  := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (CBS.Bounded_String (Left),
         CBS.Bounded_String (Right),
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   function Append
     (Left  : in Bounded_String;
      Right : in Standard.String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (CBS.Bounded_String (Left),
         Right,
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   function Append
     (Left  : in Standard.String;
      Right : in Bounded_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (Left,
         CBS.Bounded_String (Right),
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   function Append
     (Left  : in Bounded_String;
      Right : in Character;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (CBS.Bounded_String (Left),
         Right,
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   function Append
     (Left  : in Character;
      Right : in Bounded_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Append
        (Left,
         CBS.Bounded_String (Right),
         Drop);
   begin
      return Bounded_String (Result);
   end Append;

   procedure Append
     (Source   : in out Bounded_String;
      New_Item : in Bounded_String;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Append
        (CBS_Source,
         CBS.Bounded_String (New_Item),
         Drop);
      Source := Bounded_String (CBS_Source);
   end Append;

   procedure Append
     (Source   : in out Bounded_String;
      New_Item : in Standard.String;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Append
        (CBS_Source,
         New_Item,
         Drop);
      Source := Bounded_String (CBS_Source);
   end Append;

   procedure Append
     (Source   : in out Bounded_String;
      New_Item : in Character;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error)
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

   function "&"
     (Left, Right : in Bounded_String)
     return        Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        CBS.Bounded_String (Left) & CBS.Bounded_String (Right);
   begin
      return Bounded_String (Result);
   end "&";

   function "&"
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        CBS.Bounded_String (Left) & Right;
   begin
      return Bounded_String (Result);
   end "&";

   function "&"
     (Left  : in Standard.String;
      Right : in Bounded_String)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        Left & CBS.Bounded_String (Right);
   begin
      return Bounded_String (Result);
   end "&";

   function "&"
     (Left  : in Bounded_String;
      Right : in Character)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String :=
        CBS.Bounded_String (Left) & Right;
   begin
      return Bounded_String (Result);
   end "&";

   function "&"
     (Left  : in Character;
      Right : in Bounded_String)
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

   function Element
     (Source : in Bounded_String;
      Index  : in Positive)
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

   procedure Replace_Element
     (Source : in out Bounded_String;
      Index  : in Positive;
      By     : in Character)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Replace_Element (CBS_Source, Index, By);
      Source := Bounded_String (CBS_Source);
   end Replace_Element;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : in Bounded_String;
      Low    : in Positive;
      High   : in Natural)
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

   function "="  (Left, Right : in Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) = CBS.Bounded_String (Right);
   begin
      return Result;
   end "=";

   function "="
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) = Right;
   begin
      return Result;
   end "=";

   function "="
     (Left  : in Standard.String;
      Right : in Bounded_String)
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

   function "<"  (Left, Right : in Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) < CBS.Bounded_String (Right);
   begin
      return Result;
   end "<";

   function "<"
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) < Right;
   begin
      return Result;
   end "<";

   function "<"
     (Left  : in Standard.String;
      Right : in Bounded_String)
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

   function "<=" (Left, Right : in Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) <= CBS.Bounded_String (Right);
   begin
      return Result;
   end "<=";

   function "<="
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) <= Right;
   begin
      return Result;
   end "<=";

   function "<="
     (Left  : in Standard.String;
      Right : in Bounded_String)
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

   function ">"  (Left, Right : in Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) > CBS.Bounded_String (Right);
   begin
      return Result;
   end ">";

   function ">"
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) > Right;
   begin
      return Result;
   end ">";

   function ">"
     (Left  : in Standard.String;
      Right : in Bounded_String)
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

   function ">=" (Left, Right : in Bounded_String) return Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) >= CBS.Bounded_String (Right);
   begin
      return Result;
   end ">=";

   function ">="
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBS.Bounded_String (Left) >= Right;
   begin
      return Result;
   end ">=";

   function ">="
     (Left  : in Standard.String;
      Right : in Bounded_String)
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

   function Index
     (Source  : in Bounded_String;
      Pattern : in Standard.String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping
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

   function Index
     (Source  : in Bounded_String;
      Pattern : in Standard.String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
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

   function Index
     (Source : in Bounded_String;
      Set    : in Ada.Strings.Maps.Character_Set;
      Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : in Ada.Strings.Direction  := Ada.Strings.Forward)
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

   function Index_Non_Blank
     (Source : in Bounded_String;
      Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
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

   function Count
     (Source  : in Bounded_String;
      Pattern : in Standard.String;
      Mapping : in Ada.Strings.Maps.Character_Mapping
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

   function Count
     (Source  : in Bounded_String;
      Pattern : in Standard.String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
     return    Natural
   is
      Result : constant Natural := CBS.Count
        (CBS.Bounded_String (Source),
         Pattern,
         Mapping);
   begin
      return Result;
   end Count;

   function Count
     (Source : in Bounded_String;
      Set    : in Ada.Strings.Maps.Character_Set)
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

   procedure Find_Token
     (Source : in Bounded_String;
      Set    : in Ada.Strings.Maps.Character_Set;
      Test   : in Ada.Strings.Membership;
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

   function Translate
     (Source   : in Bounded_String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping)
     return     Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Translate
        (CBS.Bounded_String (Source),
         Mapping);
   begin
      return Bounded_String (Result);
   end Translate;

   procedure Translate
     (Source   : in out Bounded_String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Translate (CBS_Source, Mapping);
      Source := Bounded_String (CBS_Source);
   end Translate;

   function Translate
     (Source  : in Bounded_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
     return    Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Translate
        (CBS.Bounded_String (Source),
         Mapping);
   begin
      return Bounded_String (Result);
   end Translate;

   procedure Translate
     (Source  : in out Bounded_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Translate (CBS_Source, Mapping);
      Source := Bounded_String (CBS_Source);
   end Translate;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source   : in Bounded_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Standard.String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   procedure Replace_Slice
     (Source   : in out Bounded_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Standard.String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Replace_Slice (CBS_Source, Low, High, By, Drop);
      Source := Bounded_String (CBS_Source);
   end Replace_Slice;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : in Bounded_String;
      Before   : in Positive;
      New_Item : in Standard.String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   procedure Insert
     (Source   : in out Bounded_String;
      Before   : in Positive;
      New_Item : in Standard.String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Insert (CBS_Source, Before, New_Item, Drop);
      Source := Bounded_String (CBS_Source);
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source    : in Bounded_String;
      Position  : in Positive;
      New_Item  : in Standard.String;
      Drop      : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   procedure Overwrite
     (Source    : in out Bounded_String;
      Position  : in Positive;
      New_Item  : in Standard.String;
      Drop      : in Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Overwrite (CBS_Source, Position, New_Item, Drop);
      Source := Bounded_String (CBS_Source);
   end Overwrite;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : in Bounded_String;
      From    : in Positive;
      Through : in Natural)
     return    Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Delete
        (CBS.Bounded_String (Source),
         From,
         Through);
   begin
      return Bounded_String (Result);
   end Delete;

   procedure Delete
     (Source  : in out Bounded_String;
      From    : in Positive;
      Through : in Natural)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Delete (CBS_Source, From, Through);
      Source := Bounded_String (CBS_Source);
   end Delete;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : in Bounded_String;
      Side   : in Ada.Strings.Trim_End)
     return   Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Trim
        (CBS.Bounded_String (Source), Side);
   begin
      return Bounded_String (Result);
   end Trim;

   procedure Trim
     (Source : in out Bounded_String;
      Side   : in Ada.Strings.Trim_End)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Trim (CBS_Source, Side);
      Source := Bounded_String (CBS_Source);
   end Trim;

   function Trim
     (Source  : in Bounded_String;
      Left   : in Ada.Strings.Maps.Character_Set;
      Right  : in Ada.Strings.Maps.Character_Set)
     return   Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Trim
        (CBS.Bounded_String (Source), Left, Right);
   begin
      return Bounded_String (Result);
   end Trim;

   procedure Trim
     (Source : in out Bounded_String;
      Left   : in Ada.Strings.Maps.Character_Set;
      Right  : in Ada.Strings.Maps.Character_Set)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Trim (CBS_Source, Left, Right);
      Source := Bounded_String (CBS_Source);
   end Trim;

   ----------
   -- Head --
   ----------

   function Head
     (Source : in Bounded_String;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
     return   Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Head
        (CBS.Bounded_String (Source), Count, Pad, Drop);
   begin
      return Bounded_String (Result);
   end Head;

   procedure Head
     (Source : in out Bounded_String;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Head (CBS_Source, Count, Pad, Drop);
      Source := Bounded_String (CBS_Source);
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : in Bounded_String;
      Count  : in Natural;
      Pad    : in Character  := Ada.Strings.Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
     return Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Tail
        (CBS.Bounded_String (Source), Count, Pad, Drop);
   begin
      return Bounded_String (Result);
   end Tail;

   procedure Tail
     (Source : in out Bounded_String;
      Count  : in Natural;
      Pad    : in Character  := Ada.Strings.Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
   is
      CBS_Source : CBS.Bounded_String := CBS.Bounded_String (Source);
   begin
      CBS.Tail (CBS_Source, Count, Pad, Drop);
      Source := Bounded_String (CBS_Source);
   end Tail;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : in Natural;
      Right : in Character)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := Left * Right;
   begin
      return Bounded_String (Result);
   end "*";

   function "*"
     (Left  : in Natural;
      Right : in Standard.String)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := Left * Right;
   begin
      return Bounded_String (Result);
   end "*";

   function "*"
     (Left  : in Natural;
      Right : in Bounded_String)
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

   function Replicate
     (Count : in Natural;
      Item  : in Character;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Replicate
        (Count, Item, Drop);
   begin
      return Bounded_String (Result);
   end Replicate;

   function Replicate
     (Count : in Natural;
      Item  : in Standard.String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Replicate
        (Count, Item, Drop);
   begin
      return Bounded_String (Result);
   end Replicate;

   function Replicate
     (Count : in Natural;
      Item  : in Bounded_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String
   is
      Result : constant CBS.Bounded_String := CBS.Replicate
        (Count, CBS.Bounded_String (Item), Drop);
   begin
      return Bounded_String (Result);
   end Replicate;

end CORBA.Bounded_Strings;
