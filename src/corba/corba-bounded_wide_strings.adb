------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C O R B A . B O U N D E D _ W I D E _ S T R I N G S            --
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

package body CORBA.Bounded_Wide_Strings is

   use CBWS;

   ----------------------------
   -- TC_Bounded_Wide_String --
   ----------------------------

   function TC_Bounded_Wide_String return CORBA.TypeCode.Object is
      PTC_Bounded_Wide_String : constant PolyORB.Any.TypeCode.Object :=
        PolyORB.Any.TypeCode.Build_Bounded_Wide_String_TC (Max_Length);
   begin
      return TypeCode.Internals.To_CORBA_Object (PTC_Bounded_Wide_String);
   end TC_Bounded_Wide_String;

   --  Since the bounded wide string type does not exist in the neutral core
   --  of PolyORB, we handle this type internally as an unbounded wide string
   --  So, the TypeCode is changed to unbounded wide string at the beginning
   --  of the From_Any function and to bounded wide string at the end of the
   --  To_Any function in order to assure interoperability with other ORBs.

   --------------
   -- From_Any --
   --------------

   function From_Any (From : CORBA.Any) return Bounded_Wide_String is
      From_Cache : Any := From;
      CORBA_WStr : CORBA.Wide_String;
   begin
      Internals.Set_Type (From_Cache, TC_Wide_String);
      CORBA_WStr := From_Any (From_Cache);
      return To_Bounded_Wide_String (To_Standard_Wide_String (CORBA_WStr));
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (To : Bounded_Wide_String) return CORBA.Any is
      CORBA_WStr : constant CORBA.Wide_String := To_CORBA_Wide_String
        (To_Wide_String (To));
      Result     : Any;
   begin
      Result := To_Any (CORBA_WStr);
      Internals.Set_Type (Result, TC_Bounded_Wide_String);
      return Result;
   end To_Any;

   ------------
   -- Length --
   ------------

   function Length (Source : in Bounded_Wide_String) return Length_Range
   is
      Result : constant CBWS.Length_Range :=
        CBWS.Length (CBWS.Bounded_Wide_String (Source));
   begin
      return Length_Range (Result);
   end Length;

   ----------------------------
   -- To_Bounded_Wide_String --
   ----------------------------

   function To_Bounded_Wide_String
     (Source : in Standard.Wide_String;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   function To_Wide_String
     (Source : in Bounded_Wide_String)
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

   function Append
     (Left, Right : in Bounded_Wide_String;
      Drop        : in Ada.Strings.Truncation  := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (CBWS.Bounded_Wide_String (Left),
         CBWS.Bounded_Wide_String (Right),
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   function Append
     (Left  : in Bounded_Wide_String;
      Right : in Standard.Wide_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (CBWS.Bounded_Wide_String (Left),
         Right,
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   function Append
     (Left  : in Standard.Wide_String;
      Right : in Bounded_Wide_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (Left,
         CBWS.Bounded_Wide_String (Right),
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   function Append
     (Left  : in Bounded_Wide_String;
      Right : in Wide_Character;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (CBWS.Bounded_Wide_String (Left),
         Right,
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   function Append
     (Left  : in Wide_Character;
      Right : in Bounded_Wide_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Append
        (Left,
         CBWS.Bounded_Wide_String (Right),
         Drop);
   begin
      return Bounded_Wide_String (Result);
   end Append;

   procedure Append
     (Source   : in out Bounded_Wide_String;
      New_Item : in Bounded_Wide_String;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error)
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

   procedure Append
     (Source   : in out Bounded_Wide_String;
      New_Item : in Standard.Wide_String;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error)
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

   procedure Append
     (Source   : in out Bounded_Wide_String;
      New_Item : in Wide_Character;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error)
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

   function "&"
     (Left, Right : in Bounded_Wide_String)
     return        Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        CBWS.Bounded_Wide_String (Left) & CBWS.Bounded_Wide_String (Right);
   begin
      return Bounded_Wide_String (Result);
   end "&";

   function "&"
     (Left  : in Bounded_Wide_String;
      Right : in Standard.Wide_String)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        CBWS.Bounded_Wide_String (Left) & Right;
   begin
      return Bounded_Wide_String (Result);
   end "&";

   function "&"
     (Left  : in Standard.Wide_String;
      Right : in Bounded_Wide_String)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        Left & CBWS.Bounded_Wide_String (Right);
   begin
      return Bounded_Wide_String (Result);
   end "&";

   function "&"
     (Left  : in Bounded_Wide_String;
      Right : in Wide_Character)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String :=
        CBWS.Bounded_Wide_String (Left) & Right;
   begin
      return Bounded_Wide_String (Result);
   end "&";

   function "&"
     (Left  : in Wide_Character;
      Right : in Bounded_Wide_String)
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

   function Element
     (Source : in Bounded_Wide_String;
      Index  : in Positive)
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

   procedure Replace_Element
     (Source : in out Bounded_Wide_String;
      Index  : in Positive;
      By     : in Wide_Character)
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

   function Slice
     (Source : in Bounded_Wide_String;
      Low    : in Positive;
      High   : in Natural)
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

   function "="  (Left, Right : in Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) = CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "=";

   function "="
     (Left  : in Bounded_Wide_String;
      Right : in Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) = Right;
   begin
      return Result;
   end "=";

   function "="
     (Left  : in Standard.Wide_String;
      Right : in Bounded_Wide_String)
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

   function "<"  (Left, Right : in Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) < CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "<";

   function "<"
     (Left  : in Bounded_Wide_String;
      Right : in Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) < Right;
   begin
      return Result;
   end "<";

   function "<"
     (Left  : in Standard.Wide_String;
      Right : in Bounded_Wide_String)
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

   function "<=" (Left, Right : in Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) <= CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end "<=";

   function "<="
     (Left  : in Bounded_Wide_String;
      Right : in Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) <= Right;
   begin
      return Result;
   end "<=";

   function "<="
     (Left  : in Standard.Wide_String;
      Right : in Bounded_Wide_String)
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

   function ">"  (Left, Right : in Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) > CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end ">";

   function ">"
     (Left  : in Bounded_Wide_String;
      Right : in Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) > Right;
   begin
      return Result;
   end ">";

   function ">"
     (Left  : in Standard.Wide_String;
      Right : in Bounded_Wide_String)
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

   function ">=" (Left, Right : in Bounded_Wide_String) return Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) >= CBWS.Bounded_Wide_String (Right);
   begin
      return Result;
   end ">=";

   function ">="
     (Left  : in Bounded_Wide_String;
      Right : in Standard.Wide_String)
     return  Boolean
   is
      Result : constant Boolean :=
        CBWS.Bounded_Wide_String (Left) >= Right;
   begin
      return Result;
   end ">=";

   function ">="
     (Left  : in Standard.Wide_String;
      Right : in Bounded_Wide_String)
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

   function Index
     (Source  : in Bounded_Wide_String;
      Pattern : in Standard.Wide_String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Wide_Maps.Wide_Character_Mapping
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

   function Index
     (Source  : in Bounded_Wide_String;
      Pattern : in Standard.Wide_String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Wide_Maps.Wide_Character_Mapping_Function)
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

   function Index
     (Source : in Bounded_Wide_String;
      Set    : in Ada.Strings.Wide_Maps.Wide_Character_Set;
      Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : in Ada.Strings.Direction  := Ada.Strings.Forward)
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

   function Index_Non_Blank
     (Source : in Bounded_Wide_String;
      Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
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

   function Count
     (Source  : in Bounded_Wide_String;
      Pattern : in Standard.Wide_String;
      Mapping : in Ada.Strings.Wide_Maps.Wide_Character_Mapping
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

   function Count
     (Source  : in Bounded_Wide_String;
      Pattern : in Standard.Wide_String;
      Mapping : in Ada.Strings.Wide_Maps.Wide_Character_Mapping_Function)
     return    Natural
   is
      Result : constant Natural := CBWS.Count
        (CBWS.Bounded_Wide_String (Source),
         Pattern,
         Mapping);
   begin
      return Result;
   end Count;

   function Count
     (Source : in Bounded_Wide_String;
      Set    : in Ada.Strings.Wide_Maps.Wide_Character_Set)
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

   procedure Find_Token
     (Source : in Bounded_Wide_String;
      Set    : in Ada.Strings.Wide_Maps.Wide_Character_Set;
      Test   : in Ada.Strings.Membership;
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

   function Translate
     (Source   : in Bounded_Wide_String;
      Mapping  : in Ada.Strings.Wide_Maps.Wide_Character_Mapping)
     return     Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Translate
        (CBWS.Bounded_Wide_String (Source),
         Mapping);
   begin
      return Bounded_Wide_String (Result);
   end Translate;

   procedure Translate
     (Source   : in out Bounded_Wide_String;
      Mapping  : in Ada.Strings.Wide_Maps.Wide_Character_Mapping)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Translate (CBWS_Source, Mapping);
      Source := Bounded_Wide_String (CBWS_Source);
   end Translate;

   function Translate
     (Source  : in Bounded_Wide_String;
      Mapping : in Ada.Strings.Wide_Maps.Wide_Character_Mapping_Function)
     return    Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Translate
        (CBWS.Bounded_Wide_String (Source),
         Mapping);
   begin
      return Bounded_Wide_String (Result);
   end Translate;

   procedure Translate
     (Source  : in out Bounded_Wide_String;
      Mapping : in Ada.Strings.Wide_Maps.Wide_Character_Mapping_Function)
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

   function Replace_Slice
     (Source   : in Bounded_Wide_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Standard.Wide_String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   procedure Replace_Slice
     (Source   : in out Bounded_Wide_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Standard.Wide_String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   function Insert
     (Source   : in Bounded_Wide_String;
      Before   : in Positive;
      New_Item : in Standard.Wide_String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   procedure Insert
     (Source   : in out Bounded_Wide_String;
      Before   : in Positive;
      New_Item : in Standard.Wide_String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   function Overwrite
     (Source    : in Bounded_Wide_String;
      Position  : in Positive;
      New_Item  : in Standard.Wide_String;
      Drop      : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   procedure Overwrite
     (Source    : in out Bounded_Wide_String;
      Position  : in Positive;
      New_Item  : in Standard.Wide_String;
      Drop      : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   function Delete
     (Source  : in Bounded_Wide_String;
      From    : in Positive;
      Through : in Natural)
     return    Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Delete
        (CBWS.Bounded_Wide_String (Source),
         From,
         Through);
   begin
      return Bounded_Wide_String (Result);
   end Delete;

   procedure Delete
     (Source  : in out Bounded_Wide_String;
      From    : in Positive;
      Through : in Natural)
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

   function Trim
     (Source : in Bounded_Wide_String;
      Side   : in Ada.Strings.Trim_End)
     return   Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Trim
        (CBWS.Bounded_Wide_String (Source), Side);
   begin
      return Bounded_Wide_String (Result);
   end Trim;

   procedure Trim
     (Source : in out Bounded_Wide_String;
      Side   : in Ada.Strings.Trim_End)
   is
      CBWS_Source : CBWS.Bounded_Wide_String := CBWS.Bounded_Wide_String
        (Source);
   begin
      CBWS.Trim (CBWS_Source, Side);
      Source := Bounded_Wide_String (CBWS_Source);
   end Trim;

   function Trim
     (Source  : in Bounded_Wide_String;
      Left   : in Ada.Strings.Wide_Maps.Wide_Character_Set;
      Right  : in Ada.Strings.Wide_Maps.Wide_Character_Set)
     return   Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Trim
        (CBWS.Bounded_Wide_String (Source), Left, Right);
   begin
      return Bounded_Wide_String (Result);
   end Trim;

   procedure Trim
     (Source : in out Bounded_Wide_String;
      Left   : in Ada.Strings.Wide_Maps.Wide_Character_Set;
      Right  : in Ada.Strings.Wide_Maps.Wide_Character_Set)
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

   function Head
     (Source : in Bounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Ada.Strings.Wide_Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
     return   Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Head
        (CBWS.Bounded_Wide_String (Source), Count, Pad, Drop);
   begin
      return Bounded_Wide_String (Result);
   end Head;

   procedure Head
     (Source : in out Bounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Ada.Strings.Wide_Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   function Tail
     (Source : in Bounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character  := Ada.Strings.Wide_Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
     return Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Tail
        (CBWS.Bounded_Wide_String (Source), Count, Pad, Drop);
   begin
      return Bounded_Wide_String (Result);
   end Tail;

   procedure Tail
     (Source : in out Bounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character  := Ada.Strings.Wide_Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
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

   function "*"
     (Left  : in Natural;
      Right : in Wide_Character)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := Left * Right;
   begin
      return Bounded_Wide_String (Result);
   end "*";

   function "*"
     (Left  : in Natural;
      Right : in Standard.Wide_String)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := Left * Right;
   begin
      return Bounded_Wide_String (Result);
   end "*";

   function "*"
     (Left  : in Natural;
      Right : in Bounded_Wide_String)
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

   function Replicate
     (Count : in Natural;
      Item  : in Wide_Character;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Replicate
        (Count, Item, Drop);
   begin
      return Bounded_Wide_String (Result);
   end Replicate;

   function Replicate
     (Count : in Natural;
      Item  : in Standard.Wide_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Replicate
        (Count, Item, Drop);
   begin
      return Bounded_Wide_String (Result);
   end Replicate;

   function Replicate
     (Count : in Natural;
      Item  : in Bounded_Wide_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_Wide_String
   is
      Result : constant CBWS.Bounded_Wide_String := CBWS.Replicate
        (Count, CBWS.Bounded_Wide_String (Item), Drop);
   begin
      return Bounded_Wide_String (Result);
   end Replicate;

end CORBA.Bounded_Wide_Strings;
