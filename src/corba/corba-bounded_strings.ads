------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                C O R B A . B O U N D E D _ S T R I N G S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

--  As defined by the IDL-to-Ada mapping, this package has the same
--  specification and semantic as the
--  Ada.Strings.Bounded.Generic_Bounded_Length package

with Ada.Strings.Maps;
with Ada.Strings.Bounded;

generic
   Max : Positive;
   --  Maximum length of a Bounded_String

package CORBA.Bounded_Strings is

   Max_Length : constant Positive := Max;

   type Bounded_String is private;

   Null_Bounded_String : constant Bounded_String;

   subtype Length_Range is Natural range 0 .. Max_Length;

   function Length (Source : in Bounded_String) return Length_Range;

   ------------------------------
   -- Any conversion functions --
   ------------------------------

   function TC_Bounded_String return CORBA.TypeCode.Object;

   function From_Any (From : CORBA.Any) return Bounded_String;

   function To_Any (To : Bounded_String) return CORBA.Any;

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Bounded_String
     (Source : in Standard.String;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
     return   Bounded_String;

   function To_String (Source : in Bounded_String) return Standard.String;

   function Append
     (Left, Right : in Bounded_String;
      Drop        : in Ada.Strings.Truncation  := Ada.Strings.Error)
     return        Bounded_String;

   function Append
     (Left  : in Bounded_String;
      Right : in Standard.String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String;

   function Append
     (Left  : in Standard.String;
      Right : in Bounded_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String;

   function Append
     (Left  : in Bounded_String;
      Right : in Character;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String;

   function Append
     (Left  : in Character;
      Right : in Bounded_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String;

   procedure Append
     (Source   : in out Bounded_String;
      New_Item : in Bounded_String;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error);

   procedure Append
     (Source   : in out Bounded_String;
      New_Item : in Standard.String;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error);

   procedure Append
     (Source   : in out Bounded_String;
      New_Item : in Character;
      Drop     : in Ada.Strings.Truncation  := Ada.Strings.Error);

   function "&"
     (Left, Right : in Bounded_String)
     return        Bounded_String;

   function "&"
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Bounded_String;

   function "&"
     (Left  : in Standard.String;
      Right : in Bounded_String)
     return  Bounded_String;

   function "&"
     (Left  : in Bounded_String;
      Right : in Character)
     return  Bounded_String;

   function "&"
     (Left  : in Character;
      Right : in Bounded_String)
     return  Bounded_String;

   function Element
     (Source : in Bounded_String;
      Index  : in Positive)
     return   Character;

   procedure Replace_Element
     (Source : in out Bounded_String;
      Index  : in Positive;
      By     : in Character);

   function Slice
     (Source : in Bounded_String;
      Low    : in Positive;
      High   : in Natural)
     return   Standard.String;

   function "="  (Left, Right : in Bounded_String) return Boolean;

   function "="
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean;

   function "="
     (Left  : in Standard.String;
      Right : in Bounded_String)
     return  Boolean;

   function "<"  (Left, Right : in Bounded_String) return Boolean;

   function "<"
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean;

   function "<"
     (Left  : in Standard.String;
      Right : in Bounded_String)
     return  Boolean;

   function "<=" (Left, Right : in Bounded_String) return Boolean;

   function "<="
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean;

   function "<="
     (Left  : in Standard.String;
      Right : in Bounded_String)
     return  Boolean;

   function ">"  (Left, Right : in Bounded_String) return Boolean;

   function ">"
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean;

   function ">"
     (Left  : in Standard.String;
      Right : in Bounded_String)
     return  Boolean;

   function ">=" (Left, Right : in Bounded_String) return Boolean;

   function ">="
     (Left  : in Bounded_String;
      Right : in Standard.String)
     return  Boolean;

   function ">="
     (Left  : in Standard.String;
      Right : in Bounded_String)
     return  Boolean;

   ----------------------
   -- Search Functions --
   ----------------------

   function Index
     (Source  : in Bounded_String;
      Pattern : in Standard.String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity)
     return    Natural;

   function Index
     (Source  : in Bounded_String;
      Pattern : in Standard.String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
     return    Natural;

   function Index
     (Source : in Bounded_String;
      Set    : in Ada.Strings.Maps.Character_Set;
      Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : in Ada.Strings.Direction  := Ada.Strings.Forward)
     return   Natural;

   function Index_Non_Blank
     (Source : in Bounded_String;
      Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
     return   Natural;

   function Count
     (Source  : in Bounded_String;
      Pattern : in Standard.String;
      Mapping : in Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity)
     return    Natural;

   function Count
     (Source  : in Bounded_String;
      Pattern : in Standard.String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
     return    Natural;

   function Count
     (Source : in Bounded_String;
      Set    : in Ada.Strings.Maps.Character_Set)
     return   Natural;

   procedure Find_Token
     (Source : in Bounded_String;
      Set    : in Ada.Strings.Maps.Character_Set;
      Test   : in Ada.Strings.Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source   : in Bounded_String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping)
     return     Bounded_String;

   procedure Translate
     (Source   : in out Bounded_String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping);

   function Translate
     (Source  : in Bounded_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
     return    Bounded_String;

   procedure Translate
     (Source  : in out Bounded_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source   : in Bounded_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Standard.String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
     return     Bounded_String;

   procedure Replace_Slice
     (Source   : in out Bounded_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Standard.String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error);

   function Insert
     (Source   : in Bounded_String;
      Before   : in Positive;
      New_Item : in Standard.String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error)
     return     Bounded_String;

   procedure Insert
     (Source   : in out Bounded_String;
      Before   : in Positive;
      New_Item : in Standard.String;
      Drop     : in Ada.Strings.Truncation := Ada.Strings.Error);

   function Overwrite
     (Source    : in Bounded_String;
      Position  : in Positive;
      New_Item  : in Standard.String;
      Drop      : in Ada.Strings.Truncation := Ada.Strings.Error)
     return      Bounded_String;

   procedure Overwrite
     (Source    : in out Bounded_String;
      Position  : in Positive;
      New_Item  : in Standard.String;
      Drop      : in Ada.Strings.Truncation := Ada.Strings.Error);

   function Delete
     (Source  : in Bounded_String;
      From    : in Positive;
      Through : in Natural)
     return    Bounded_String;

   procedure Delete
     (Source  : in out Bounded_String;
      From    : in Positive;
      Through : in Natural);

   ---------------------------------
   -- String Selector Subprograms --
   ---------------------------------

   function Trim
     (Source : in Bounded_String;
      Side   : in Ada.Strings.Trim_End)
     return   Bounded_String;

   procedure Trim
     (Source : in out Bounded_String;
      Side   : in Ada.Strings.Trim_End);

   function Trim
     (Source  : in Bounded_String;
      Left   : in Ada.Strings.Maps.Character_Set;
      Right  : in Ada.Strings.Maps.Character_Set)
     return   Bounded_String;

   procedure Trim
     (Source : in out Bounded_String;
      Left   : in Ada.Strings.Maps.Character_Set;
      Right  : in Ada.Strings.Maps.Character_Set);

   function Head
     (Source : in Bounded_String;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
     return   Bounded_String;

   procedure Head
     (Source : in out Bounded_String;
      Count  : in Natural;
      Pad    : in Character  := Ada.Strings.Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error);

   function Tail
     (Source : in Bounded_String;
      Count  : in Natural;
      Pad    : in Character  := Ada.Strings.Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error)
     return Bounded_String;

   procedure Tail
     (Source : in out Bounded_String;
      Count  : in Natural;
      Pad    : in Character  := Ada.Strings.Space;
      Drop   : in Ada.Strings.Truncation := Ada.Strings.Error);

   ------------------------------------
   -- String Constructor Subprograms --
   ------------------------------------

   function "*"
     (Left  : in Natural;
      Right : in Character)
     return  Bounded_String;

   function "*"
     (Left  : in Natural;
      Right : in Standard.String)
     return  Bounded_String;

   function "*"
     (Left  : in Natural;
      Right : in Bounded_String)
     return  Bounded_String;

   function Replicate
     (Count : in Natural;
      Item  : in Character;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String;

   function Replicate
     (Count : in Natural;
      Item  : in Standard.String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String;

   function Replicate
     (Count : in Natural;
      Item  : in Bounded_String;
      Drop  : in Ada.Strings.Truncation := Ada.Strings.Error)
     return  Bounded_String;

private

   package CBS is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_Length);

   type Bounded_String is new CBS.Bounded_String;

   Null_Bounded_String : constant Bounded_String
     := Bounded_String (CBS.Null_Bounded_String);

end CORBA.Bounded_Strings;
