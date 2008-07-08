------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O L Y O R B . U T I L S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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

--  Miscellaneous utility subprograms

with Ada.Streams;

package PolyORB.Utils is

   pragma Pure;

   function Hex_Value (C : Character) return Integer;
   --  The integer value corresponding to hexadecimal digit C.
   --  If C is not a valid hexadecimal digit, Constraint_Error
   --  is raised.

   function SEA_To_Hex_String
     (A : Ada.Streams.Stream_Element_Array)
     return String;
   --  Return a string of hexadecimal digits representing the
   --  contents of A.

   function Hex_String_To_SEA
     (S : String)
     return Ada.Streams.Stream_Element_Array;
   --  Return the Stream_Element_Array represented by the
   --  string of hexadecimal digits contained in S.

   No_Escape : constant String := "";

   function URI_Encode
     (S           : String;
      Also_Escape : String := "/") return String;
   --  Implement the encoding scheme defined in the RFC 2396.
   --  Return S with special characters replaced by "%" "hexdigit"
   --  "hexdigit" if these characters need to be escaped in URIs. Any
   --  character in Also_Escape is considered as special.

   function URI_Decode (S : String) return String;
   --  Implement the decoding scheme defined in the RFC 2396.
   --  Return S with any %xy sequence replaced with the character
   --  whose hexadecimal representation is xy.

   -----------------------
   -- String operations --
   -----------------------

   type Direction_Type is private;

   Forward  : constant Direction_Type;
   Backward : constant Direction_Type;

   function Find_Skip
     (S         : String;
      Start     : Integer;
      What      : Character;
      Skip      : Boolean;
      Direction : Direction_Type)
     return Integer;
   --  If Skip is False, return the index of the
   --  first occurrence of What in S starting at Start and going
   --  in the indicated direction (which can be Forward or Backward).
   --  If Skip is True, return the index of the
   --  first occurrence of any character OTHER THAN What.
   --  If no such character exists, S'Last + 1 is returned.

   --  Shorthands for commonly-used forms of Find_Skip

   function Find
     (S         : String;
      Start     : Integer;
      What      : Character;
      Skip      : Boolean        := False;
      Direction : Direction_Type := Forward)
     return Integer
     renames Find_Skip;

   function Find_Whitespace
     (S         : String;
      Start     : Integer;
      What      : Character      := ' ';
      Skip      : Boolean        := False;
      Direction : Direction_Type := Forward)
     return Integer
     renames Find_Skip;

   function Skip_Whitespace
     (S         : String;
      Start     : Integer;
      What      : Character      := ' ';
      Skip      : Boolean        := True;
      Direction : Direction_Type := Forward)
     return Integer
     renames Find_Skip;

   function Has_Prefix (S : String; Prefix : String) return Boolean;
   --  True if, and only if, S starts with Prefix

   function To_Upper (S : String) return String;
   --  Folds all characters of string S to upper case

   function To_Lower (S : String) return String;
   --  Folds all characters of string S to lower case

private
   type Direction_Type is new Integer range -1 .. +1;
   Forward  : constant Direction_Type := +1;
   Backward : constant Direction_Type := -1;
   --  Direction_Type value 0 does not make sense

   pragma Inline (Hex_Value, SEA_To_Hex_String, Hex_String_To_SEA,
                  URI_Encode, URI_Decode, Find_Skip);

end PolyORB.Utils;
