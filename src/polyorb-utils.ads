------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O L Y O R B . U T I L S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If  --
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

--  Miscellaneous utility subprograms.

--  $Id$

with Ada.Streams;

package PolyORB.Utils is

   pragma Pure;

   function Hex_Value (C : Character) return Integer;
   --  The integer value corresponding to hexadecimal digit C.
   --  If C is not a valid hexadecimal digit, Constraint_Error
   --  is raised.

   function To_String
     (A : Ada.Streams.Stream_Element_Array)
     return String;
   --  Return a string of hexadecimal digits representing the
   --  contents of A.

   function To_Stream_Element_Array
     (S : String)
     return Ada.Streams.Stream_Element_Array;
   --  Return the Stream_Element_Array represented by the
   --  string of hexadecimal digits contaned in S.

   function URI_Encode (S : String) return String;
   --  Return S with special characters replaced by
   --  "%" "hexdigit" "hexdigit" if these characters
   --  need to be escaped in URIs, except for spaces
   --  which are replaced by '+'.

   function URI_Decode (S : String) return String;
   --  Return S with any %xy sequence replaced with
   --  the character whose hexadecimal representation
   --  is xy, and any '+' characters replaced by spaces.

   -----------------------
   -- String operations --
   -----------------------

   function Trimmed_Image (I : Integer) return String;
   --  Return Integer'Image (I) without a leading space.

   function Find_Skip
     (S     : String;
      Start : Integer;
      What  : Character;
      Skip  : Boolean)
     return Integer;
   --  If Skip is False, return the index of the
   --  first occurrence of What in S.
   --  If Skip is True, return the index of the
   --  first occurrence of any character OTHER THAN What.
   --  If no such character exists, S'Last + 1 is returned.

   --  Shorthands for commonly-used forms of Find_Skip

   function Find
     (S     : String;
      Start : Integer;
      What  : Character;
      Skip  : Boolean := False)
     return Integer
     renames Find_Skip;

   function Find_Whitespace
     (S     : String;
      Start : Integer;
      What  : Character := ' ';
      Skip  : Boolean := False)
     return Integer
     renames Find_Skip;

   function Skip_Whitespace
     (S     : String;
      Start : Integer;
      What  : Character := ' ';
      Skip  : Boolean := True)
     return Integer
     renames Find_Skip;

end PolyORB.Utils;
