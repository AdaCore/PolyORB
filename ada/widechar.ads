------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             W I D E C H A R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  Subprograms for manipulation of wide character sequences

with Types; use Types;

package Widechar is

   function Length_Wide return Nat;
   --  Returns the maximum length in characters for the escape sequence that
   --  is used to encode wide character literals outside the ASCII range. Used
   --  only in the implementation of the attribute Width for Wide_Character.

   procedure Scan_Wide
     (S   : Source_Buffer_Ptr;
      P   : in out Source_Ptr;
      C   : out Char_Code;
      Err : out Boolean);
   --  On entry S (P) points to the first character in the source text for
   --  a wide character (i.e. to an ESC character, a left bracket, or an
   --  upper half character, depending on the representation method). A
   --  single wide character is scanned. If no error is found, the value
   --  stored in C is the code for this wide character, P is updated past
   --  the sequence and Err is set to False. If an error is found, then
   --  P points to the improper character, C is undefined, and Err is
   --  set to True.

   procedure Set_Wide
     (C : Char_Code;
      S : in out String;
      P : in out Natural);
   --  The escape sequence (including any leading ESC character) for the
   --  given character code is stored starting at S (P + 1), and on return
   --  P points to the last stored character (i.e. P is the count of stored
   --  characters on entry and exit, and the escape sequence is appended to
   --  the end of the stored string). The character code C represents a code
   --  originally constructed by Scan_Wide, so it is known to be in a range
   --  that is appropriate for the encoding method in use.

   procedure Skip_Wide (S : String; P : in out Natural);
   --  On entry, S (P) points to an ESC character for a wide character escape
   --  sequence or to an upper half character if the encoding method uses the
   --  upper bit, or to a left bracket if the brackets encoding method is in
   --  use. On exit, P is bumped past the wide character sequence. No error
   --  checking is done, since this is only used on escape sequences generated
   --  by Set_Wide, which are known to be correct.

   function Is_Start_Of_Wide_Char
     (S    : Source_Buffer_Ptr;
      P    : Source_Ptr)
      return Boolean;
   --  Determines if S (P) is the start of a wide character sequence

end Widechar;
