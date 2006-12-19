--  The package Types is a part of the GNAT compiler.
--  Copyright (C) 1992-1998 Free Software Foundation, Inc.

--  This modified version is a part of the CIAO project.
--  Copyright (C) 1999 École nationale supérieure des télécommunications.

------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T Y P E S                                 --
--                                                                          --
--                                 S p e c                                  --
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

with Unchecked_Deallocation;

package CIAO.Types is
pragma Preelaborate (CIAO.Types);

--  This package contains host independent type definitions which are used
--  in more than one unit in the compiler. They are gathered here for easy
--  reference, though in some cases the full description is found in the
--  relevant module which implements the definition. The main reason that
--  they are not in their "natural" specs is that this would cause a lot of
--  inter-spec dependencies, and in particular some awkward circular
--  dependencies would have to be dealt with.

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file a-types.h

--  Note: the declarations in this package reflect an expectation that the
--  host machine has an efficient integer base type with a range at least
--  32 bits 2s-complement. If there are any machines for which this is not
--  a correct assumption, a significant number of changes will be required!

   -------------------------------
   -- General Use Integer Types --
   -------------------------------

   type Int is range -2 ** 31 .. +2 ** 31 - 1;
   --  Signed 32-bit integer

   subtype Nat is Int range 0 .. Int'Last;
   --  Non-negative Int values

   subtype Pos is Int range 1 .. Int'Last;
   --  Positive Int values

   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer

   type Short is range -32768 .. +32767;
   for Short'Size use 16;
   --  16-bit signed integer

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;
   --  8-bit unsigned integer

   type size_t is mod 2 ** Standard'Address_Size;
   --  Memory size value, for use in calls to C routines

   --------------------------------------
   -- 8-Bit Character and String Types --
   --------------------------------------

   --  We use Standard.Character and Standard.String freely, since we are
   --  compiling ourselves, and we properly implement the required 8-bit
   --  character code as required in Ada 95. This section defines a few
   --  general use constants and subtypes.

   EOF : constant Character := ASCII.SUB;
   --  The character SUB (16#1A#) is used in DOS and other systems derived
   --  from DOS (OS/2, NT etc) to signal the end of a text file. Internally
   --  all source files are ended by an EOF character, even on Unix systems.
   --  An EOF character acts as the end of file only as the last character
   --  of a source buffer, in any other position, it is treated as a blank
   --  if it appears between tokens, and as an illegal character otherwise.
   --  This makes life easier dealing with files that originated from DOS,
   --  including concatenated files with interspersed EOF characters.

   subtype Graphic_Character is Character range ' ' .. '~';
   --  Graphic characters, as defined in ARM

   subtype Line_Terminator is Character range ASCII.LF .. ASCII.CR;
   --  Line terminator characters (LF, VT, FF, CR)

   subtype Upper_Half_Character is
     Character range Character'Val (16#80#) .. Character'Val (16#FF#);
   --  Characters with the upper bit set

   type Character_Ptr   is access all Character;
   type String_Ptr      is access all String;
   type Wide_String_Ptr is access all Wide_String;
   --  Standard character and string pointers

   procedure Free is new Unchecked_Deallocation (String, String_Ptr);
   procedure Free is new Unchecked_Deallocation (Wide_String, Wide_String_Ptr);
   --  Procedure for freeing dynamically allocated String values

--    subtype Word_Hex_String is String (1 .. 8);
--    --  Type used to represent Word value as 8 hex digits, with upper case
--    --  letters for the alphabetic cases.

--    function Get_Hex_String (W : Word) return Word_Hex_String;
--    --  Convert word value to 8-character hex string

   -----------------------------------------
   -- Types Used for Text Buffer Handling --
   -----------------------------------------

   --  We can't use type String for text buffers, since we must use the
   --  standard 32-bit integer as an index value, since we count on all
   --  index values being the same size.

   type Text_Ptr is new Int;
   --  Type used for subscripts in text buffer

   type Text_Buffer is array (Text_Ptr range <>) of Character;
   --  Text buffer used to hold source file or library information file

   type Text_Buffer_Ptr is access all Text_Buffer;
   --  Text buffers for input files are allocated dynamically and this type
   --  is used to reference these text buffers.

   procedure Free is new Unchecked_Deallocation (Text_Buffer, Text_Buffer_Ptr);
   --  Procedure for freeing dynamically allocated text buffers

   ------------------------------------------
   -- Types Used for Source Input Handling --
   ------------------------------------------

   type Logical_Line_Number is range 0 .. Int'Last;
   for Logical_Line_Number'Size use 32;
   --  Line number type, used for storing logical line numbers (i.e. line
   --  numbers that include the line offset from pragma Source_Reference)
   --  Logical line number zero is reserved for the line containing the
   --  Source_Reference pragma at the start of the file.

   No_Line_Number : constant Logical_Line_Number := 0;
   --  Special value used to indicate no line number

   type Column_Number is range 0 .. 32767;
   for Column_Number'Size use 16;
   --  Column number (assume that 2**15 is large enough, see declaration
   --  of Hostparm.Max_Line_Length)

   No_Column_Number : constant Column_Number := 0;
   --  Special value used to indicate no column number

   subtype Source_Buffer is Text_Buffer;
   --  Type used to store text of a source file . The buffer for the main
   --  source (the source specified on the command line) has a lower bound
   --  starting at zero. Subsequent subsidiary sources have lower bounds
   --  which are one greater than the previous upper bound.

   subtype Big_Source_Buffer is Text_Buffer (0 .. Text_Ptr'Last);
   --  This is a virtual type used as the designated type of the access
   --  type Source_Buffer_Ptr, see Osint.Read_Source_File for details.

   type Source_Buffer_Ptr is access all Big_Source_Buffer;
   --  Pointer to source buffer. We use virtual origin addressing for
   --  source buffers, with thin pointers. The pointer points to a virtual
   --  instance of type Big_Source_Buffer, where the actual type is in fact
   --  of type Source_Buffer. The address is adjusted so that the virtual
   --  origin addressing works correctly. See Osint.Read_Source_Buffer for
   --  further details.

   subtype Source_Ptr is Text_Ptr;
   --  Type used to represent a source location, which is a subscript of a
   --  character in the source buffer. As noted above, diffferent source
   --  buffers have different ranges, so it is possible to tell from a
   --  Source_Ptr value which source it refers to. Note that negative numbers
   --  are allowed to accomodate the following special values.

   No_Location : constant Source_Ptr := -1;
   --  Value used to indicate no source position set in a node

   Standard_Location : constant Source_Ptr := -2;
   --  Used for all nodes in the representation of package Standard other
   --  than nodes representing the contents of Standard.ASCII. Note that
   --  testing for <= Standard_Location tests for both Standard_Location
   --  and for Standard_ASCII_Location.

   Standard_ASCII_Location : constant Source_Ptr := -3;
   --  Used for all nodes in the presentation of package Standard.ASCII

   First_Source_Ptr : constant Source_Ptr := 0;
   --  Starting source pointer index value for first source program

end CIAO.Types;
