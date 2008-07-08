------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             X E _ T Y P E S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2006 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides all the basic types used in gnatdist.

with Ada.Unchecked_Deallocation;

package XE_Types is
pragma Preelaborate (XE_Types);

--  This package contains host independent type definitions which are used
--  in more than one unit in the compiler. They are gathered here for easy
--  reference, though in some cases the full description is found in the
--  relevant module which implements the definition. The main reason that
--  they are not in their "natural" specs is that this would cause a lot of
--  inter-spec dependencies, and in particular some awkward circular
--  dependencies would have to be dealt with.

   -------------------------------
   -- General Use Integer XE_Types --
   -------------------------------

   type Int is range -2 ** 31 .. +2 ** 31 - 1;
   for Int'Size use 32;
   --  Signed 32-bit integer

   type Short is range -32768 .. +32767;
   for Short'Size use 16;
   --  16-bit signed integer

   subtype Nat is Int range 0 .. Int'Last;
   --  Non-negative Int values

   subtype Pos is Int range 1 .. Int'Last;
   --  Positive Int values

   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;
   --  8-bit unsigned integer

   type size_t is mod 2 ** Standard'Address_Size;
   --  Memory size value, for use in calls to C routines

   --------------------------------------
   -- 8-Bit Character and String XE_Types --
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

   type Character_Ptr is access all Character;
   type String_Ptr    is access all String;
   --  Standard character and string pointers

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);
   --  Procedure for freeing dynamically allocated String values

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

   procedure Free is
      new Ada.Unchecked_Deallocation (Text_Buffer, Text_Buffer_Ptr);
   --  Procedure for freeing dynamically allocated text buffers

   --------------------------------
   -- Types for XE_Names Package --
   --------------------------------

   --  Name_Id values are used to identify entries in the names table. Except
   --  for the special values No_Name, and Error_Name, they are subscript
   --  values for the Names table defined in package XE_Names.

   --  Note that with only a few exceptions, which are clearly documented, the
   --  type Name_Id should be regarded as a private type. In particular it is
   --  never appropriate to perform arithmetic operations using this type.

   Names_Low_Bound : constant := 300_000_000;
   --  Low bound for name Id values

   Names_High_Bound : constant := 399_999_999;
   --  Maximum number of names that can be allocated is 100 million, which is
   --  in practice infinite and there is no need to check the range.

   type Name_Id is range Names_Low_Bound .. Names_High_Bound;
   for Name_Id'Size use 32;
   --  Type used to identify entries in the names table

   No_Str   : constant String := "";

   No_Name : constant Name_Id := Names_Low_Bound;
   --  The special Name_Id value No_Name is used in the parser to indicate
   --  a situation where no name is present (e.g. on a loop or block).

   First_Name_Id : constant Name_Id := Names_Low_Bound + 2;
   --  Subscript of first entry in names table

   subtype File_Name_Type is Name_Id;
   --  File names are stored in the names table and this synonym is used to
   --  indicate that a Name_Id value is being used to hold a simple file
   --  name (which does not include any directory information).

   No_File_Name : constant File_Name_Type := File_Name_Type (No_Name);
   --  Constant used to indicate no file found

   subtype Unit_Name_Type is Name_Id;
   --  Unit names are stored in the names table and this synonym is used to
   --  indicate that a Name_Id value is being used to hold a unit name.

   -----------------------------------
   -- Representation of Time Stamps --
   -----------------------------------

   --  All compiled units are marked with a time stamp which is derived from
   --  the source file (we assume that the host system has the concept of a
   --  file time stamp which is modified when a file is modified). These
   --  time stamps are used to ensure consistency of the set of units that
   --  constitutes a library. Time stamps are 12 character strings with
   --  with the following format:

   --     YYYYMMDDHHMMSS

   --       YYYY   year
   --       MM     month (2 digits 01-12)
   --       DD     day (2 digits 01-31)
   --       HH     hour (2 digits 00-23)
   --       MM     minutes (2 digits 00-59)
   --       SS     seconds (2 digits 00-59)

   --  In the case of Unix systems (and other systems which keep the time in
   --  GMT), the time stamp is the GMT time of the file, not the local time.
   --  This solves problems in using libraries across networks with clients
   --  spread across multiple time-zones.

   Time_Stamp_Length : constant := 14;
   --  Length of time stamp value

   subtype Time_Stamp_Index is Natural range 1 .. Time_Stamp_Length;
   type Time_Stamp_Type is new String (Time_Stamp_Index);
   --  Type used to represent time stamp

   Empty_Time_Stamp : constant Time_Stamp_Type := (others => ' ');
   --  Type used to represent an empty or missing time stamp. Looks less
   --  than any real time stamp if two time stamps are compared. Note that

   --  although this is not a private type, clients should not rely on the
   --  exact way in which this string is represented, and instead should
   --  use the subprograms below.

   Dummy_Time_Stamp : constant Time_Stamp_Type := (others => '0');
   --  This is used for dummy time stamp values used in the D lines for
   --  non-existant files, and is intended to be an impossible value.

   type Node_Id is new Int;
   No_Node : constant Node_Id := 0;

   function Present (E : Node_Id) return Boolean;
   --  Return true when E is not No_Node

   function No (E : Node_Id) return Boolean;
   --  Return true when E is No_Node

   procedure Dummy (E : Node_Id);

   type List_Id is new Node_Id;
   No_List : constant List_Id := 0;

end XE_Types;
