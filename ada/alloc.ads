------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A L L O C                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

package Alloc is

--  This package contains definitions for initial sizes and growth increments
--  for the various dynamic arrays used for principle compiler data strcutures.
--  The indicated initial size is allocated for the start of each file, and
--  the increment factor is a percentage used to increase the table size when
--  it needs expanding (e.g. a value of 100 = 100% increase = double)

   Alloc_Elists_Initial : constant := 1_000;
   --  Initial allocation in Elist headers for tree (Elists)

   Alloc_Elists_Increment : constant := 150;
   --  Incremental allocation factor for Elist headers table (Elists)

   Alloc_Elmts_Initial : constant := 4_000;
   --  Initial allocation in Elmts table (Elists)

   Alloc_Elmts_Increment : constant := 150;
   --  Incremental allocation factor for Elmts table (Elists)

   Alloc_Feature_List_Initial : constant := 100;
   --  Initial allocation for Feature_List table (Features)

   Alloc_Feature_List_Increment : constant := 300;
   --  Incremental allocation factor for Feature_List table (Features)

   Alloc_Hash_Sub_Initial : constant := 2048;
   --  Initial allocation in tree substitution hash table (Atree)

   Alloc_Hash_Sub_Increment : constant := 300;
   --  Incremental allocation factor in tree substitution hash table (Atree)

   Alloc_Lines_Initial : constant := 4_000;
   --  Initial allocation for lines table (Lib, Lib.Load)

   Alloc_Lines_Increment : constant := 150;
   --  Incremental allocation factor for lines table (Sinput)

   Alloc_Lists_Initial : constant := 4_000;
   --  Initial allocation in list headers for tree (Nlists)

   Alloc_Lists_Increment : constant := 200;
   --  Incremental allocation factor for list table (Nlists)

   Alloc_Name_Chars_Initial : constant := 64_000;
   --  Initial allocation for name characters table (Namet)

   Alloc_Name_Chars_Increment : constant := 100;
   --  Incremental allocation factor for name characters table (Namet)

   Alloc_Names_Initial : constant := 4_000;
   --  Initial allocation of entries in names table (Namet)

   Alloc_Names_Increment : constant := 100;
   --  Incremental allocation factor of entries in names table (Namet)

   Alloc_Nodes_Initial : constant := 8_000;
   --  Initial allocation in nodes for tree (Atree)

   Alloc_Nodes_Increment : constant := 200;
   --  Incremental allocation factor for nodes table (Atree)

   Alloc_SFN_Table_Initial : constant := 50;
   --  Initial allocation in entries for source file name table (Fname)

   Alloc_SFN_Table_Increment : constant := 200;
   --  Incremental allocatoin factor for source file name table (Fname)

   Alloc_String_Chars_Initial : constant := 64_000;
   --  Initial allocation for name characters table (Stringt)

   Alloc_String_Chars_Increment : constant := 150;
   --  Incremental allocation factor for name characters table (Stringt)

   Alloc_Strings_Initial : constant := 500;
   --  Initial allocation of entries in names table (Stringt)

   Alloc_Strings_Increment : constant := 150;
   --  Incremental allocation factor of entries in names table (Stringt)

   Alloc_Udigits_Initial : constant := 50_000;
   --  Initial allocation for Uint digits table (Uintp)

   Alloc_Udigits_Increment : constant := 100;
   --  Incremental allocation factor Uint digits table (Uintp)

   Alloc_Uints_Initial : constant := 20_000;
   --  Initial allocation for uint table in digits (Uintp)

   Alloc_Uints_Increment : constant := 100;
   --  Incremental allocation factor for Uint table in digits (Uintp)

   Alloc_Ureals_Initial : constant := 2000;
   --  Initial allocation for universal real table in entries (Urealp)

   Alloc_Ureals_Increment : constant := 100;
   --  Incremental allocation factor for universal real table (Urealp)

   Alloc_With_List_Initial : constant := 100;
   --  Initial allocation for With_List table (Features)

   Alloc_With_List_Increment : constant := 300;
   --  Incremental allocation factor for With_List table (Features)

end Alloc;
