------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . H T A B L E S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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

--  This package provides dynamic perfect hash tables.

--  $Id$

package PolyORB.Utils.HTables is

private

   type String_Access is access all String;

   type Element is record
      Key         : String_Access;
      Used        : Boolean;
      Table_Id    : Natural;
      Subtable_Id : Natural;
   end record;
   --  Key is the element key in the hash algorithm terminology. When
   --  an element in an array has a Used attribute set to true, this
   --  denotes a non-empty slot. Table_Id and Subtable_Id are indices
   --  to internal tables used by the perfect dynamic hash table
   --  algorithm.

   --  XXXXX If (Used) <=> (Key /= null) then we do not need Used.


   type Subtable is record
      First  : Natural;
      Last   : Natural;
      High   : Natural;
      Max    : Natural;
      K      : Natural;
   end record;
   --  First (resp. Last) is the first (resp. last) subtable index.
   --  Max is the maximum size of the subtable. When Last - First + 1
   --  is greater than High, the algorithm reorganizes the subtable
   --  for algorithm purposes. K is a subtable attribute that ensures
   --  h (Key) = ((K * Key) mod Prime) mod Size.

   type Table_Info is record
      Prime        : Natural;
      Length       : Natural;
      High         : Natural;
      K            : Natural;
   end record;
   --  Prime is a prime number used by the algorithm. It can be
   --  specified by the user. Length is the actual number of elements
   --  in the table. When Length > High, the algorithm reorganizes all
   --  the subtables. K is a table attribute that ensures h (Key) =
   --  ((K * Key) mod Prime) mod Size.

   --  XXXXX : what is the difference between Table.K and Subtable.K.
   --  Why is it duplicated ?

   type Element_Array is array (Natural range <>) of Element;
   type Element_Array_Ptr is access all Element_Array;

   type Subtable_Array is array (Natural range <>) of Subtable;
   type Subtable_Array_Ptr is access all Subtable_Array;

   type Hash_Table is record
      Info      : Table_Info;
      Elements  : Element_Array_Ptr;
      Subtables : Subtable_Array_Ptr;
   end record;
   --  XXXXX : comments ???

   procedure Initialize
     (T      : out Hash_Table;
      Prime  : Natural;
      Max    : Natural);
   --  Prime is a prime number used by hash functions. Max is the max
   --  number of elements to store.

   procedure Finalize
     (T : in out Hash_Table);

   procedure Lookup
     (T      : Hash_Table;
      Key    : String;
      Index1 : out Natural;
      Index2 : out Natural);
   --  Key is the string to hash.

   --  XXXXX : meaning of Index1 and Index2 ?

   procedure Insert
     (T   : Hash_Table;
      Key : String);
   --  Key is the string to hash.
   --  Value is the Item associated with Key

   --  XXXXX where is Value now ???. Why don't we get the hash code back ?

   procedure Delete
     (T   : Hash_Table;
      Key : String);
   --  Key is the string to hash.

end PolyORB.Utils.HTables;
