------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . U T I L S . H T A B L E                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

package PolyORB.Utils.HTables is

   --  Je ne comprends pas pourquoi tous ces types se trouvent la.
   --  Pour l'essentiel, ils ne sont utilisables que par Perfect.
   --  Donc, Perfect devrait etre rapatrie dans ce paquetage.

private

   type String_Access is access all String;

   type Element is record
      Key         : String_Access;
      Deleted     : Boolean;
      Table_Id    : Natural;
      Subtable_Id : Natural;
   end record;
   --  Key         is hashed data (hashcode = h(Key))

   --  Deleted     True when Element is unused

   --  Table_Id    Used by the algorithm when it tries
   --              to find a hash function that satisfies
   --              a condition
   --  Subtable_Id Used by the algorithm when it tries
   --              to find an injective hash function for
   --              the sub_tables

   type Subtable is record
      Size  : Natural;
      K : Natural;
      Max_Elements : Natural;
      N_Elements : Natural;
      First : Natural;
   end record;
   --  Size                is the size of the Subtable in the memory
   --                      and define range of the hash function
   --  Max_Elements        is an upper bound fixed by the algorithm.
   --                      when N_Elements is superior to
   --                      Max_Elements the algorithm needs to reorganize
   --                      the sub_table
   --  N_Elements          Actual number of Element stored in the sub_table
   --
   --  Number_Of_Elements < Max_Elements < Size

   --  First               First subtable index

   --  K                   is used to choose the hash function for
   --                      the sub_table
   --                      h(Key) = ((K * Key) mod Prime) mod Size

   type Table_Info is record
      Size : Natural;
      Prime : Natural;
      K : Natural;
      Max_Elements : Natural;
      Count : Natural;
   end record;
   --  P             Prime number for all the hash functions
   --                h(Key) = ((K * Key) mod Prime) mod Size

   --  K             is used to choose the hash function for
   --                the table
   --                h(Key) = ((K * Key) mod Prime) mod Size

   --  Count         is a variable that count the number of
   --                modification (insertion) in the table
   --  N_Sub_Tables  Number of Subtables
   --  M             when Count > M the algorithm needs to
   --                reorganize all the sub_tables (worst
   --                case)

   type Element_Array is array (Natural range <>) of Element;
   type Element_Array_Ptr is access all Element_Array;

   type Subtable_Array is array (Natural range <>) of Subtable;
   type Subtable_Array_Ptr is access all Subtable_Array;

   type Hash_Table is record
      Info      : Table_Info;
      Elements  : Element_Array_Ptr;
      Subtables : Subtable_Array_Ptr;
   end record;

   procedure Initialize
     (T      : out Hash_Table;
      Prime  : Natural;
      Length : Natural);
   --  Prime is a prime number used by hash functions. Length is the
   --  max number of elements that can be stored.

   procedure Destroy
     (T : in out Hash_Table);

   procedure Lookup
     (T   : Hash_Table;
      Key : String;
      Index1 : out Natural;
      Index2 : out Natural);
   --  Key is the string to be hashed

   procedure Insert
     (T     : Hash_Table;
      Key   : String);
   --  Key is the string to be hashed
   --  Value is the Item associated with Key

   procedure Delete
     (T   : Hash_Table;
      Key : String);
   --  Key is the string to be hashed
end PolyORB.Utils.HTables;
