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
with PolyORB.Utils.Dynamic_Tables;


package PolyORB.Utils.HTables is

   pragma Preelaborate;

private



   type String_Access is access all String;

   type What_To_Do is (Reorder_SubTable,
                       Reorder_Table,
                       Do_Nothing,
                       Insert_Item);
   --  What_To_Do is en enum type used by Insert in order to indicates
   --  what to do after

   type Element is record
      Key        : String_Access;
      Used       : Boolean;
      ST_Index   : Natural;
      ST_Offset  : Natural;
      Item_Index : Natural;
   end record;
   --  Key is the element key in the hash algorithm terminology. When
   --  an element in an array has a Used attribute set to true, this
   --  denotes a non-empty slot. ST_Index corresponds to the subtable
   --  index, ST_Offset to the offset in this subtable and Item_Index
   --  to the position of the value associated with the key (it is
   --  useful for the children package)

   Empty : constant Element := Element'(null, False, 0, 0, 0);

   type Subtable is record
      First  : Natural;
      Last   : Natural;
      Count  : Natural;
      High   : Natural;
      Max    : Natural;
      K      : Natural;
   end record;
   --  First (resp. Last) is the first (resp. last) subtable index.
   --  Some slots between First and Last may be unused. Count
   --  represents the actual number of used elements in the
   --  subtable. Max is the maximum size of the subtable. When Count
   --  is greater than High, the algorithm reorganizes the table
   --  for algorithm purposes. K is a subtable attribute that ensures
   --  h (Key) = ((K * Key) mod Prime) mod (Last - First + 1).

   type Table_Info is record
      Prime        : Natural;
      Count        : Natural;
      High         : Natural;
      N_Subtables  : Natural;
      K            : Natural;
   end record;
   --  Prime is a prime number used by the algorithm. It can be
   --  specified by the user. Count is the  number of Key stored
   --  in the table. When Count = High, the algorithm can't add
   --  more elements. K is a table attribute that
   --  ensures : h (Key) = ((K * Key) mod Prime) mod N_Subtables.
   package Dynamic_Element_Array is new
     PolyORB.Utils.Dynamic_Tables (Element, Natural, 0, 10, 50);
   use Dynamic_Element_Array;

   package Dynamic_Subtable_Array is new
     PolyORB.Utils.Dynamic_Tables (Subtable, Natural, 0, 10, 50);
   use Dynamic_Subtable_Array;

   subtype Element_Array is Dynamic_Element_Array.Instance;

   subtype Subtable_Array is Dynamic_Subtable_Array.Instance;

   type Hash_Table is record
      Info      : Table_Info;
      Elements  : Element_Array;
      Subtables : Subtable_Array;
   end record;
   --  Info contained the variables of the table (see above for
   --  details).
   --  Elements is the array where all the elements are stored.
   --  Subtables is the array which contains all the informations
   --  specific to the sub_tables. His size is equal to Info.N_Sub_Tables.
   --  Each structure of the array contains the parameters for the sub-table
   --  hash function except the prime number stored in Info.Prime. In addition
   --  it contains the limit of each sub-table in the table Elements.
   --  We can note that :
   --          ..< Subtables.all (i).First   < Subtables.all (i).Last   <
   --              Subtables.all (i+1).First < Subtables.all (i+1).Last <..

   procedure Initialize
     (T      : out Hash_Table;
      Prime  : Natural;
      Max    : Natural);
   --  Initialize the hash table and allocate some internal
   --  structures. Prime is a prime number used by hash functions. Max
   --  is the max number of elements to store.

   procedure Finalize
     (T : in out Hash_Table);
   --  Deallocate all the internal structures.

   procedure Lookup
     (T         : Hash_Table;
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      Found     : out Boolean);
   --  Find key in hash table. Key is the string to hash. ST_Index
   --  corresponds to the subtable index and ST_Offset to the offset
   --  in this subtable. When Key does not exist, Found is set to False.
   --  If Key exists Found is set to True

   procedure Insert
     (T         : in out Hash_Table;
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      To_Do     : out What_To_Do);

   --  Insert key in hash table. In case of an already existing Key,
   --  Insert ignores insertion. Key is the string to hash.
   --  ST_Index corresponds to the subtable index and ST_Offset to
   --  the offset in this subtable of the inserted Key
   --  To_Do indicates if :
   --     -  a reorder of a sub-table or the table is
   --        necessary or not after the insertion (Reorder_SubTable or
   --        Reorder_Table)
   --     -  an item associated with the key can be inserted (Insert_Item)
   --     -  the key already exists (Nothing_To_Do)

   procedure Delete
     (T   : in out Hash_Table;
      Key : String);
   --  Delete key in hash table. In case of a non-existing Key, Delete
   --  ignores deletion. Key is the string to hash.
   --  When a Key is deleted, it's not physically. Indeed it puts just
   --  the tag Used to False



end PolyORB.Utils.HTables;
