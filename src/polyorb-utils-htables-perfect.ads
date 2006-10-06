------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . U T I L S . H T A B L E S . P E R F E C T         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides dynamic perfect hash tables; it implements
--  the Dietzfelbinger algorithm as described in "Dynamic Perfect
--  Hashing: Upper and Lower Bounds", Dietzfelbinger et al.  in SIAM
--  Journal on Computing, 1994, pp 738-761.

--  This algorithm provides dynamic perfect hash table with
--  - O (1) worst-case time for lookups and deletions,
--  - O (1) amortized expected time for insertions.

--  Note: A major hypothesis made by this algorithm is that the class
--  of hashing functions provided during instanciation is universal.

with PolyORB.Utils.Dynamic_Tables;
with PolyORB.Utils.Strings;

generic
   type Item is private;

   type Hash_Parameters is private;

   with function Default_Hash_Parameters return Hash_Parameters;

   with function Hash
     (Key   : String;
      Param : Hash_Parameters;
      Size  : Natural) return Natural;

   with function Next_Hash_Parameters
     (Param : Hash_Parameters) return Hash_Parameters;

package PolyORB.Utils.HTables.Perfect is

   pragma Preelaborate;

   type Item_Access is access all Item;

   type Table is private;
   type Table_Access is access all Table;

   type Table_Instance is record
      T : Table_Access;
   end record;

   Default_Max : constant := 10;

   procedure Initialize
     (T      : out Table_Instance;
      HParam :     Hash_Parameters := Default_Hash_Parameters;
      Max    :     Natural := Default_Max);
   --  Initialize the hash table.
   --  HParam are the hash function parameters, Max is the maximum number
   --  of elements to store.

   procedure Finalize (T : in out Table_Instance);
   --  Finalize the hash table

   function Lookup
     (T           : Table_Instance;
      Key         : String;
      Error_Value : Item) return Item;
   --  Find Key in hash table and return its associated Item.
   --  When Key does not exist, the function returns Error_Value.

   procedure Insert
     (T     : Table_Instance;
      Key   : String;
      Value : Item);
   --  Insert (Key, Value) in hash table.
   --  Key is the string to hash and Value its associated Item.
   --  If Key already exists, nothing is done.

   --  Note: this procedure may reorganize or extend, when necessary, the table
   --  or the sub_tables, leading to amortized O (1) complexity only.

   procedure Delete
     (T   : Table_Instance;
      Key : String);
   --  Delete key in hash table. Does nothing if Key is not present in T.
   --  This procedure only unsets the entry's Used flag; deallocation is
   --  actually performed only after the table or a sub-table is reorganized
   --  (procedure Insert).

   function Is_Empty (T : Table_Instance) return Boolean;
   --  True if, and only if, T has no element

   -----------------------------------------
   -- Iterator on Table_Instance elements --
   -----------------------------------------

   type Iterator is private;

   --  This Iterator type provides a way to traverse the hash tables
   --  and access the elements stored in the hash table.

   --  Note that elements are traversed in an implementation-defined arbitrary
   --  order.

   function First (T : Table_Instance) return Iterator;
   --  Return an Iterator placed on the first non null element found in T.
   --  If there is no such element, the Iterator is placed outside the bounds
   --  of T.

   function Value (I : Iterator) return Item;
   --  Return the Item on which I is placed

   function Key (I : Iterator) return String;
   --  Return the Key of the item on which I is placed

   function Last (I : Iterator) return Boolean;
   --  True if I is past the last element of the Table_Instance on which
   --  it operates.

   procedure Next (I : in out Iterator);
   --  Jump to the next non null element of the Table_Instance on which
   --  it operates.

private

   --  A hash table containts a non-generic index of type Hash_Table and an
   --  an array of (generic) items providing the stored values.

   --  As described in the Dietzfelbinger algorithm, the Hash Table is divided
   --  into several sub-tables, each of which contains indices for several
   --  items.

   --  Element type

   type Element is record
      Key        : Utils.Strings.String_Ptr; --  Key of the element to hash.
      Used       : Boolean;        --  Is the slot really used ?
      ST_Index   : Natural;        --  Index in the Sub Table.
      ST_Offset  : Natural;        --  Offset in the Sub Table.
      Item_Index : Natural;        --  Index of the element.
   end record;

   Empty : constant Element := Element'(null, False, 0, 0, 0);

   package Dynamic_Element_Array is new
     PolyORB.Utils.Dynamic_Tables (Element, Natural, 0, 10, 50);
   use Dynamic_Element_Array;

   subtype Element_Array is Dynamic_Element_Array.Instance;

   --  Subtable type

   type Subtable is record
      First  : Natural;         --  'First subtable index.
      Last   : Natural;         --  'Last subtable index.
      Count  : Natural;         --  Number of keys stored.
      High   : Natural;         --  Highest count before reorganization.
      Max    : Natural;         --  Subtable maximum size.
      HParam : Hash_Parameters; --  Hash parameters.
   end record;

   package Dynamic_Subtable_Array is new
     PolyORB.Utils.Dynamic_Tables (Subtable, Natural, 0, 10, 50);
   use Dynamic_Subtable_Array;

   subtype Subtable_Array is Dynamic_Subtable_Array.Instance;

   --  Table_Info type

   type Table_Info is record
      Count        : Natural;         --  Number of keys stored in Subtables.
      High         : Natural;         --  Highest Count before resizing.
      N_Subtables  : Natural;         --  Number of subtables.
      HParam       : Hash_Parameters; --  Hash parameters.
   end record;

   --  The Hash table index.

   type Hash_Table is record
      Info      : Table_Info;      --  Table information.
      Elements  : Element_Array;   --  Placeholder for elements.
      Subtables : Subtable_Array;  --  Sub tables information.
   end record;

   --  Per construction, we have:
   --          ..< Subtables.all (i).First   < Subtables.all (i).Last   <
   --              Subtables.all (i+1).First < Subtables.all (i+1).Last <..

   --  Item_Array

   package Dynamic_Item_Array is new
     PolyORB.Utils.Dynamic_Tables (Item_Access, Natural, 0, 10, 50);
   use Dynamic_Item_Array;

   subtype Item_Array is Dynamic_Item_Array.Instance;

   --  Table type

   type Table is record
      HTable : Hash_Table;
      --  Index associating key values to indices in Items

      Items  : Item_Array;
      --  Stored item values
   end record;

   --  Note: HTable.Elements.all and Items.all have the same length.

   type Iterator is record
      On_Table : Table_Instance;
      Position : Natural := 0;
   end record;

end PolyORB.Utils.HTables.Perfect;
