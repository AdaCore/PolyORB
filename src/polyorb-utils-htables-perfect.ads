------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . U T I L S . H T A B L E S . P E R F E C T         --
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

--  This package provides fonctions to use the package
--  PolyORB.Utils.HTables with a generic type. Each Item is associated with
--  an element. When hashing a key, Lookup returns this Item.

generic
   type Item is private;
package PolyORB.Utils.HTables.Perfect is

   type Table is private;

   procedure Initialize
     (T      : out Table;
      Prime  : Natural;
      Max    : Natural);
   --  Initialize the hash table and allocate some internal
   --  Prime is a prime number used by hash functions. Max is the max
   --  number of elements to store.

   procedure Finalize
     (T : in out Table);
    --  Deallocate all the internal structures.

   function Lookup
     (T     : Table;
      Key   : String;
      Value : out Item;
      OK    : out Boolean);
   --  Find key in hash table.
   --  Key is the string to hash.
   --  Value is Item associated with Key
   --  When Key does not exist, OK is set to False.
   --  If Key exists Ok is set to True

   procedure Insert
     (T     : Table;
      Key   : String;
      Value : Item);
   --  Insert (Key, Value) in hash table.
   --  Key is the string to hash and Value its corresponding value.
   --  If Key already exists, nothing is done

   procedure Delete
     (T   : Table;
      Key : String);
   --  Delete key in hash table. In case of a non-existing Key, Delete
   --  ignores deletion. Key is the string to hash.

private

   type Item_Access is access all Item;
   type Item_Array is array (Natural range <>) of Item_Access;
   type Item_Array_Ptr is access all Item_Array;

   type Table is record
      HTable : Hash_Table;
      Items  : Item_Array_Ptr;
   end record;
   -- Table is the agregation of an Hash_Table (non-generic) and
   -- and an array (generic) which contains the Values associated
   -- with the Keys. We can note that HTable.Elements.all and Items.all
   -- have the same size. Indeed if a Key is stored in HTable.Elements.all(i)
   -- then his value is stored in Items.all(i)

end PolyORB.Utils.HTables.Perfect;
