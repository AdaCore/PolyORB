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

--  $Id$

with PolyORB.Utils.Dynamic_Tables;

generic
   type Item is private;

package PolyORB.Utils.HTables.Perfect is
   pragma Preelaborate;

   No_Key : exception;

   type Item_Access is access all Item;

   type Table is private;

   type Table_Access is access all Table;

   type Table_Instance is record
      T : Table_Access;
   end record;

   Default_Prime : constant := 1777771;
   Default_Max   : constant := 10;

   procedure Initialize
     (T      : out Table_Instance;
      Prime  : Natural := Default_Prime;
      Max    : Natural := Default_Max);
   --  Initialize the hash table and allocate some internal
   --  Prime is a prime number used by hash functions. Max is the max
   --  number of elements to store.

   procedure Finalize
     (T : in out Table_Instance);
   --  Deallocate all the internal structures.

   function Lookup
     (T     : Table_Instance;
      Key   : String;
      Error_Value : Item)
      return Item;
   --  Find key in hash table and return the associated Item.
   --  Key is the string to hash.
   --  When Key does not exist, The function returns Error_Value


   function Lookup
     (T     : Table_Instance;
      Key   : String)
      return Item;
   --  Find key in hash table and return the associated Item.
   --  Key is the string to hash.
   --  When Key does not exist, the function raise No_Key exception

   procedure Insert
     (T     : Table_Instance;
      Key   : String;
      Value : Item);
   --  Insert (Key, Value) in hash table.
   --  Key is the string to hash and Value its corresponding value.
   --  If Key already exists, nothing is done
   --  This procedure uses the procedure Insert of polyorb.utils.htables.ads
   --  and it rorganizes if necessary the table or the sub_tables. In
   --  addition,it inserts Value in the table Items (see below)

   procedure Delete
     (T   : Table_Instance;
      Key : String);
   --  Delete key in hash table. In case of a non-existing Key, Delete
   --  ignores deletion. Key is the string to hash. This procedure only put
   --  the flag Used to False. Deallocations appears only after reorganisation
   --  of the table or a sub-table (procedure Insert)

private

   package Dynamic_Item_Array is new
     PolyORB.Utils.Dynamic_Tables (Item_Access, Natural, 0, 10, 50);
   use Dynamic_Item_Array;

   subtype Item_Array is Dynamic_Item_Array.Instance;

   type Table is record
      HTable : Hash_Table;
      Items  : Item_Array;
   end record;
   --  Table is the agregation of an Hash_Table (non-generic) and
   --  and an array (generic) which contains the Values associated
   --  with the Keys. We can note that HTable.Elements.all and Items.all
   --  have the same size. Indeed if a Key is stored in HTable.Elements(i)
   --  then his value is stored in Items(HTable.Elements(i).Item_Index)

end PolyORB.Utils.HTables.Perfect;
