generic
   type Item is private;
package PolyORB.Utils.HTables.Perfect is

   type Table is private;

   procedure Initialize
     (T      : out Table;
      Prime  : Natural;
      Length : Natural);
   --  Prime is a prime number used by hash functions. Length is the
   --  max number of elements that can be stored.

   procedure Destroy
     (T : in out Table);

   function Lookup
     (T   : Table;
      Key : String)
     return Item;
   --  Key is the string to be hashed

   procedure Insert
     (T     : Table;
      Key   : String;
      Value : Item);
   --  Key is the string to be hashed
   --  Value is the Item associated with Key

   procedure Delete
     (T   : Table;
      Key : String);
   --  Key is the string to be hashed

private

   type Item_Access is access all Item;
   type Item_Array is array (Natural range <>) of Item_Access;
   type Item_Array_Ptr is access all Item_Array;

   type Table is record
      HTable : Hash_Table;
      Items  : Item_Array_Ptr;
   end record;

end PolyORB.Utils.HTables.Perfect;
