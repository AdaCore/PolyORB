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
   --  Key is ...

   procedure Insert
     (T     : Table;
      Key   : String;
      Value : Item);
   --  Key is ... and Value ...

   procedure Delete
     (T   : Table;
      Key : String);
   --  Key is ...

private

   --  type Item_Access is access Item;
   --  type Item_Array is array (Natural range <>) of Item_Access;
   --  Ne faut-il pas utiliser des pointeurs pour eviter les copies
   --  d'objets trop larges ou avoir des tableaux trop larges.

   type Item_Array is array (Natural range <>) of Subtable;
   type Item_Array_Ptr is access all Item_Array;

   type Table is record
      HTable : Hash_Table;
      Items  : Item_Array_Ptr;
   end record;

end PolyORB.Utils.HTables.Perfect;
