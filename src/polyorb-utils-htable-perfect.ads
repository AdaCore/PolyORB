generic
   type Item is private;

package PolyORB.Utils.H_Table.Perfect is

   type Table is private;

   -- Function Initialize --
   -- Prime : Prime Number for hash functions
   -- Max_Elements : Max number of elements that can be stored
   function Initialize_Table (Prime : Natural;
                              Max_Elements : Natural)
                              return Table;

   -- Deallocate
   procedure Deallocate_Table (T : Table);

   -- Function Look_Up
   function Look_Up (Key : String; T : Table) return Item;

   -- Insert
   procedure Insert (Key : String;
                     Field : Item;
                     T : Table);

   -- Delete
   procedure Delete (Key : String; T : Table);


private
   type Items is array (Natural range <>) of Sub_Table;
   type Items_Ptr is access all Items;

   type Table is record
      H_T : H_Table;
      I : Items_Ptr;
   end record;

end PolyORB.Utils.H_Table.Perfect;
