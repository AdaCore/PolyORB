with Common; use Common;

package Prime is

   type New_Pool_Type is new Pool_Type with null record;

   procedure Test_Primarity
     (Pool    : access New_Pool_Type;
      Number  : in     Natural;
      Divider :    out Natural;
      Where   :    out Partition_ID);

end Prime;
