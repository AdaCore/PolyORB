with Common; use Common;
package Prime is

   type New_Pool_Type is new Prime_Pool_Type with null record;

   procedure Test_Number
     (Pool   : access New_Pool_Type;
      Number : in     Natural;
      Cell   : in out Natural;
      Prime  :    out Natural);

end Prime;
