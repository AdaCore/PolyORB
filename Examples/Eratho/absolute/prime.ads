with Common; use Common;

package Prime is

   pragma Remote_Types;

   type New_Pool_Type is new Pool_Type with null record;

   procedure Test_Primarity
     (Pool    : access New_Pool_Type;
      Number  : in     Natural);

end Prime;
