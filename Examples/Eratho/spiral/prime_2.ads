with Common; use Common;

package Prime_2 is
   pragma Remote_Call_Interface;

   procedure Test_Primarity
     (Number  : in  Natural;
      Divider : out Natural;
      Where   : out Partition_ID);

end Prime_2;
